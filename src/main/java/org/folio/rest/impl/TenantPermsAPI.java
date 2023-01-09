package org.folio.rest.impl;

import static org.folio.rest.impl.PermsAPI.checkPermissionExists;
import static org.folio.rest.impl.PermsAPI.getCQL;
import static org.folio.rest.impl.PermsAPI.updateUserPermissions;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.sqlclient.Tuple;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.validation.constraints.NotNull;
import javax.ws.rs.core.Response;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.okapi.common.ModuleId;
import org.folio.okapi.common.SemVer;
import org.folio.rest.jaxrs.model.OkapiPermission;
import org.folio.rest.jaxrs.model.OkapiPermissionSet;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.PermissionUser;
import org.folio.rest.jaxrs.resource.Tenantpermissions;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.interfaces.Results;
import org.folio.rest.tools.utils.TenantTool;

/**
 *
 * @author kurt
 */
public class TenantPermsAPI implements Tenantpermissions {

  private static final String PERMISSION_NAME_FIELD = "'permissionName'";
  private static final String MODULE_NAME_FIELD = "'moduleName'";
  public static final String DEPRECATED_PREFIX = "(deprecated) ";

  private static final String ADD_PERM_TO_SUB_PERMS = "update %s_mod_permissions.permissions "
      + "set jsonb = jsonb_set(jsonb, '{subPermissions}', (jsonb->'subPermissions')::jsonb || $1) "
      + "where jsonb->'subPermissions' ? $2 and not jsonb->'subPermissions' ? $3 ";

  private final Logger logger = LogManager.getLogger(TenantPermsAPI.class);

  @Override
  public void postTenantpermissions(OkapiPermissionSet entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    String tenantId = TenantTool.tenantId(okapiHeaders);
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    pgClient.withTrans(connection -> handlePermLists(entity, connection, vertxContext, tenantId))
        .onSuccess(v -> asyncResultHandler.handle(Future.succeededFuture(
            PostTenantpermissionsResponse.respond201WithApplicationJson(entity))))
        .onFailure(t -> {
          logger.error(t.getMessage(), t);
          asyncResultHandler.handle(Future.succeededFuture(
              PostTenantpermissionsResponse.respond400WithTextPlain(t.getMessage())));
        });
  }

  private Future<Permission> addMissingModuleContext(Permission perm, ModuleId moduleId, Conn connection) {
    perm.setModuleName(moduleId.getProduct());
    SemVer semver = moduleId.getSemVer();
    perm.setModuleVersion(semver != null ? semver.toString() : null);

    return connection.update(PermsAPI.TABLE_NAME_PERMS, perm, perm.getId()).map(result -> perm);
  }

  private Future<List<Permission>> getPermsForModule(ModuleId moduleId,
      @NotNull List<OkapiPermission> perms, @NotNull List<String> replaces, Conn connection) {

    return getPermsByModule(moduleId, connection)
        .compose(existing -> {
          if (!existing.isEmpty()) {
            return Future.succeededFuture(existing); // permission already has context
          }
          // this means either:
          // A. the first time enabling this module, or
          // B. the permissions exist but don't yet have the moduleName field
          List<Permission> ret = new ArrayList<>();
          List<Future<Void>> futures = new ArrayList<>(perms.size());
          perms.forEach(perm ->
              futures.add(getModulePermByName(perm.getPermissionName(), connection)
                  .compose(dbPerm -> {
                    if (dbPerm == null || Boolean.TRUE.equals(dbPerm.getDummy())) {
                      // permission does not already exist or is dummy
                      return Future.succeededFuture();
                    }
                    // we only allow to override for immutable permissions (those posted with tenantPermissions)
                    // and if one of the following it true:
                    // 1: it's deprecated
                    // 2: no module context (yet)
                    // 3: permissions belongs to a module that is replaced.
                    if (Boolean.FALSE.equals(dbPerm.getMutable()) &&
                        (Boolean.TRUE.equals(dbPerm.getDeprecated())
                            || dbPerm.getModuleName() == null
                            || replaces.contains(dbPerm.getModuleName()))) {
                      return addMissingModuleContext(dbPerm, moduleId, connection)
                          .onSuccess(ret::add)
                          .mapEmpty();
                    } else {
                      // Edge case of (A) where a permission with the same name already exists.
                      // We need to fail here as there isn't anything we can do.
                      String msg = String.format(
                          "Collision! A Permission named %s is already defined: %s",
                          perm.getPermissionName(), Json.encode(dbPerm));
                      logger.error(msg);
                      return Future.failedFuture(msg);
                    }
                  })));
          return GenericCompositeFuture.all(futures).map(ret);
        })
        .onFailure(t -> logger.error(t.getMessage(), t));
  }

  private List<OkapiPermission> getNewPerms(Map<String, Permission> dbPerms,
      @NotNull List<OkapiPermission> perms) {

    return perms.stream()
        .filter(perm -> !dbPerms.containsKey(perm.getPermissionName()))
        .filter(perm -> {
          // filter out renamed perms, we'll deal with them separately
          for (String oldName : perm.getReplaces()) {
            if (dbPerms.containsKey(oldName)) {
              return false;
            }
          }
          return true;
        })
        .collect(Collectors.toList());
  }

  private Map<OkapiPermission, List<Permission>> getRenamedPerms(Map<String, Permission> dbPerms,
      List<OkapiPermission> perms) {

    Map<OkapiPermission, List<Permission>> renamedPerms = new HashMap<>();
    perms.stream()
        .filter(perm -> !dbPerms.containsKey(perm.getPermissionName()))
        .forEach(perm -> {
          List<Permission> replaced = null;
          for (String oldName : perm.getReplaces()) {
            Permission dbPerm = dbPerms.get(oldName);
            if (dbPerm != null) {
              if (replaced == null) {
                replaced = new ArrayList<>();
              }
              replaced.add(dbPerm);
              renamedPerms.put(perm, replaced);
            }
          }
        });
    return renamedPerms;
  }

  private List<OkapiPermission> getModifiedPerms(ModuleId moduleId, Map<String, Permission> dbPerms,
      @NotNull List<OkapiPermission> okapiPerms) {

    return okapiPerms.stream()
        .filter(okapiPerm -> {
          String name = okapiPerm.getPermissionName();
          return dbPerms.containsKey(name)
              && (dbPerms.get(name).getDeprecated()
              || !PermissionUtils.equals(okapiPerm, moduleId.getProduct(), dbPerms.get(name)));
        })
        .collect(Collectors.toList());
  }

  private List<Permission> getRemovedPerms(Map<String, Permission> dbPerms,
      List<OkapiPermission> perms) {

    Set<String> permNames = perms.stream()
        .map(OkapiPermission::getPermissionName)
        .collect(Collectors.toSet());

    return dbPerms.values().stream()
        .filter(dbPerm -> !permNames.contains(dbPerm.getPermissionName()))
        .collect(Collectors.toList());
  }

  private Future<Void> handlePermLists(OkapiPermissionSet permSet, Conn connection, Context vertxContext,
      String tenantId) {
    ModuleId moduleId = new ModuleId(permSet.getModuleId());
    List<OkapiPermission> perms = new ArrayList<>();
    if (permSet.getPerms() != null) {
      permSet.getPerms().stream()
          .filter(p -> p != null && p.getPermissionName() != null)
          .forEach(perms::add);
    }

    return getPermsForModule(moduleId, perms, permSet.getReplaces(), connection)
        .compose(existing -> {
          Map<String, Permission> dbPerms = new HashMap<>(existing.size());
          existing.forEach(dbPerm -> dbPerms.put(dbPerm.getPermissionName(), dbPerm));

          return handleNewPerms(moduleId, dbPerms, perms, connection, vertxContext, tenantId)
              .compose(x -> handleModifiedPerms(moduleId, dbPerms, perms, connection, vertxContext, tenantId))
              .compose(v -> handleRenamedPerms(moduleId, dbPerms, perms, connection, vertxContext, tenantId))
              .compose(v -> handleRemovedPerms(getRemovedPerms(dbPerms, perms), connection))
              .compose(v -> migratePermsAssign(moduleId, dbPerms, connection, vertxContext, tenantId));
        });
  }

  private Future<Void> migratePermsAssignUser(PermissionUser permUser, Conn connection, Context vertxContext, String tenantId) {
    // it would be nice with a PermsCache that operated on connection
    return PermsCache.expandPerms(permUser.getPermissions(), vertxContext, tenantId)
        .compose(expandedPerms -> {
          JsonArray originalList = new JsonArray(permUser.getPermissions());
          JsonArray newList = new JsonArray(permUser.getPermissions());
          List<String> permissionsToBeAdded = Collections.emptyList();
          if (originalList.contains(PermissionUtils.PERMS_OKAPI_ALL)) {
            permissionsToBeAdded = List.of(PermissionUtils.PERMS_USERS_ASSIGN_OKAPI,
                PermissionUtils.PERMS_USERS_ASSIGN_IMMUTABLE,
                PermissionUtils.PERMS_USERS_ASSIGN_MUTABLE);
          }
          boolean added = false;
          for (String perm : permissionsToBeAdded) {
            if (!originalList.contains(perm)) {
              newList.add(perm);
              added = true;
            }
          }
          if (!added) {
            return Future.succeededFuture();
          }
          permUser.setPermissions(newList.getList());
          return connection.update(PermsAPI.TABLE_NAME_PERMSUSERS, permUser, permUser.getId())
              .compose(x -> updateUserPermissions(connection, permUser.getId(), originalList, newList,
                null, null, null));
        });
  }

  private Future<Void> migratePermsAssign(ModuleId moduleId, Map<String, Permission> dbPerms,
      Conn connection, Context vertxContext, String tenantId) {
    // only if are upgrading from an earlier mod-permissions module do we upgrade users.
    Permission permission = dbPerms.get(PermissionUtils.PERMS_USERS_ASSIGN_MUTABLE);
    if (!"mod-permissions".equals(moduleId.getProduct())
        || (permission != null && !permission.getDeprecated())) {
      return Future.succeededFuture();
    }
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setVal(PermissionUtils.PERMS_OKAPI_ALL);
    nameCrit.setOperation("=");
    return connection.get(PermsAPI.TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit), false)
        .compose(res -> {
          if (res.getResults().isEmpty()) {
            return Future.succeededFuture(); // okapi not enabled
          }
          Permission perm = res.getResults().get(0);
          List<Object> grantedTo = perm.getGrantedTo();
          List<Future<Void>> futures = new ArrayList<>(grantedTo.size());
          for (Object o : grantedTo) {
            futures.add(connection.getById(PermsAPI.TABLE_NAME_PERMSUSERS, (String) o, PermissionUser.class)
                .compose(permUser -> migratePermsAssignUser(permUser, connection, vertxContext, tenantId))
                .mapEmpty());
          }
          return GenericCompositeFuture.all(futures).mapEmpty();
        });
  }

  private Future<Void> handleNewPerms(ModuleId moduleId, Map<String,Permission> dbPerms, @NotNull List<OkapiPermission> permList,
      Conn connection, Context vertxContext, String tenantId) {

    List<OkapiPermission> newPerms = getNewPerms(dbPerms, permList);
    return savePermList(moduleId, newPerms, connection, vertxContext, tenantId);
  }

  private Future<Void> handleModifiedPerms(ModuleId moduleId, Map<String,Permission> dbPerms, @NotNull List<OkapiPermission> permList,
      Conn connection, Context vertxContext, String tenantId) {

    List<OkapiPermission> modifiedPerms = getModifiedPerms(moduleId, dbPerms, permList);
    return savePermList(moduleId, modifiedPerms, connection, vertxContext, tenantId);
  }

  private Future<Void> handleRenamedPerms(ModuleId moduleId,  Map<String,Permission> dbPerms, @NotNull List<OkapiPermission> permList,
      Conn connection, Context vertxContext, String tenantId) {

    Map<OkapiPermission, List<Permission>> renamedPerms = getRenamedPerms(dbPerms, permList);
    return renamePermList(connection, moduleId, renamedPerms, vertxContext, tenantId);
  }

  private Future<Void> handleRemovedPerms(List<Permission> permList, Conn connection) {
    if (permList.isEmpty()) {
      return Future.succeededFuture();
    }
    return softDeletePermList(permList, connection);
  }

  protected Future<Void> softDeletePermList(List<Permission> permList, Conn connection) {
    List<Permission> entities = new ArrayList<>();

    permList.stream()
      .filter(perm -> !perm.getDeprecated()) //skip perms which are already deprecated
      .forEach(perm -> {
        perm.setDeprecated(true);
        perm.setDisplayName(DEPRECATED_PREFIX + perm.getDisplayName());
        entities.add(perm);
      });
    return connection.upsertBatch(PermsAPI.TABLE_NAME_PERMS, entities).mapEmpty();
  }

  private Future<Void> updatedGrantedTo(Conn connection, String permissionName, String permUserId) {

    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permissionName);
    Criterion crit = new Criterion(nameCrit);

    return connection.get(PermsAPI.TABLE_NAME_PERMS, Permission.class, crit, false).compose(result -> {
      List<Permission> permList = result.getResults();
      if (permList.isEmpty()) {
        throw new RuntimeException("Permission with name " + permissionName + " does not exist");
      }
      // now we can actually add it
      Permission perm = permList.get(0);
      if (perm.getGrantedTo().contains(permUserId)) {
        throw new RuntimeException("Permission " + permissionName + " already granted to " + permUserId);
      }
      perm.getGrantedTo().add(permUserId);
      String query = String.format("permissionName==%s", permissionName);
      CQLWrapper cqlFilter = getCQL(query, PermsAPI.TABLE_NAME_PERMS);
      return connection.update(PermsAPI.TABLE_NAME_PERMS, perm, cqlFilter, false).mapEmpty();
    });
  }

  private Future<Void> renamePermList(Conn connection, ModuleId moduleId,
      @NotNull Map<OkapiPermission, List<Permission>> permList, Context vertxContext, String tenantId) {

    if (permList.isEmpty()) {
      return Future.succeededFuture();
    }
    return savePermList(moduleId, new ArrayList<>(permList.keySet()), connection, vertxContext, tenantId)
        .compose(v -> {
          List<Future<Void>> futures = new ArrayList<>(permList.size());
          permList.keySet().forEach(okapiPerm -> {
            String newPermName = okapiPerm.getPermissionName();
            permList.get(okapiPerm).forEach(replaced -> {
              // add new permission name to all relevant sub permissions
              String oldPermName = replaced.getPermissionName();
              futures.add(connection.execute(String.format(ADD_PERM_TO_SUB_PERMS, tenantId),
                  Tuple.of(new JsonArray().add(newPermName), oldPermName, newPermName)).mapEmpty());
              replaced.getGrantedTo().forEach(permUser -> {
                String permissionName = okapiPerm.getPermissionName();
                futures.add(addPermissionToUser(connection, permUser.toString(), permissionName));
                futures.add(updatedGrantedTo(connection, permissionName, permUser.toString()));
              });
            });
          });
          return GenericCompositeFuture.all(futures);
        })
        .compose(cf -> softDeletePermList(permList.values()
            .stream()
            .flatMap(List::stream)
            .collect(Collectors.toList()), connection));
  }

  private Future<Void> savePermList(ModuleId moduleId, @NotNull List<OkapiPermission> permList,
      Conn connection, Context vertxContext, String tenantId) {

    if (permList.isEmpty()) {
      return Future.succeededFuture();
    }
    List<OkapiPermission> permListCopy = new ArrayList<>(permList);
    return checkAnyPermsHaveAllSubs(permListCopy, connection)
        .compose(result -> {
          OkapiPermission perm = permListCopy.get(0);
          permListCopy.remove(0);
          if (Boolean.TRUE.equals(result)) {
            return findMissingSubs(perm.getSubPermissions(), connection)
                .compose(findMissingSubsRes -> {
                  if (findMissingSubsRes.isEmpty()) {
                    return savePerm(moduleId, perm, connection, vertxContext, tenantId);
                  }
                  permListCopy.add(perm); //Add to back
                  return Future.succeededFuture();
                });
          } else {
            //No valid perms, we need to create some dummies
            permListCopy.add(perm); //Push our initial perm back on the list
            return createDummies(permListCopy, connection)
                .compose(createDummiesRes ->
                  //see if we're able to actually create any perms now
                  checkAnyPermsHaveAllSubs(permListCopy, connection)
                      .map(check2res -> {
                        if (Boolean.TRUE.equals(check2res)) {
                          //We'll be able to save more perms in promise run-throughs
                          return null;
                        } else {
                          throw new RuntimeException(String.format("Unable to resolve permission dependencies for %s",
                              Json.encode(permListCopy)));
                        }
                      })
                );
          }
        }).compose(next ->
            savePermList(moduleId, permListCopy, connection, vertxContext, tenantId)
        );
  }

  private Future<Void> addPermissionToUser(Conn connection, String userId, String permissionName) {

    return PermsAPI.lookupPermsUsersById(userId, "id", connection).compose(user -> {
      if (user == null) {
        return Future.failedFuture("User with id " + userId + " does not exist");
      }
      String actualId = user.getId();
      if (user.getPermissions().contains(permissionName)) {
        return Future.failedFuture("User with id " + actualId + " already has permission " + permissionName);
      }
      user.getPermissions().add(permissionName);
      return connection.update(PermsAPI.TABLE_NAME_PERMSUSERS, user, actualId).mapEmpty();
    });
  }

  private Future<Boolean> checkAnyPermsHaveAllSubs(List<OkapiPermission> permList, Conn connection) {
    if (permList.isEmpty()) {
      return Future.succeededFuture(false); //If we made it this far, we must not have found any
    }
    List<OkapiPermission> permListCopy = new ArrayList<>(permList);
    OkapiPermission perm = permListCopy.get(0);
    permListCopy.remove(0);
    return findMissingSubs(perm.getSubPermissions(), connection)
        .compose(result -> {
          if (result.isEmpty()) {
            return Future.succeededFuture(true);
          }
          return checkAnyPermsHaveAllSubs(permListCopy, connection);
        });
  }

  /*
    Given a list of permission names, return a list of any that do not currently
    exist
   */
  private Future<List<String>> findMissingSubs(List<String> subPerms, Conn connection) {
    Map<String, Future<Boolean>> futureMap = new HashMap<>();
    List<String> notFoundList = new ArrayList<>();
    for (String permName : subPerms) {
      Future<Boolean> permCheckFuture = checkPermExists(permName, connection);
      futureMap.put(permName, permCheckFuture);
    }
    CompositeFuture compositeFuture = CompositeFuture.all(new ArrayList<>(futureMap.values()));
    return compositeFuture.compose(res -> {
      futureMap.forEach((permName, existsCheckFuture) -> {
        if (Boolean.FALSE.equals(existsCheckFuture.result())) {
          notFoundList.add(permName);
        }
      });
      return Future.succeededFuture(notFoundList);
    });
  }

  private Future<Boolean> checkPermExists(String permName, Conn connection) {
    return getModulePermByName(permName, connection)
        .map(Objects::nonNull);
  }

  private Future<Permission> getModulePermByName(String permName, Conn connection) {
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permName);
    Criterion crit = new Criterion(nameCrit);
    return connection.get(PermsAPI.TABLE_NAME_PERMS, Permission.class, crit.setLimit(new Limit(1)), false)
        .map(result -> {
          List<Permission> returnList = result.getResults();
          return returnList.isEmpty() ? null : returnList.get(0);
        });
  }

  private Future<List<Permission>> getPermsByModule(ModuleId moduleId, Conn connection) {
    String moduleName = moduleId.getProduct();
    Criteria modCrit = new Criteria();
    modCrit.addField(MODULE_NAME_FIELD);
    modCrit.setOperation("=");
    modCrit.setVal(moduleName);

    return connection.get(PermsAPI.TABLE_NAME_PERMS,
        Permission.class, new Criterion(modCrit), true).map(Results::getResults);
  }

  private Future<Void> savePerm(ModuleId moduleId, OkapiPermission perm, Conn connection, Context vertxContext, String tenantId) {
    if (perm.getPermissionName() == null) {
      return Future.succeededFuture();
    }
    Permission permission = new Permission();
    permission.setMutable(false); //All permissions created via tenantPermissions API are immutable
    permission.setPermissionName(perm.getPermissionName());
    permission.setDisplayName(perm.getDisplayName());
    permission.setDescription(perm.getDescription());
    permission.setModuleName(moduleId.getProduct());
    permission.setDeprecated(false);
    SemVer semver = moduleId.getSemVer();
    permission.setModuleVersion(semver != null ? semver.toString() : null);

    if (perm.getSubPermissions() != null && !perm.getSubPermissions().isEmpty()) {
      List<Object> subPerms = new ArrayList<>(perm.getSubPermissions());
      permission.setSubPermissions(subPerms);
    }
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(perm.getPermissionName());
    //If already exists, we don't have to do anything
    return connection.get(PermsAPI.TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit), true)
        .compose(result -> {
          List<Permission> returnList = result.getResults();
          Permission foundPerm = null;
          if (!returnList.isEmpty()) {
            foundPerm = returnList.get(0);
            if (!Boolean.FALSE.equals(foundPerm.getMutable())) {
              throw new RuntimeException("PermissionName collision with user-defined permission: " + perm.getPermissionName());
            }
            // leverage dummy permission to handle permission update
            if (Boolean.TRUE.equals(foundPerm.getDeprecated())
                || (perm.getSubPermissions() != null && !perm.getSubPermissions().equals(foundPerm.getSubPermissions()))
                || (perm.getVisible() != null && !perm.getVisible().equals(foundPerm.getVisible()))
                || (perm.getDisplayName() != null && !perm.getDisplayName().equals(foundPerm.getDisplayName()))
                || (perm.getDescription() != null && !perm.getDescription().equals(foundPerm.getDescription()))
                || (moduleId.getSemVer() != null && !moduleId.getSemVer().toString().equals(foundPerm.getModuleVersion()))) {
              foundPerm.setDummy(true);
            }
          }
          if (foundPerm != null && !foundPerm.getDummy()) {
            //If it isn't a dummy permission, we won't replace it
            return Future.succeededFuture();
          }
          List<Object> grantedTo = foundPerm == null ? null : foundPerm.getGrantedTo();
          Future<Void> deleteExistingFuture = foundPerm == null
              ? Future.succeededFuture()
              : deletePerm(connection, perm.getPermissionName());
          return deleteExistingFuture.compose(res -> {
            String newId = UUID.randomUUID().toString();
            permission.setId(newId);
            permission.setDummy(false);

            // preserve grantedTo if applicable.
            if (grantedTo != null && !grantedTo.isEmpty()) {
              permission.setGrantedTo(grantedTo);
            }
            if (perm.getVisible() == null) {
              permission.setVisible(false);
            } else {
              permission.setVisible(perm.getVisible());
            }
            return connection.save(PermsAPI.TABLE_NAME_PERMS, newId, permission)
                .compose(res1 ->
                    PermsAPI.updateSubPermissions(connection, permission.getPermissionName(),
                        new JsonArray(), new JsonArray(permission.getSubPermissions()),
                        null, vertxContext, tenantId));
          });
        });
  }

  /*
    Given a list of perms, look at them to determine which subpermissions among
    them cannot be satisfied by other perms in the list. Create dummy permissions
    for these permissions. Return as list of permissionNames
   */
  Future<List<String>> createDummies(List<OkapiPermission> permList, Conn connection) {
    //First determine which need dummies -- Assume all perms in list are currently
    //not satisfiable
    List<String> externalSubsNeeded = new ArrayList<>();
    for (OkapiPermission perm : permList) {
      for (String sub : perm.getSubPermissions()) {
        boolean externalNeeded = true;
        for (OkapiPermission perm2 : permList) {
          if (perm2.getPermissionName().equals(sub)) {
            externalNeeded = false;
            break;
          }
        }
        if (externalNeeded && !externalSubsNeeded.contains(sub)) {
          externalSubsNeeded.add(sub);
        }
      }
    }
    return makeDummyPermList(connection, externalSubsNeeded).map(x -> externalSubsNeeded);
  }

  private Future<Void> makeDummyPermList(Conn connection, List<String> permList) {
    Future<Void> future = Future.succeededFuture();
    for (String perm : permList) {
      future = future
          .compose(x -> checkPermissionExists(connection, perm))
          .compose(exists -> exists ? Future.succeededFuture() : makeDummyPerm(connection, perm));
    }
    return future;
  }

  private Future<Void> makeDummyPerm(Conn connection, String perm) {
    Permission dummyPermission = new Permission();
    dummyPermission.setPermissionName(perm);
    String newId = UUID.randomUUID().toString();
    dummyPermission.setId(newId);
    dummyPermission.setDummy(true);
    dummyPermission.setVisible(false);
    dummyPermission.setMutable(false);
    return connection.save(PermsAPI.TABLE_NAME_PERMS, newId, dummyPermission).mapEmpty();
  }

  private Future<Void> deletePerm(Conn connection, String permName) {
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permName);
    return connection.delete(PermsAPI.TABLE_NAME_PERMS, new Criterion(nameCrit)).mapEmpty();
  }
}
