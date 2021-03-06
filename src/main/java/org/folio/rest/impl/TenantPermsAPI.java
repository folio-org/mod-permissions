package org.folio.rest.impl;

import static org.folio.rest.impl.PermsAPI.checkPermissionExists;
import static org.folio.rest.impl.PermsAPI.getCQL;
import static org.folio.rest.impl.PermsAPI.getIdCriterion;
import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.ws.rs.core.Response;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.ModuleId;
import org.folio.okapi.common.SemVer;
import org.folio.rest.jaxrs.model.OkapiPermission;
import org.folio.rest.jaxrs.model.OkapiPermissionSet;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.PermissionUser;
import org.folio.rest.jaxrs.resource.Tenantpermissions;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.SQLConnection;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.utils.TenantTool;

/**
 *
 * @author kurt
 */
public class TenantPermsAPI implements Tenantpermissions {

  private static final String PERMISSION_NAME_FIELD = "'permissionName'";
  private static final String MODULE_NAME_FIELD = "'moduleName'";
  private static final String TABLE_NAME_PERMS = "permissions";
  private static final String TABLE_NAME_PERMSUSERS = "permissions_users";
  public static final String DEPRECATED_PREFIX = "(deprecated) ";

  private static final String ADD_PERM_TO_SUB_PERMS = "update %s_mod_permissions.permissions "
      + "set jsonb = jsonb_set(jsonb, '{subPermissions}', (jsonb->'subPermissions')::jsonb || $1) "
      + "where jsonb->'subPermissions' ? $2 and not jsonb->'subPermissions' ? $3 ";

  private final Logger logger = LogManager.getLogger(TenantPermsAPI.class);

  @Override
  public void postTenantpermissions(OkapiPermissionSet entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      handlePermLists(entity, vertxContext, tenantId)
          .onSuccess(v -> asyncResultHandler.handle(Future.succeededFuture(
              PostTenantpermissionsResponse.respond201WithApplicationJson(entity))))
          .onFailure(t -> {
            logger.error(t.getMessage(), t);
            asyncResultHandler.handle(Future.succeededFuture(
                PostTenantpermissionsResponse.respond400WithTextPlain(t.getMessage())));
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          PostTenantpermissionsResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  private Future<Permission> addMissingModuleContext(Permission perm, ModuleId moduleId,
      Context vertxContext, String tenantId) {
    Promise<Permission> promise = Promise.promise();
perm.setModuleName(moduleId.getProduct());
    SemVer semver = moduleId.getSemVer();
    perm.setModuleVersion(semver != null ? semver.toString() : null);

    PostgresClient.getInstance(vertxContext.owner(), tenantId).update(TABLE_NAME_PERMS, perm,
        perm.getId(), updateReply -> {
          if (updateReply.failed()) {
            Throwable cause = updateReply.cause();
            logger.error(cause.getLocalizedMessage(), cause);
            promise.fail(cause);
            return;
          }
          logger.info(String.format("Added defined context to %s", perm.getPermissionName()));
          promise.complete(perm);
        });
    return promise.future();
  }

  private Future<List<Permission>> getPermsForModule(ModuleId moduleId, List<OkapiPermission> perms,
      Context vertxContext, String tenantId) {
    return getPermsByModule(moduleId, vertxContext, tenantId)
        .compose(existing -> {
          if (!existing.isEmpty()) {
            return Future.succeededFuture(existing); // permission already has context
          }
          // this means either:
          // A) the first time enabling this module, or
          // B) the permissions exist but don't yet have the moduleName field
          List<Permission> ret = new ArrayList<>();
          List<Future> futures = new ArrayList<>(perms.size());
          perms.forEach(perm ->
              futures.add(getModulePermByName(perm.getPermissionName(), vertxContext, tenantId)
                  .compose(dbPerm -> {
                    if (dbPerm == null || Boolean.TRUE.equals(dbPerm.getDummy())) {
                      // permission does not already exist or is dummy
                      return Future.succeededFuture();
                    }
                    // we only allow overwrite for immutable permissions (those posted with tenantPermissions)
                    // and if one of the following it true:
                    // 1: it's deprecated
                    // 2: no module context (yet)
                    if (Boolean.FALSE.equals(dbPerm.getMutable()) &&
                        (Boolean.TRUE.equals(dbPerm.getDeprecated())
                            || dbPerm.getModuleName() == null)) {
                      return addMissingModuleContext(dbPerm, moduleId, vertxContext, tenantId)
                          .onSuccess(ret::add);
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
          return CompositeFuture.all(futures).map(ret);
        })
        .onFailure(t -> logger.error(t.getMessage(), t));
  }

  private List<OkapiPermission> getNewPerms(Map<String, Permission> dbPerms,
      List<OkapiPermission> perms) {
    List<OkapiPermission> newPerms = new ArrayList<>();

    if(perms != null) {
      perms.stream()
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
        .forEach(newPerms::add);
    }

    return newPerms;
  }

  private Map<OkapiPermission, List<Permission>> getRenamedPerms(Map<String, Permission> dbPerms,
      List<OkapiPermission> perms) {
    Map<OkapiPermission, List<Permission>> renamedPerms = new HashMap<>();
    if(perms != null) {
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
    }
    return renamedPerms;
  }

  private List<OkapiPermission> getNewAndModifiedPerms(ModuleId moduleId,
      Map<String, Permission> dbPerms, List<OkapiPermission> okapiPerms) {
    List<OkapiPermission> newPerms = getNewPerms(dbPerms, okapiPerms);
    List<OkapiPermission> modifiedPerms = getModifiedPerms(moduleId, dbPerms, okapiPerms);
    newPerms.addAll(modifiedPerms);
    return newPerms;
  }

  private List<OkapiPermission> getModifiedPerms(ModuleId moduleId, Map<String, Permission> dbPerms,
      List<OkapiPermission> okapiPerms) {
    List<OkapiPermission> modifiedPerms = new ArrayList<>();

    if(okapiPerms != null) {
      okapiPerms.stream()
        .filter(okapiPerm -> {
            String name = okapiPerm.getPermissionName();
            return dbPerms.containsKey(name)
                && (dbPerms.get(name).getDeprecated()
                || !PermissionUtils.equals(okapiPerm, moduleId.getProduct(), dbPerms.get(name)));
          })
        .forEach(modifiedPerms::add);
    }
    return modifiedPerms;
  }

  private List<Permission> getRemovedPerms(Map<String, Permission> dbPerms,
      List<OkapiPermission> perms) {
    List<Permission> removedPerms = new ArrayList<>();
    List<String> permNames = new ArrayList<>();

    perms.stream().forEach(perm -> permNames.add(perm.getPermissionName()));

    dbPerms.values().stream()
      .filter(dbPerm -> !permNames.contains(dbPerm.getPermissionName()))
      .forEach(removedPerms::add);

    return removedPerms;
  }

  private Future<Void> handlePermLists(OkapiPermissionSet permSet, Context vertxContext,
      String tenantId) {
    ModuleId moduleId = new ModuleId(permSet.getModuleId());
    List<OkapiPermission> perms = new ArrayList<>();
    if (permSet.getPerms() != null) {
      permSet.getPerms().stream()
          .filter(p -> p != null && p.getPermissionName() != null)
          .forEach(perms::add);
    }

    return getPermsForModule(moduleId, perms, vertxContext, tenantId)
        .compose(existing -> {
          Map<String, Permission> dbPerms = new HashMap<>(existing.size());
          existing.forEach(dbPerm -> dbPerms.put(dbPerm.getPermissionName(), dbPerm));

          return handleNewAndModifiedPerms(moduleId,
              getNewAndModifiedPerms(moduleId, dbPerms, perms), vertxContext, tenantId)
                  .compose(v -> handleRenamedPerms(moduleId, getRenamedPerms(dbPerms, perms),
                      vertxContext, tenantId))
                  .compose(v -> handleRemovedPerms(getRemovedPerms(dbPerms, perms), vertxContext,
                      tenantId));
        });
  }

  private Future<Void> handleNewAndModifiedPerms(ModuleId moduleId, List<OkapiPermission> permList,
      Context vertxContext, String tenantId) {
    if (permList.isEmpty()) {
      return Future.succeededFuture();
    }

    Promise<Void> promise = Promise.promise();
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    pgClient.startTx(connection -> savePermList(moduleId, permList, vertxContext, tenantId)
        .onSuccess(v -> pgClient.endTx(connection, done -> promise.complete()))
        .onFailure(t -> pgClient.rollbackTx(connection, rollback -> {
          logger.error(String.format("Error saving permissions: %s", t.getLocalizedMessage()));
          promise.fail(t);
        })));
    return promise.future();
  }

  private Future<Void> handleRenamedPerms(ModuleId moduleId,
      Map<OkapiPermission, List<Permission>> permList, Context vertxContext, String tenantId) {
    if (permList.isEmpty()) {
      return Future.succeededFuture();
    }

    Promise<Void> promise = Promise.promise();
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    pgClient.startTx(
        connection -> renamePermList(connection, moduleId, permList, vertxContext, tenantId)
            .onSuccess(v -> pgClient.endTx(connection, done -> {
              logger.info(String.format("Renamed %s permissions", permList.size()));
              promise.complete();
            }))
            .onFailure(t -> pgClient.rollbackTx(connection, rollback -> {
              logger.info(String.format("Error renaming permissions: %s", t.getLocalizedMessage()));
              promise.fail(t);
            })));
    return promise.future();
  }

  private Future<Void> handleRemovedPerms(List<Permission> permList, Context vertxContext,
      String tenantId) {
    if (permList.isEmpty()) {
      return Future.succeededFuture();
    }

    Promise<Void> promise = Promise.promise();
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    pgClient.startTx(connection -> softDeletePermList(connection, permList, vertxContext, tenantId)
        .onSuccess(v -> pgClient.endTx(connection, done -> {
          logger.info(String.format("Soft deleted %s permissions", permList.size()));
          promise.complete();
        }))
        .onFailure(t -> pgClient.rollbackTx(connection, rollback -> {
          logger
              .info(String.format("Error soft deleting permissions: %s", t.getLocalizedMessage()));
          promise.fail(t);
        })));
    return promise.future();
  }

  protected Future<Void> softDeletePermList(AsyncResult<SQLConnection> connection,
      List<Permission> permList, Context vertxContext, String tenantId) {
    Promise<Void> promise = Promise.promise();

    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    List<Permission> entities = new ArrayList<>();

    permList.stream()
      .filter(perm -> !perm.getDeprecated()) //skip perms which are already deprecated
      .forEach(perm -> {
        perm.setDeprecated(true);
        perm.setDisplayName(DEPRECATED_PREFIX + perm.getDisplayName());
        entities.add(perm);
      });

    pgClient.upsertBatch(connection, TABLE_NAME_PERMS, entities, updateReply -> {
      if (updateReply.failed()) {
        logger.info(String.format("Error soft deleting permissions: %s",
            updateReply.cause().getLocalizedMessage()));
        promise.fail(updateReply.cause());
        return;
      }
      logger.info(String.format("Successfully soft deleted %s permissions", entities.size()));
      promise.complete();
    });

    return promise.future();
  }

  private Future<Void> updatedGrantedTo(AsyncResult<SQLConnection> connection,
      String permissionName, String permUserId, Context vertxContext, String tenantId) {
    Promise<Void> promise = Promise.promise();

    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permissionName);
    Criterion crit = new Criterion(nameCrit);

    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    pgClient.get(TABLE_NAME_PERMS, Permission.class, crit, true, false, getReply -> {
      if (getReply.failed()) {
        String errStr =
            "Error getting permission " + permissionName + ": " + getReply.cause().getMessage();
        logger.error(errStr, getReply.cause());
        promise.fail(getReply.cause());
        return;
      }
      List<Permission> permList = getReply.result().getResults();
      if (permList.isEmpty()) {
        promise.fail("Permission with name " + permissionName + " does not exist");
        return;
      }
      // now we can actually add it
      Permission perm = permList.get(0);
      if (perm.getGrantedTo().contains(permUserId)) {
        promise.fail("Permission " + permissionName + " already granted to " + permUserId);
        return;
      }
      try {
        if (perm.getGrantedTo().contains(permUserId)) {
          logger
              .debug("Permission '" + permissionName + "' already grantedTo '" + permUserId + "'");
          promise.complete();
          return;
        }

        perm.getGrantedTo().add(permUserId);
        String query = String.format("permissionName==%s", permissionName);
        CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMS);
        pgClient.update(connection, TABLE_NAME_PERMS, perm, cqlFilter, true, updateReply -> {
          if (updateReply.failed()) {
            promise.fail(updateReply.cause());
            logger.error(updateReply.cause().getMessage(), updateReply.cause());
            return;
          }
          promise.complete();
        });
      } catch (Exception e) {
        logger.error(e.getMessage(), e);
        promise.fail(e);
      }
    });

    return promise.future();
  }

  private Future<Void> renamePermList(AsyncResult<SQLConnection> connection, ModuleId moduleId,
      Map<OkapiPermission, List<Permission>> permList, Context vertxContext, String tenantId) {
    return savePermList(moduleId, new ArrayList<>(permList.keySet()), vertxContext, tenantId)
        .compose(v -> {
          List<Future> futures = new ArrayList<>(permList.size());
          PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
          permList.keySet().forEach(okapiPerm -> {
            String newPermName = okapiPerm.getPermissionName();
            permList.get(okapiPerm).forEach(replaced -> {
              // add new permission name to all relevant sub permissions
              String oldPermName = replaced.getPermissionName();
                futures.add(Future.<RowSet<Row>>future(p -> pgClient.execute(connection, 
                    String.format(ADD_PERM_TO_SUB_PERMS, tenantId),
                    Tuple.of(new JsonArray().add(newPermName), oldPermName, newPermName), p)));

              replaced.getGrantedTo().forEach(permUser -> {
                String permissionName = okapiPerm.getPermissionName();
                futures.add(addPermissionToUser(connection, permUser.toString(), permissionName,
                    vertxContext, tenantId));
                futures.add(updatedGrantedTo(connection, permissionName, permUser.toString(),
                    vertxContext, tenantId));
              });
            });
          });
          return CompositeFuture.all(futures);
        })
        .compose(cf -> softDeletePermList(connection,
            permList.values()
                .stream()
                .flatMap(List::stream)
                .collect(Collectors.toList()),
            vertxContext, tenantId));
  }

  private Future<Void> savePermList(ModuleId moduleId, List<OkapiPermission> permList,
      Context vertxContext, String tenantId) {
    Promise<Void> promise = Promise.promise();
    if (permList == null || permList.isEmpty()) {
      return Future.succeededFuture();
    }
    List<OkapiPermission> permListCopy = new ArrayList<>(permList);
    checkAnyPermsHaveAllSubs(permListCopy, vertxContext, tenantId)
        .onComplete(checkRes -> {
          if (checkRes.failed()) {
            promise.fail(checkRes.cause());
            return;
          }
          OkapiPermission perm = permListCopy.get(0);
          permListCopy.remove(0);
          if (Boolean.TRUE.equals(checkRes.result())) {
            findMissingSubs(perm.getSubPermissions(), vertxContext, tenantId)
                .onComplete(findMissingSubsRes -> {
                  if (findMissingSubsRes.failed()) {
                    promise.fail(findMissingSubsRes.cause());
                    return;
                  }
                  if (findMissingSubsRes.result().isEmpty()) {
                    savePerm(moduleId, perm, tenantId, vertxContext).onComplete(savePermRes -> {
                      if (savePermRes.failed()) {
                        promise.fail(savePermRes.cause());
                      } else {
                        //Successfully added permission, we're done for this iteration
                        promise.complete();
                      }
                    });
                  } else {
                    permListCopy.add(perm); //Add to back
                    promise.complete();
                  }
                });
          } else {
            //No valid perms, we need to create some dummies
            permListCopy.add(perm); //Push our initial perm back on the list
            createDummies(permListCopy, moduleId, vertxContext, tenantId).onComplete(
                createDummiesRes -> {
                  if (createDummiesRes.failed()) {
                    promise.fail(createDummiesRes.cause());
                    return;
                  }
                  //see if we're able to actually create any perms now
                  checkAnyPermsHaveAllSubs(permListCopy, vertxContext, tenantId)
                      .onComplete(check2res -> {
                        if (check2res.failed()) {
                          promise.fail(check2res.cause());
                          return;
                        }
                        if (Boolean.TRUE.equals(check2res.result())) {
                          //We'll be able to save more perms in promise run-throughs
                          promise.complete();
                        } else {
                          promise.fail(String.format("Unable to resolve permission dependencies for %s",
                              Json.encode(permListCopy)));
                        }
                      });
                });
          }
        });
    return promise.future().compose(next ->
        savePermList(moduleId, permListCopy, vertxContext, tenantId)
    );
  }

  private Future<Void> addPermissionToUser(AsyncResult<SQLConnection> connection, String userId,
      String permissionName, Context vertxContext, String tenantId) {
    Promise<Void> promise = Promise.promise();

    Criterion useridCrit = getIdCriterion(userId);
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    pgClient.get(TABLE_NAME_PERMSUSERS, PermissionUser.class, useridCrit, true, false, getReply -> {
      if (getReply.failed()) {
        String errStr = "Error checking for user: " + getReply.cause().getMessage();
        logger.error(errStr, getReply.cause());
        promise.fail(getReply.cause());
        return;
      }
      List<PermissionUser> userList = getReply.result().getResults();
      if (userList.isEmpty()) {
        promise.fail("User with id " + userId + " does not exist");
        return;
      }
      // now we can actually add it
      PermissionUser user = userList.get(0);
      String actualId = user.getId();
      if (user.getPermissions().contains(permissionName)) {
        promise.fail("User with id " + actualId + " already has permission " + permissionName);
        return;
      }
      try {
        user.getPermissions().add(permissionName);

        String query = String.format("id==%s", actualId);
        CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMSUSERS);
        pgClient.update(connection, TABLE_NAME_PERMSUSERS, user, cqlFilter, true, putReply -> {
          if (putReply.failed()) {
            promise.fail(putReply.cause());
            logger.error(putReply.cause().getMessage(), putReply.cause());
            return;
          }
          promise.complete();
        });
      } catch (Exception e) {
        logger.error(e.getMessage(), e);
        promise.fail(e);
      }
    });

    return promise.future();
  }

  private Future<Boolean> checkAnyPermsHaveAllSubs(List<OkapiPermission> permList, Context vertxContext,
                                                   String tenantId) {

    Promise<Boolean> promise = Promise.promise();
    if (permList.isEmpty()) {
      return Future.succeededFuture(false); //If we made it this far, we must not have found any
    }
    List<OkapiPermission> permListCopy = new ArrayList<>(permList);
    OkapiPermission perm = permListCopy.get(0);
    permListCopy.remove(0);
    findMissingSubs(perm.getSubPermissions(), vertxContext, tenantId).onComplete(
        fmsRes -> {
          if (fmsRes.failed()) {
            promise.fail(fmsRes.cause());
            return;
          }
          promise.complete(fmsRes.result().isEmpty());
        });
    return promise.future().compose(next -> {
      if (Boolean.TRUE.equals(next)) {
        return Future.succeededFuture(next);
      } else {
        return checkAnyPermsHaveAllSubs(permListCopy, vertxContext, tenantId);
      }
    });
  }

  /*
    Given a list of permission names, return a list of any that do not currently
    exist
   */
  private Future<List<String>> findMissingSubs(List<String> subPerms, Context vertxContext,
      String tenantId) {
    Promise<List<String>> promise = Promise.promise();
    Map<String, Future<Boolean>> futureMap = new HashMap<>();
    List<String> notFoundList = new ArrayList<>();
    for (String permName : subPerms) {
      Future<Boolean> permCheckFuture = checkPermExists(permName, vertxContext, tenantId);
      futureMap.put(permName, permCheckFuture);
    }
    CompositeFuture compositeFuture = CompositeFuture.all(new ArrayList<>(futureMap.values()));
    compositeFuture.onComplete(compositeRes -> {
      if (compositeRes.failed()) {
        promise.fail(compositeRes.cause());
        return;
      }
      futureMap.forEach((permName, existsCheckFuture) -> {
        if (Boolean.FALSE.equals(existsCheckFuture.result())) {
          notFoundList.add(permName);
        }
      });
      promise.complete(notFoundList);
    });
    return promise.future();
  }

  private Future<Boolean> checkPermExists(String permName, Context vertxContext, String tenantId) {
    Promise<Boolean> promise = Promise.promise();
    getModulePermByName(permName, vertxContext, tenantId)
      .onFailure(t -> {
        logger.error(t.getLocalizedMessage(), t);
        promise.fail(t);
      })
      .onSuccess(returnPerm -> {
        if (returnPerm != null) {
          logger.info(String.format("Permission with name '%s' exists", permName));
          promise.complete(true); // OkapiPermission already exists, no need to re-add
        } else {
          logger.info(String.format("Permission with name '%s' does not exist", permName));
          promise.complete(false);
        }
      });
    return promise.future();
  }

  private Future<Permission> getModulePermByName(String permName, Context vertxContext, String tenantId) {
    Promise<Permission> promise = Promise.promise();
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permName);
    Criterion crit = new Criterion(nameCrit);

    PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
        Permission.class, crit.setLimit(new Limit(1)), true, false, getReply -> {
          if (getReply.failed()) {
            String err = getReply.cause().getLocalizedMessage();
            logger.error(err, getReply.cause());
            promise.fail(getReply.cause());
            return;
          }
          List<Permission> returnList = getReply.result().getResults();
          if (!returnList.isEmpty()) {
            promise.complete(returnList.get(0));
          } else {
            promise.complete(null);
          }
        });
    return promise.future();
  }

  private Future<List<Permission>> getPermsByModule(ModuleId moduleId, Context vertxContext,
      String tenantId) {
    Promise<List<Permission>> promise = Promise.promise();

    try {
      String moduleName = moduleId.getProduct();
      Criteria modCrit = new Criteria();
      modCrit.addField(MODULE_NAME_FIELD);
      modCrit.setOperation("=");
      modCrit.setVal(moduleName);

      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
          Permission.class, new Criterion(modCrit), true, false, getReply -> {
            if (getReply.failed()) {
              Throwable cause = getReply.cause();
              logger.error(cause.getLocalizedMessage(), cause);
              promise.fail(cause);
              return;
            }
            promise.complete(getReply.result().getResults());
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<Void> savePerm(ModuleId moduleId, OkapiPermission perm, String tenantId,
      Context vertxContext) {
    Promise<Void> promise = Promise.promise();
    if (perm.getPermissionName() == null) {
      return Future.succeededFuture();
    }
    try {
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
        List<Object> subPerms = new ArrayList<>();
        for (String s : perm.getSubPermissions()) {
          subPerms.add(s);
        }
        permission.setSubPermissions(subPerms);
      }
      Criteria nameCrit = new Criteria();
      nameCrit.addField(PERMISSION_NAME_FIELD);
      nameCrit.setOperation("=");
      nameCrit.setVal(perm.getPermissionName());
      //If already exists, we don't have to do anything
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(),
          tenantId);
      pgClient.startTx(connection -> pgClient.get(connection, TABLE_NAME_PERMS, Permission.class,
          new Criterion(nameCrit), true, false, getReply -> {
            if (getReply.failed()) {
              //rollback the transaction
              pgClient.rollbackTx(connection, rollback -> {
                logger.debug(String.format("Error querying permission: %s",
                    getReply.cause().getLocalizedMessage()));
                promise.fail(getReply.cause());
              });
              return;
            }
            List<Permission> returnList = getReply.result().getResults();
            Permission foundPerm = null;
            if (!returnList.isEmpty()) {
              foundPerm = returnList.get(0);
              if (!Boolean.FALSE.equals(foundPerm.getMutable())) {
                pgClient.rollbackTx(connection, rollback ->
                  promise.fail("PermissionName collision with user-defined permission: " + perm.getPermissionName()));
                return;
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
              pgClient.endTx(connection, done -> promise.complete());
            } else {
              List<Object> grantedTo = foundPerm == null ? null : foundPerm.getGrantedTo();
              Future<Void> deleteExistingFuture;
              if (foundPerm == null) {
                deleteExistingFuture = Future.succeededFuture();
              } else {
                deleteExistingFuture = deletePerm(connection, perm.getPermissionName(),
                    vertxContext, tenantId);
              }
              deleteExistingFuture.onComplete(deleteExistingRes -> {
                if (deleteExistingRes.failed()) {
                  pgClient.rollbackTx(connection, rollback -> {
                    promise.fail(deleteExistingRes.cause());
                  });
                  return;
                }
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
                try {
                  pgClient.save(connection, TABLE_NAME_PERMS, newId, permission,
                      postReply -> {
                        if (postReply.failed()) {
                          pgClient.rollbackTx(connection, rollback -> {
                            logger.debug(String.format("Error saving permission: %s",
                                postReply.cause().getLocalizedMessage()));
                            promise.fail(postReply.cause());
                          });
                          return;
                        }
                        PermsAPI.updateSubPermissions(connection, permission.getPermissionName(),
                            new JsonArray(), new JsonArray(permission.getSubPermissions()),
                            vertxContext, tenantId, logger).onComplete(updateSubsRes -> {
                          if (updateSubsRes.failed()) {
                            pgClient.rollbackTx(connection, rollback -> {
                              logger.debug(String.format("Error updating permission metadata: %s",
                                  updateSubsRes.cause().getLocalizedMessage()));
                              promise.fail(updateSubsRes.cause());
                            });
                            return;
                          }
                          pgClient.endTx(connection, done -> {
                            logger.info(String.format("Saved perm %s",
                                perm.getPermissionName()));
                            promise.complete();
                          });

                        });
                      });
                } catch (Exception e) {
                  pgClient.rollbackTx(connection, rollback -> {
                    logger.debug(String.format("Error: %s", e.getLocalizedMessage()));
                    promise.fail(e);
                  });
                }
              });
            }
          })
      );
    } catch (Exception e) {
      logger.debug("Error running on vertx for savePerm: " + e.getLocalizedMessage());
      promise.fail(e);
    }
    return promise.future();
  }

  /*
    Given a list of perms, look at them to determine which subpermissions among
    them cannot be satisfied by other perms in the list. Create dummy permissions
    for these permissions. Return as list of permissionNames
   */
  Future<List<String>> createDummies(List<OkapiPermission> permList, ModuleId moduleId,
      Context vertxContext, String tenantId) {

    Promise<List<String>> promise = Promise.promise();
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
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(),
        tenantId);
    pgClient.startTx(connection -> {
      makeDummyPermList(connection, externalSubsNeeded, moduleId, vertxContext, tenantId)
          .onComplete(makeDummyRes -> {
            if (makeDummyRes.failed()) {
              pgClient.rollbackTx(connection, rollback -> {
                promise.fail(makeDummyRes.cause());
              });
              return;
            }
            pgClient.endTx(connection, done -> {
              promise.complete(externalSubsNeeded);
            });

          });
    });
    return promise.future();
  }

  private Future<Void> makeDummyPermList(AsyncResult<SQLConnection> connection,
      List<String> permList, ModuleId moduleId, Context vertxContext, String tenantId) {
    Promise<Void> promise = Promise.promise();
    if (permList.isEmpty()) {
      return Future.succeededFuture();
    }
    List<String> permListCopy = new ArrayList<>(permList);
    String perm = permListCopy.get(0);
    permListCopy.remove(0);
    checkPermissionExists(connection, perm, vertxContext, tenantId).onComplete(
      existsRes -> {
        if (existsRes.failed()) {
          promise.fail(existsRes.cause());
        } else {
          if (existsRes.result()) {
            //permission already exists
            promise.complete();
          } else {
            makeDummyPerm(connection, perm, vertxContext, tenantId).onComplete(
                makeDummyRes -> promise.handle(makeDummyRes.mapEmpty()));
          }
        }
      });
    return promise.future().compose(
        next -> makeDummyPermList(connection, permListCopy, moduleId, vertxContext, tenantId));
  }

  private Future<Void> makeDummyPerm(AsyncResult<SQLConnection> connection, String perm,
      Context vertxContext, String tenantId) {

    Promise<Void> promise = Promise.promise();
    Permission dummyPermission = new Permission();
    dummyPermission.setPermissionName(perm);
    String newId = UUID.randomUUID().toString();
    dummyPermission.setId(newId);
    dummyPermission.setDummy(true);
    dummyPermission.setVisible(false);
    dummyPermission.setMutable(false);

    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    pgClient.save(connection, TABLE_NAME_PERMS, newId, dummyPermission,
        saveReply -> promise.handle(saveReply.mapEmpty()));
    return promise.future();
  }

  private Future<Void> deletePerm(AsyncResult<SQLConnection> connection, String permName,
                                  Context vertxContext, String tenantId) {
    Promise<Void> promise = Promise.promise();
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permName);
    PostgresClient pgClient = PostgresClient.getInstance(
      vertxContext.owner(), tenantId);
    pgClient.delete(connection, TABLE_NAME_PERMS, new Criterion(nameCrit),
      deleteReply -> promise.handle(deleteReply.mapEmpty()));
    return promise.future();
  }

}
