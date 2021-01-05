package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.json.Json;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.core.json.JsonArray;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.ws.rs.core.Response;
import org.folio.okapi.common.ModuleId;
import org.folio.rest.jaxrs.model.OkapiPermission;
import org.folio.rest.jaxrs.model.OkapiPermissionSet;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.resource.Tenantpermissions;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.SQLConnection;
import org.folio.rest.tools.utils.TenantTool;
import org.hamcrest.core.IsNull;
import org.mockito.internal.matchers.NotNull;
import static org.folio.rest.impl.PermsAPI.checkPermissionExists;

/**
 *
 * @author kurt
 */
public class TenantPermsAPI implements Tenantpermissions {

  private static final String PERMISSION_NAME_FIELD = "'permissionName'";
  private static final String DEFINED_BY_FIELD = "'definedBy'";
  private static final String MODULE_NAME_FIELD = "'moduleName'";
  private static final String TABLE_NAME_PERMS = "permissions";

  private final Logger logger = LoggerFactory.getLogger(TenantPermsAPI.class);

  @Override
  public void postTenantpermissions(OkapiPermissionSet entity, Map<String, String> okapiHeaders,
                                    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      handlePermLists(entity, vertxContext, tenantId).onComplete(ar -> {
        if (ar.failed()) {
          String err = ar.cause().getMessage();
          logger.error(err, ar.cause());
          asyncResultHandler.handle(Future.succeededFuture(
              PostTenantpermissionsResponse.respond400WithTextPlain(err)));
          return;
        }
        asyncResultHandler.handle(Future.succeededFuture(
            PostTenantpermissionsResponse.respond201WithApplicationJson(entity)));
      });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          PostTenantpermissionsResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  /* compare two objects for equality, w/ checks for null, etc. */
  private static boolean equals(Object a, Object b) {
    if ((a == null && b != null) || (a != null && b == null)) {
      return false;
    }
    return (a == null && b == null) || (a.equals(b));
  }

  /* compare an OkapiPermission to a Permission, including definedBy */
  private static boolean comparePerms(OkapiPermission okapiPerm, String moduleName, Permission perm) {
    return comparePerms(okapiPerm, perm)
        && perm.getDefinedBy() != null 
        && equals(moduleName, perm.getDefinedBy().getModuleName());
  }
  
  /* compare an OkapiPermission to a Permission */
  private static boolean comparePerms(OkapiPermission okapiPerm, Permission perm) {
    if ((okapiPerm == null && perm != null) || (okapiPerm != null && perm == null)) {
      return false;
    }
    return okapiPerm != null 
        && okapiPerm.getPermissionName().equals(perm.getPermissionName())
        && equals(okapiPerm.getDescription(), perm.getDescription())
        && equals(okapiPerm.getDisplayName(), perm.getDisplayName())
        && equals(okapiPerm.getSubPermissions(), perm.getSubPermissions())
        && equals(okapiPerm.getVisible(), perm.getVisible());
  }

  private Future<Void> handlePermLists(OkapiPermissionSet permSet, Context vertxContext,
      String tenantId) {
    Promise<Void> promise = Promise.promise();

    try {
      getPermsForModule(permSet, vertxContext, tenantId).compose(existing -> {
        Map<String, Permission> dbPerms = new HashMap<>(existing.size());
        existing.forEach(dbPerm -> dbPerms.put(dbPerm.getPermissionName(), dbPerm));

        return CompositeFuture.all(
            savePermList(getNewPerms(dbPerms, permSet), vertxContext, tenantId),
            updatePermList(getModifiedPerms(dbPerms, permSet), vertxContext, tenantId),
            deletePermList(getRemovedPerms(dbPerms, permSet), vertxContext, tenantId));
      }).onComplete(ar -> {
        if (ar.failed()) {
          promise.fail(ar.cause());
          return;
        }
        promise.complete();
      });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      promise.fail(e);
    }

    return promise.future();
  }

  private Future<List<Permission>> getPermsForModule(OkapiPermissionSet permSet,
      Context vertxContext, String tenantId) {
    Promise<List<Permission>> promise = Promise.promise();
    
    try {
      getPermsByModule(permSet.getModuleId(), vertxContext, tenantId)
        .onComplete(getResult -> {
          if (getResult.failed()) {
            Throwable cause = getResult.cause();
            logger.error(cause.getMessage(), cause);
            promise.fail(cause);
            return;
          }

          List<Permission> existing = getResult.result();
          if (existing.isEmpty() && permSet.getPerms() != null) {
            // this means either:
            // A) the first time enabling this module, or
            // B) the permissions exist but don't yet have the definedBy field
            List<OkapiPermission> perms = permSet.getPerms().stream()
                .filter(p -> p.getPermissionName() != null)
                .collect(Collectors.toList());
            List<Permission> ret = new ArrayList<>();

            List<Future> futures = new ArrayList<>(perms.size());
            perms.forEach(perm -> {
              futures.add(getModulePermByName(perm.getPermissionName(), null, vertxContext, tenantId)
                .compose(dbPerm -> {
                  if (dbPerm != null && comparePerms(perm, dbPerm)) {
                    // (B) we have a match, but lack definedBy
                    ret.add(dbPerm);
                  }
                  return Future.succeededFuture();
                }));
            });
            CompositeFuture.all(futures).onComplete(ar -> {
              if(ar.failed()) {
                promise.fail(ar.cause());
                return;
              }
              promise.complete(ret);
            });
          } else {
            promise.complete(existing); // Happy path.
          }
        });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      promise.fail(e);
    }
    
    return promise.future();
  }
  
  private List<OkapiPermission> getNewPerms(Map<String, Permission> dbPerms,
      OkapiPermissionSet permSet) {
    List<OkapiPermission> newPerms = new ArrayList<>();
    List<OkapiPermission> perms = permSet.getPerms();
    
    if(perms != null) {
      perms.stream()
        .filter(perm -> perm.getPermissionName() != null)
        .filter(perm -> !dbPerms.containsKey(perm.getPermissionName()))
        .forEach(newPerms::add);
    }

    return newPerms;
  }
  
  private List<OkapiPermission> getModifiedPerms(Map<String, Permission> dbPerms,
      OkapiPermissionSet permSet) {
    List<OkapiPermission> modifiedPerms = new ArrayList<>();    
    List<OkapiPermission> perms = permSet.getPerms();
    
    if(perms != null) {
      perms.stream()
        .filter(perm -> perm.getPermissionName() != null)
        .filter(perm -> {
          String name = perm.getPermissionName();
          for (String oldName : perm.getRenamedFrom()) {
            if(dbPerms.containsKey(oldName)) {
              return true;
            }
          }
          return dbPerms.containsKey(name)
              && !comparePerms(perm, permSet.getModuleId(), dbPerms.get(name));
        }).forEach(modifiedPerms::add);
    }

    return modifiedPerms;
  }

  private List<Permission> getRemovedPerms(Map<String, Permission> dbPerms,
      OkapiPermissionSet permSet) {
    List<Permission> removedPerms = new ArrayList<>();
    List<String> permNames = new ArrayList<>();
    List<OkapiPermission> perms = permSet.getPerms();

    if (perms != null) {
      perms.stream()
        .filter(perm -> perm.getPermissionName() != null)
        .forEach(perm -> permNames.add(perm.getPermissionName()));
    }

    dbPerms.values().stream()
      .filter(dbPerm -> !permNames.contains(dbPerm.getPermissionName()))
      .forEach(removedPerms::add);

    return removedPerms;
  }

  private Future<Void> updatePermList(List<OkapiPermission> permList, Context vertxContext,
      String tenantId) {
    Promise<Void> promise = Promise.promise();
    promise.complete();
    return promise.future();
  }

  private Future<Void> deletePermList(List<Permission> permList, Context vertxContext,
      String tenantId) {
    Promise<Void> promise = Promise.promise();
    promise.complete();
    return promise.future();
  }

  private Future<Void> savePermList(List<OkapiPermission> permList, Context vertxContext,
      String tenantId) {
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
                    savePerm(perm, tenantId, vertxContext).onComplete(savePermRes -> {
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
            createDummies(permListCopy, vertxContext, tenantId).onComplete(
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
        savePermList(permListCopy, vertxContext, tenantId)
    );
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
  private Future<List<String>> findMissingSubs(List<String> subPerms, Context vertxContext, String tenantId) {
    Promise<List<String>> promise = Promise.promise();
    Map<String, Future<Boolean>> futureMap = new HashMap<>();
    List<String> notFoundList = new ArrayList<>();
    for (String permName : subPerms) {
      Future<Boolean> permCheckFuture = checkPermExists(permName, null, vertxContext, tenantId);
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

  private Future<Boolean> checkPermExists(String permName, String moduleId, Context vertxContext, String tenantId) {
    Promise<Boolean> promise = Promise.promise();
    getModulePermByName(permName, moduleId, vertxContext, tenantId)
      .onComplete(getReply -> {
          if (getReply.failed()) {
            String err = getReply.cause().getLocalizedMessage();
            logger.error(err, getReply.cause());
            promise.fail(getReply.cause());
            return;
          }
          Permission returnPerm = getReply.result();
          if (returnPerm != null) {
            logger.info(String.format("Permission with name '%s' exists for module '%s'", permName,
                moduleId == null ? "unspecified" : moduleId));
            promise.complete(true); //OkapiPermission already exists, no need to re-add
          } else {
            logger.info(String.format("Permission with name '%s' does not exist for module '%s'", permName, 
                moduleId == null ? "unspecified" : moduleId));
            promise.complete(false);
          }
        });
    return promise.future();
  }

  private Future<Permission> getModulePermByName(String permName, String moduleId, Context vertxContext, String tenantId) {
    Promise<Permission> promise = Promise.promise();
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permName);
    Criterion crit = new Criterion(nameCrit);

    if (moduleId != null) {
      String moduleName = new ModuleId(moduleId).getProduct();      
      Criteria modCrit = new Criteria();
      modCrit.addField(DEFINED_BY_FIELD);
      modCrit.addField(MODULE_NAME_FIELD);
      modCrit.setOperation("=");
      modCrit.setVal(moduleName);
      crit.addCriterion(modCrit);
    }  

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

  private Future<List<Permission>> getPermsByModule(String moduleId, Context vertxContext, String tenantId) {
    Promise<List<Permission>> promise = Promise.promise();

    try {
      String moduleName = new ModuleId(moduleId).getProduct();
      Criteria modCrit = new Criteria();
      modCrit.addField(DEFINED_BY_FIELD);
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

  private Future<Void> savePerm(OkapiPermission perm, String tenantId, Context vertxContext) {
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
              // leverage dummy permission to handle permission update
              if ((perm.getSubPermissions() != null && !perm.getSubPermissions().equals(foundPerm.getSubPermissions()))
                  || (perm.getVisible() != null && !perm.getVisible().equals(foundPerm.getVisible()))
                  || (perm.getDisplayName() != null && !perm.getDisplayName().equals(foundPerm.getDisplayName()))
                  || (perm.getDescription() != null && !perm.getDescription().equals(foundPerm.getDescription()))) {
                foundPerm.setDummy(true);
              }
            }
            if (foundPerm != null && !foundPerm.getDummy()) {
              //If it isn't a dummy permission, we won't replace it
              pgClient.endTx(connection, done -> promise.complete());
            } else {
              Future<Void> deleteExistingFuture = null;
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
  Future<List<String>> createDummies(List<OkapiPermission> permList, Context vertxContext,
                                     String tenantId) {

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
      makeDummyPermList(connection, externalSubsNeeded, vertxContext, tenantId)
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

  private Future<Void> makeDummyPermList(AsyncResult<SQLConnection> connection, List<String> permList,
    Context vertxContext, String tenantId) {
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
    return promise.future().compose(next -> makeDummyPermList(connection, permListCopy, vertxContext, tenantId));
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
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(),
        tenantId);
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
