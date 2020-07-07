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
import javax.ws.rs.core.Response;
import org.folio.rest.jaxrs.model.OkapiPermissionSet;
import org.folio.rest.jaxrs.model.Perm;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.resource.Tenantpermissions;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.SQLConnection;
import org.folio.rest.tools.utils.TenantTool;

import static org.folio.rest.impl.PermsAPI.checkPermissionExists;

/**
 *
 * @author kurt
 */
public class TenantPermsAPI implements Tenantpermissions {

  private static final String PERMISSION_NAME_FIELD = "'permissionName'";
  private static final String TABLE_NAME_PERMS = "permissions";

  private final Logger logger = LoggerFactory.getLogger(TenantPermsAPI.class);

  @Override
  public void postTenantpermissions(OkapiPermissionSet entity, Map<String, String> okapiHeaders,
                                    Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      savePermList(entity.getPerms(), vertxContext, tenantId).onComplete(savePermsRes -> {
        if (savePermsRes.failed()) {
          String err = savePermsRes.cause().getMessage();
          logger.error(err, savePermsRes.cause());
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

  private Future<Void> savePermList(List<Perm> permList, Context vertxContext, String tenantId) {
    Promise<Void> promise = Promise.promise();
    if (permList == null || permList.isEmpty()) {
      return Future.succeededFuture(); //Whee, we're done!
    }
    List<Perm> permListCopy = new ArrayList<>(permList);
    checkAnyPermsHaveAllSubs(permListCopy, vertxContext, tenantId)
        .onComplete(checkRes -> {
          if (checkRes.failed()) {
            promise.fail(checkRes.cause());
            return;
          }
          Perm perm = permListCopy.get(0);
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

  private Future<Boolean> checkAnyPermsHaveAllSubs(List<Perm> permList, Context vertxContext,
                                                   String tenantId) {

    Promise<Boolean> promise = Promise.promise();
    if (permList.isEmpty()) {
      return Future.succeededFuture(false); //If we made it this far, we must not have found any
    }
    List<Perm> permListCopy = new ArrayList<>(permList);
    Perm perm = permListCopy.get(0);
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
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permName);
    PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
        Permission.class, new Criterion(nameCrit), true, false, getReply -> {
          if (getReply.failed()) {
            String err = getReply.cause().getLocalizedMessage();
            logger.error(err, getReply.cause());
            promise.fail(getReply.cause());
            return;
          }
          List<Permission> returnList = getReply.result().getResults();
          if (!returnList.isEmpty()) {
            logger.info(String.format("Permission with name '%s' exists", permName));
            promise.complete(true); //Perm already exists, no need to re-add
          } else {
            logger.info(String.format("Permission with name '%s' does not exist", permName));
            promise.complete(false);
          }
        });
    return promise.future();
  }

  private Future<Void> savePerm(Perm perm, String tenantId, Context vertxContext) {
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
  Future<List<String>> createDummies(List<Perm> permList, Context vertxContext,
                                     String tenantId) {

    Promise<List<String>> promise = Promise.promise();
    //First determine which need dummies -- Assume all perms in list are currently
    //not satisfiable
    List<String> externalSubsNeeded = new ArrayList<>();
    for (Perm perm : permList) {
      for (String sub : perm.getSubPermissions()) {
        boolean externalNeeded = true;
        for (Perm perm2 : permList) {
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
