package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.ws.rs.core.Response;
import static org.folio.rest.RestVerticle.MODULE_SPECIFIC_ARGS;
import org.folio.rest.impl.PermsAPI.InvalidPermissionsException;
import static org.folio.rest.impl.PermsAPI.checkPermissionExists;
import org.folio.rest.jaxrs.model.OkapiPermissionSet;
import org.folio.rest.jaxrs.model.Perm;
import org.folio.rest.jaxrs.model.Permission;

import org.folio.rest.jaxrs.resource.Tenantpermissions;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.SQLConnection;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.tools.utils.ValidationHelper;

/**
 *
 * @author kurt
 */
public class TenantPermsAPI implements Tenantpermissions {

  private static final String OKAPI_TENANT_HEADER = "x-okapi-tenant";
  private static final String PERMISSION_NAME_FIELD = "'permissionName'";
  private static final String TABLE_NAME_PERMS = "permissions";

  private final Logger logger = LoggerFactory.getLogger(TenantPermsAPI.class);
  private boolean noisy = Boolean.parseBoolean(MODULE_SPECIFIC_ARGS
    .getOrDefault("report.extra.logging", "false"));

  private void report(String noise) {
    if (!noisy) {
      return;
    }
    logger.info(noise);
  }

  @Override
  public void postTenantpermissions(OkapiPermissionSet entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        if (entity.getPerms() == null) {
          asyncResultHandler.handle(Future.succeededFuture(PostTenantpermissionsResponse.respond201WithApplicationJson(entity)));
        } else {
          //should we duplicate the list first?
          savePermList(entity.getPerms(), vertxContext, tenantId).onComplete(savePermsRes -> {
            if (savePermsRes.failed()) {
              String err = savePermsRes.cause().getLocalizedMessage();
              logger.error(err, savePermsRes.cause());
              if (savePermsRes.cause() instanceof InvalidPermissionsException) {
                asyncResultHandler.handle(Future.succeededFuture(
                  PostTenantpermissionsResponse.respond422WithApplicationJson(
                    ValidationHelper.createValidationErrorMessage(
                      "permissions for module", entity.getModuleId(), err))));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                  PostTenantpermissionsResponse.respond500WithTextPlain(
                    "Internal Server Error: " + err)));
              }
            } else {
              asyncResultHandler.handle(Future.succeededFuture(PostTenantpermissionsResponse.respond201WithApplicationJson(entity)));
            }
          });
        }
      });
    } catch (Exception e) {
      logger.error("Error adding permissions set: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(PostTenantpermissionsResponse.respond500WithTextPlain("Internal Server Error")));
    }
  }

  private Future<Void> savePermList(List<Perm> permList, Context vertxContext, String tenantId) {
    Future future = Future.future();
    if (permList.isEmpty()) {
      return Future.succeededFuture(); //Whee, we're done!
    }
    List<Perm> permListCopy = new ArrayList<>(permList);
    checkAnyPermsHaveAllSubs(permListCopy, vertxContext, tenantId)
      .onComplete(checkRes -> {
        if (checkRes.failed()) {
          future.fail(checkRes.cause());
        } else {
          Perm perm = permListCopy.get(0);
          permListCopy.remove(0);
          if (checkRes.result()) {
            report(String.format("Checking to see if we can save permission '%s'",
              perm.getPermissionName()));
            findMissingSubs(perm.getSubPermissions(), vertxContext, tenantId)
              .onComplete(findMissingSubsRes -> {
                if (findMissingSubsRes.failed()) {
                  future.fail(findMissingSubsRes.cause());
                } else {
                  if (findMissingSubsRes.result().isEmpty()) {
                    savePerm(perm, tenantId, vertxContext).onComplete(savePermRes -> {
                      if (savePermRes.failed()) {
                        future.fail(savePermRes.cause());
                      } else {
                        //Successfully added permission, we're done for this iteration
                        future.complete();
                      }
                    });
                  } else {
                    report(String.format("Can't save permission '%s' yet, pushing to back of queue",
                      perm.getPermissionName()));
                    permListCopy.add(perm); //Add to back
                    future.complete();
                  }
                }
              });
          } else {
            //No valid perms, we need to create some dummies
            permListCopy.add(perm); //Push our initial perm back on the list
            if (noisy) {
              report(String.format("Attempting to create dummies for perm list: %s",
                getPermListStringRep(permListCopy)));
            }
            createDummies(permListCopy, vertxContext, tenantId).onComplete(
              createDummiesRes -> {
                if (createDummiesRes.failed()) {
                  future.fail(createDummiesRes.cause());
                } else {
                  if (noisy) {
                    report(String.format("Created dummies: %s",
                      createDummiesRes.result()));
                  }
                  //see if we're able to actually create any perms now
                  checkAnyPermsHaveAllSubs(permListCopy, vertxContext, tenantId)
                    .onComplete(check2res -> {
                      if (check2res.failed()) {
                        future.fail(check2res.cause());
                      } else {
                        if (check2res.result()) {
                          //We'll be able to save more perms in future run-throughs
                          future.complete();
                        } else {
                          future.fail(String.format("Unable to resolve permission dependencies for %s",
                            getPermListStringRep(permListCopy)));
                        }
                      }
                    });
                }
              });
          }
        }
      });
    return future.compose(next ->
      savePermList(permListCopy, vertxContext, tenantId)
    );
  }

  private Future<Boolean> checkAnyPermsHaveAllSubs(List<Perm> permList, Context vertxContext,
    String tenantId) {
    if (noisy) {
      report(String.format("Checking list %s to see if any are have all subpermission satisfied",
        getPermListStringRep(permList)));
    }
    Future<Boolean> future = Future.future();
    if (permList.isEmpty()) {
      return Future.succeededFuture(false); //If we made it this far, we must not have found any
    }
    List<Perm> permListCopy = new ArrayList<>(permList);
    Perm perm = permListCopy.get(0);
    permListCopy.remove(0);
    findMissingSubs(perm.getSubPermissions(), vertxContext, tenantId).onComplete(
      fmsRes -> {
        if (fmsRes.failed()) {
          future.fail(fmsRes.cause());
        } else {
          if (fmsRes.result().isEmpty()) {
            report(String.format("Permission '%s' is able to be saved",
              perm.getPermissionName()));
            future.complete(true);
          } else {
            report(String.format("Permission '%s' is missing subs %s",
              perm.getPermissionName(), fmsRes.result()));
            future.complete(false);
          }
        }
      });
    return future.compose(next -> {
      if (next) {
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
    Future<List<String>> future = Future.future();
    Map<String, Future<Boolean>> futureMap = new HashMap<>();
    List<String> notFoundList = new ArrayList<>();
    for (String permName : subPerms) {
      Future<Boolean> permCheckFuture = checkPermExists(permName, vertxContext, tenantId);
      futureMap.put(permName, permCheckFuture);
    }
    CompositeFuture compositeFuture = CompositeFuture.all(new ArrayList<>(futureMap.values()));
    compositeFuture.onComplete(compositeRes -> {
      if (compositeRes.failed()) {
        future.fail(compositeRes.cause());
      } else {
        Iterator it = futureMap.entrySet().iterator();
        while (it.hasNext()) {
          Map.Entry pair = (Map.Entry) it.next();
          Future<Boolean> existsCheckFuture = (Future<Boolean>) pair.getValue();
          if (existsCheckFuture.result() == false) {
            notFoundList.add((String) pair.getKey());
          }
        }
        future.complete(notFoundList);
      }
    });
    return future;
  }

  private Future<Boolean> checkPermExists(String permName, Context vertxContext, String tenantId) {
    Future<Boolean> future = Future.future();
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permName);
    report("Initiating PG Client get() (no transaction)(checkPermExists)");
    PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
      Permission.class, new Criterion(nameCrit), true, false, getReply -> {
        if (getReply.failed()) {
          String err = getReply.cause().getLocalizedMessage();
          logger.error(err, getReply.cause());
          future.fail(getReply.cause());
        } else {
          List<Permission> returnList = getReply.result().getResults();
          if (returnList.size() > 0) {
            logger.info(String.format("Permission with name '%s' exists", permName));
            future.complete(true); //Perm already exists, no need to re-add
          } else {
            logger.info(String.format("Permission with name '%s' does not exist", permName));
            future.complete(false);
          }
        }
      });
    return future;
  }

  private Future<Void> savePerm(Perm perm, String tenantId, Context vertxContext) {
    Future future = Future.future();
    if (perm.getPermissionName() == null) {
      return Future.succeededFuture();
    }
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
    try {
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(),
        tenantId);
      report("Opening transaction(savePerm)");
      pgClient.startTx(connection -> {
        report("Initating PG Client get() (in transaction)(savePerm)");
        pgClient.get(connection, TABLE_NAME_PERMS, Permission.class,
          new Criterion(nameCrit), true, false, getReply -> {
            if (getReply.failed()) {
              //rollback the transaction
              report("Rolling transaction back(savePerm)");
              pgClient.rollbackTx(connection, rollback -> {
                logger.debug(String.format("Error querying permission: %s",
                  getReply.cause().getLocalizedMessage()));
                future.fail(getReply.cause());
              });
            } else {
              List<Permission> returnList = getReply.result().getResults();
              Permission foundPerm = null;
              Future<Void> deleteExistingFuture = Future.future();
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
                report("Closing transaction(savePerm)");
                pgClient.endTx(connection, done -> {
                  future.complete();
                });
              } else {
                if (foundPerm == null) {
                  deleteExistingFuture = Future.succeededFuture();
                } else {
                  deleteExistingFuture = deletePerm(connection, perm.getPermissionName(),
                    vertxContext, tenantId);
                }
                deleteExistingFuture.onComplete(deleteExistingRes -> {
                  if (deleteExistingRes.failed()) {
                    report("Rolling transaction back(savePerm)");
                    pgClient.rollbackTx(connection, rollback -> {
                      future.fail(deleteExistingRes.cause());
                    });
                  } else {
                    String newId = UUID.randomUUID().toString();
                    permission.setId(newId);
                    permission.setDummy(false);

                    if (perm.getVisible() == null) {
                      permission.setVisible(false);
                    } else {
                      permission.setVisible(perm.getVisible());
                    }
                    try {
                      report("Initiating PG Client save() (in transaction)(savePerm)");
                      pgClient.save(connection, TABLE_NAME_PERMS, newId, permission,
                        postReply -> {
                          if (postReply.failed()) {
                            report("Rolling transaction back(savePerm)");
                            pgClient.rollbackTx(connection, rollback -> {
                              logger.debug(String.format("Error saving permission: %s",
                                postReply.cause().getLocalizedMessage()));
                              future.fail(postReply.cause());
                            });
                          } else {
                            PermsAPI.updateSubPermissions(connection, permission.getPermissionName(),
                              new JsonArray(), new JsonArray(permission.getSubPermissions()),
                              vertxContext, tenantId, logger).onComplete(updateSubsRes -> {
                                if (updateSubsRes.failed()) {
                                  report("Rolling transaction back(savePerm)");
                                  pgClient.rollbackTx(connection, rollback -> {
                                    logger.debug(String.format("Error updating permission metadata: %s",
                                      updateSubsRes.cause().getLocalizedMessage()));
                                    future.fail(updateSubsRes.cause());
                                  });
                                } else {
                                  report("Closing transaction(savePerm)");
                                  pgClient.endTx(connection, done -> {
                                    logger.info(String.format("Saved perm %s",
                                      perm.getPermissionName()));
                                    future.complete();
                                  });
                                }
                              });
                          }
                        });
                    } catch (Exception e) {
                      report("Rolling transaction back(savePerm)");
                      pgClient.rollbackTx(connection, rollback -> {
                        logger.debug(String.format("Error: %s", e.getLocalizedMessage()));
                        future.fail(e);
                      });
                    }
                    //save this permission
                  }
                });
              }
            }
          });
      });

    } catch (Exception e) {
      logger.debug("Error running on vertx for savePerm: " + e.getLocalizedMessage());
      future.fail(e);
    }
    return future;
  }

  /*
    Given a list of perms, look at them to determine which subpermissions among
    them cannot be satisfied by other perms in the list. Create dummy permissions
    for these permissions. Return as list of permissionNames
   */
  Future<List<String>> createDummies(List<Perm> permList, Context vertxContext,
    String tenantId) {
    Future<List<String>> future = Future.future();
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
    report("Starting transaction (createDummies)");
    pgClient.startTx(connection -> {
      makeDummyPermList(connection, externalSubsNeeded, vertxContext, tenantId)
        .onComplete(makeDummyRes -> {
          if (makeDummyRes.failed()) {
            report("Rolling back transaction (createDummies)");
            pgClient.rollbackTx(connection, rollback -> {
              future.fail(makeDummyRes.cause());
            });
          } else {
            report("Closing transaction (createDummies)");
            pgClient.endTx(connection, done -> {
              future.complete(externalSubsNeeded);
            });
          }
        });
    });
    return future;
  }

  private Future<Void> makeDummyPermList(AsyncResult<SQLConnection> connection, List<String> permList,
    Context vertxContext, String tenantId) {
    Future<Void> future = Future.future();
    if (permList.isEmpty()) {
      return Future.succeededFuture();
    }
    List<String> permListCopy = new ArrayList<>(permList);
    String perm = permListCopy.get(0);
    permListCopy.remove(0);
    checkPermissionExists(connection, perm, vertxContext, tenantId).onComplete(
      existsRes -> {
        if (existsRes.failed()) {
          future.fail(existsRes.cause());
        } else {
          if (existsRes.result()) {
            //permission already exists
            future.complete();
          } else {
            makeDummyPerm(connection, perm, vertxContext, tenantId).onComplete(
              makeDummyRes -> {
                if (makeDummyRes.failed()) {
                  future.fail(makeDummyRes.cause());
                } else {
                  future.complete();
                }
              });
          }
        }
      });

    return future.compose(next -> {
      return makeDummyPermList(connection, permListCopy, vertxContext, tenantId);
    });
  }

  private Future<Void> makeDummyPerm(AsyncResult<SQLConnection> connection, String perm,
    Context vertxContext, String tenantId) {
    Future<Void> future = Future.future();
    Permission dummyPermission = new Permission();
    dummyPermission.setPermissionName(perm);
    String newId = UUID.randomUUID().toString();
    dummyPermission.setId(newId);
    dummyPermission.setDummy(true);
    dummyPermission.setVisible(false);
    dummyPermission.setMutable(false);
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(),
      tenantId);
    report(String.format(
      "Calling PostgresClient save() (in transaction) (makeDummyPerm) (%s)",
      dummyPermission.getPermissionName()));
    pgClient.save(connection, TABLE_NAME_PERMS, newId, dummyPermission, saveReply -> {
      if (saveReply.failed()) {
        future.fail(saveReply.cause());
      } else {
        report(String.format("Saved dummy perm '%s'",
          dummyPermission.getPermissionName()));
        future.complete();
      }
    });
    return future;
  }

  private Future<Void> deletePerm(AsyncResult<SQLConnection> connection, String permName,
                                  Context vertxContext, String tenantId) {
    Future<Void> future = Future.future();
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permName);
    PostgresClient pgClient = PostgresClient.getInstance(
      vertxContext.owner(), tenantId);
    report(String.format("Calling postgres delete() (%s) (in transaction) deletePerm",
      permName));
    pgClient.delete(connection, TABLE_NAME_PERMS, new Criterion(nameCrit),
      deleteReply -> {
        if (deleteReply.failed()) {
          future.fail(deleteReply.cause());
        } else {
          future.complete();
        }
      });
    return future;
  }

  static String getPermListStringRep(List<Perm> permList) {
    if (permList.isEmpty()) {
      return "";
    }
    JsonArray jsonPermList = new JsonArray();
    for (Perm perm : permList) {
      JsonArray subList = new JsonArray();
      for (String sub : perm.getSubPermissions()) {
        subList.add(sub);
      }
      jsonPermList.add(new JsonObject()
        .put("permissionName", perm.getPermissionName())
        .put("subPermissions", subList)
      );
    }
    return jsonPermList.encode();
  }

}
