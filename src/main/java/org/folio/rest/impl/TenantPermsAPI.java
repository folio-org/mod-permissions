package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.core.json.JsonArray;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.ws.rs.core.Response;
import org.folio.rest.impl.PermsAPI.InvalidPermissionsException;
import org.folio.rest.jaxrs.model.OkapiPermissionSet;
import org.folio.rest.jaxrs.model.Perm;
import org.folio.rest.jaxrs.model.Permission;


import org.folio.rest.jaxrs.resource.TenantpermissionsResource;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.tools.utils.ValidationHelper;
/**
 *
 * @author kurt
 */
public class TenantPermsAPI implements TenantpermissionsResource {
  private static final String OKAPI_TENANT_HEADER = "x-okapi-tenant";
  private static final String PERMISSION_NAME_FIELD = "'permissionName'";
  private static final String TABLE_NAME_PERMS = "permissions";

  private final Logger logger = LoggerFactory.getLogger(TenantPermsAPI.class);
  private boolean noisy = true;
  
  private void report(String noise) {
    if(!noisy) {
      return;
    }
    logger.info(noise);
  }

  //The RAML won't do right if we don't provide a GET endpoint...
  @Override
  public void getTenantpermissions(String entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }
  
  @Override
  public void postTenantpermissions(OkapiPermissionSet entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        if(entity.getPerms() == null) {
          asyncResultHandler.handle(Future.succeededFuture(PostTenantpermissionsResponse.withJsonCreated(entity)));
        } else {
          //should we duplicate the list first?
          savePermList(entity.getPerms(), vertxContext, tenantId).setHandler(savePermsRes -> {
            if(savePermsRes.failed()) {
              String err = savePermsRes.cause().getLocalizedMessage();
              logger.error(err, savePermsRes.cause());
              if(savePermsRes.cause() instanceof InvalidPermissionsException) {
                asyncResultHandler.handle(Future.succeededFuture(
                        PostTenantpermissionsResponse.withJsonUnprocessableEntity(
                        ValidationHelper.createValidationErrorMessage(
                        "permissions for module", entity.getModuleId(),err))));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                        PostTenantpermissionsResponse.withPlainInternalServerError(
                        "Internal Server Error: " + err)));
              }
            } else {
              asyncResultHandler.handle(Future.succeededFuture(PostTenantpermissionsResponse.withJsonCreated(entity)));
            }
          });
        }
      });
    } catch(Exception e) {
      logger.debug("Error adding permissions set: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(PostTenantpermissionsResponse.withPlainInternalServerError("Internal Server Error")));
    }
  }

  private Future<Void> savePermList(List<Perm> permList, Context vertxContext, String tenantId) {
    Future future = Future.future();
    if(permList.isEmpty()) {
      return Future.succeededFuture();
    }
    Perm perm = permList.get(0);
    List<Perm> permListCopy = new ArrayList<>(permList);
    permListCopy.remove(0); //pop
    Future<Boolean> checkSubsExistFuture;
    if(perm.getSubPermissions().isEmpty()) {
      checkSubsExistFuture = Future.succeededFuture(true);
    } else {
      checkSubsExistFuture = checkSubsExist(perm.getSubPermissions(), vertxContext, tenantId);
    }
    checkSubsExistFuture.setHandler(subsExist -> {
      if(subsExist.failed()) {
        future.fail(subsExist.cause());
      } else {
        if(!subsExist.result()) {
          if(permListCopy.isEmpty()) {
            future.fail(new InvalidPermissionsException(String.format(
                    "Unable to satisfy dependencies for permission '%s'",
                    perm.getPermissionName())));
          } else {
            permListCopy.add(perm); //Move it to the back
            future.complete();
          }
        } else {
          savePerm(perm, tenantId, vertxContext).setHandler(savePermRes -> {
            if(savePermRes.failed()) {
              future.fail(savePermRes.cause());
            } else {
              future.complete();
            }
          });
        }
      }
    });

    return future.compose( next -> { return savePermList(permListCopy, vertxContext, tenantId); });
  }
  
  private Future<Boolean> checkSubsExist(List<String> subPerms, Context vertxContext, String tenantId) {
    Future<Boolean> future = Future.future();
    List<Future> futureList = new ArrayList<>();
    for(String permName : subPerms) {
      Future<Boolean> permCheckFuture = checkPermExists(permName, vertxContext, tenantId);
      futureList.add(permCheckFuture);
    }
    CompositeFuture compositeFuture = CompositeFuture.all(futureList);
    compositeFuture.setHandler(compositeRes -> {
      if(compositeRes.failed()) {
        future.fail(compositeRes.cause());
      } else {
        boolean allExist = true;
        for(Future<Boolean> existsCheckFuture : futureList) {
          if(existsCheckFuture.result() == false) {
            allExist = false;
            break;
          }
        }
        future.complete(allExist);
      }
    });
    return future;
  }
  
  private Future<Boolean> checkPermExists(String permName, Context vertxContext, String tenantId) {
    Future<Boolean> future = Future.future();
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setValue(permName);
    report("Initiating PG Client get() (no transaction)(checkPermExists)");
    PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
            Permission.class, new Criterion(nameCrit), true, false, getReply -> {
      if(getReply.failed()) {
        String err = getReply.cause().getLocalizedMessage();
        logger.error(err, getReply.cause());
        future.fail(getReply.cause());
      } else {
        List<Permission> returnList = (List<Permission>)getReply.result().getResults();
          if(returnList.size() > 0) {
            future.complete(true); //Perm already exists, no need to re-add
          } else {
            future.complete(false);
          }
      }
    });    
    return future;
  }

  private Future<Void> savePerm(Perm perm, String tenantId, Context vertxContext) {
    Future future = Future.future();
    if(perm.getPermissionName() == null) {
      return Future.succeededFuture();
    }
    Permission permission = new Permission();
    permission.setMutable(false); //All permissions created via tenantPermissions API are immutable
    permission.setPermissionName(perm.getPermissionName());
    permission.setDisplayName(perm.getDisplayName());
    permission.setDescription(perm.getDescription());
    if(perm.getSubPermissions() != null && !perm.getSubPermissions().isEmpty()) {
      List<Object> subPerms = new ArrayList<>();
      for(String s : perm.getSubPermissions()) {
        subPerms.add(s);
      }
      permission.setSubPermissions(subPerms);
    }
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setValue(perm.getPermissionName());
    //If already exists, we don't have to do anything
    try {
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(),
              tenantId);
      report("Opening transaction(savePerm)");
      pgClient.startTx(connection -> {
        report("Initating PG Client get() (in transaction)(savePerm)");
        pgClient.get(connection, TABLE_NAME_PERMS, Permission.class,
                new Criterion(nameCrit), true, false, getReply -> {
          if(getReply.failed()) {
            //rollback the transaction
            report("Rolling transaction back(savePerm)");
            pgClient.rollbackTx(connection, rollback -> {
              logger.debug(String.format("Error querying permission: %s",
                      getReply.cause().getLocalizedMessage()));
              future.fail(getReply.cause());
            });
          } else {
            List<Permission> returnList = (List<Permission>)getReply.result()
                    .getResults();
            if(!returnList.isEmpty()) {
              //permission already exists, close the connection, complete future
              report("Closing transaction(savePerm)");
              pgClient.endTx(connection, done -> {
                future.complete();
              });
            } else {
              String newId = UUID.randomUUID().toString();
              permission.setId(newId);
              if(perm.getVisible() == null) {
                permission.setVisible(false);
              } else {
                permission.setVisible(perm.getVisible());
              }
              try {
                report("Initiating PG Client save() (in transaction)(savePerm)");
                pgClient.save(connection, TABLE_NAME_PERMS, permission,
                        postReply -> {
                  if(postReply.failed()) {
                    report("Rolling transaction back(savePerm)");
                    pgClient.rollbackTx(connection, rollback -> {
                      logger.debug(String.format("Error saving permission: %s",
                              postReply.cause().getLocalizedMessage()));
                      future.fail(postReply.cause());
                    });
                  } else {
                    PermsAPI.updateSubPermissions(connection, permission.getPermissionName(),
                            new JsonArray(), new JsonArray(permission.getSubPermissions()),
                            vertxContext, tenantId, logger).setHandler(updateSubsRes -> {
                      if(updateSubsRes.failed()) {
                        report("Rolling transaction back(savePerm)");
                        pgClient.rollbackTx(connection, rollback -> {
                          logger.debug(String.format("Error updating permission metadata: %s",
                                  updateSubsRes.cause().getLocalizedMessage()));
                          future.fail(updateSubsRes.cause());
                        });
                      } else {
                        report("Closing transaction(savePerm)");
                        pgClient.endTx(connection, done -> {
                          future.complete();
                        });
                      }
                    });
                  }
                });
              } catch(Exception e) {
                report("Rolling transaction back(savePerm)");
                pgClient.rollbackTx(connection, rollback -> {
                  logger.debug(String.format("Error: %s", e.getLocalizedMessage()));
                  future.fail(e);
                });
              }
            }
          }
        });
      });
      
    } catch(Exception e) {
      logger.debug("Error running on vertx for savePerm: " + e.getLocalizedMessage());
      future.fail(e);
    }
    return future;
  }
}
