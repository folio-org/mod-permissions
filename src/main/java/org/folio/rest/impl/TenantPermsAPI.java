package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.ws.rs.core.Response;
import org.folio.rest.jaxrs.model.OkapiPermissionSet;
import org.folio.rest.jaxrs.model.Perm;
import org.folio.rest.jaxrs.model.Permission;


import org.folio.rest.jaxrs.resource.TenantpermissionsResource;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.TenantTool;
/**
 *
 * @author kurt
 */
public class TenantPermsAPI implements TenantpermissionsResource {
  private static final String OKAPI_TENANT_HEADER = "x-okapi-tenant";
  private static final String PERMISSION_NAME_FIELD = "'permissionName'";
  private static final String TABLE_NAME_PERMS = "permissions";

  private final Logger logger = LoggerFactory.getLogger(TenantPermsAPI.class);

  //The RAML won't do right if we don't provide a GET endpoint...
  @Override
  public void getTenantpermissions(String entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }


  @Override
  public void postTenantpermissions(OkapiPermissionSet entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
      String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
      List<Future> futureList = new ArrayList<>();
      if(entity.getPerms() == null) {
        asyncResultHandler.handle(Future.succeededFuture(PostTenantpermissionsResponse.withJsonCreated(entity)));
        return;
      }
      for(Perm perm : entity.getPerms()) {
        Future savePermFuture = savePerm(perm, tenantId, vertxContext);
        futureList.add(savePermFuture);
      }
      CompositeFuture compositeFuture = CompositeFuture.join(futureList);
      compositeFuture.setHandler(compositeResult->{
        if(compositeResult.failed()) {
          logger.debug("Error adding permissions set: " + compositeResult.cause().getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(PostTenantpermissionsResponse.withPlainInternalServerError("Internal Server Error")));
        } else {
          asyncResultHandler.handle(Future.succeededFuture(PostTenantpermissionsResponse.withJsonCreated(entity)));
        }
      });
    });
    } catch(Exception e) {
      logger.debug("Error adding permissions set: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(PostTenantpermissionsResponse.withPlainInternalServerError("Internal Server Error")));
    }

  }

  private Future savePerm(Perm perm, String tenantId, Context vertxContext) {
    Future future = Future.future();
    if(perm.getPermissionName() == null) {
      return Future.succeededFuture();
    }
    Permission permission = new Permission();
    permission.setMutable(false);
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
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit), true, false, getReply -> {
        if(getReply.failed()) {
          logger.debug("Error querying permission: " + getReply.cause().getLocalizedMessage());
          future.fail(getReply.cause());
        } else {
          List<Permission> returnList = (List<Permission>)getReply.result()[0];
          if(returnList.size() > 0) {
            future.complete(); //Perm already exists, no need to re-add
          } else {
            //Need to save the new perm
            PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
            postgresClient.startTx(beginTx -> {
              String newId = UUID.randomUUID().toString();
              permission.setId(newId);
              try {
                postgresClient.save(beginTx, TABLE_NAME_PERMS, permission, postReply -> {
                  if(postReply.failed()) {
                    logger.debug("Error saving permission: " + postReply.cause().getLocalizedMessage());
                    future.fail(postReply.cause());
                  } else {
                    postgresClient.endTx(beginTx, done -> {
                      future.complete();
                    });
                  }
                });
              } catch(Exception e) {
                logger.debug("Error saving permission: " + e.getLocalizedMessage());
                future.fail(e);
              }
            });
          }
        }
      });
    } catch(Exception e) {
      logger.debug("Error running on vertx for savePerm: " + e.getLocalizedMessage());
      future.fail(e);
    }
    return future;
  }


}
