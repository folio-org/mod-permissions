/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import java.util.List;
import java.util.Map;
import javax.ws.rs.core.Response;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.PermissionPatch;
import org.folio.rest.jaxrs.model.PermissionUser;
import org.folio.rest.jaxrs.model.PermissionUserListObject;
import org.folio.rest.jaxrs.resource.PermsResource;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.TenantTool;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSON;

/**
 *
 * @author kurt
 */
public class PermsAPI implements PermsResource {
  
  private static final String TABLE_NAME_PERMS = "permissions";
  private static final String OKAPI_TENANT_HEADER = "X-Okapi-Tenant";
  private final Logger logger = LoggerFactory.getLogger(PermsAPI.class);
  
  private CQLWrapper getCQL(String query, int limit, int offset){
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(TABLE_NAME_PERMS + ".jsonb");
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
  }
  
  private final Messages messages = Messages.getInstance();

  @Override
  public void getPermsUsers(String query, Integer length, Integer start, 
          String sortBy, String hasPermissions, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext)
          throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        CQLWrapper cql = getCQL(query, length, start);
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        String[] fieldList = {"*"};
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  TABLE_NAME_PERMS, PermissionUser.class, fieldList, cql, true,
                  false, reply -> {
            try {
              if(reply.succeeded()) {
                PermissionUserListObject permUserCollection = new PermissionUserListObject();
                List<PermissionUser> permissionUsers = (List<PermissionUser>)reply.result()[0];
                permUserCollection.setPermissionUsers(permissionUsers);
                permUserCollection.setTotalRecords(permissionUsers.size());
                asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersResponse.withJsonOK(permUserCollection)));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                        GetPermsUsersResponse.withPlainInternalServerError(
                                reply.cause().getLocalizedMessage())));
              }
            } catch(Exception e) {
              logger.debug("Error building response from reply: " + e.getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersResponse.withPlainInternalServerError("Internal server error")));
            }
          });
        } catch(Exception e) {
          if(e.getCause() != null && e.getCause().getClass().getSimpleName().contains("CQLParseException")) {
                logger.debug("BAD CQL:" + e.getLocalizedMessage());
                asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersResponse.withPlainBadRequest(
                        "CQL Parsing Error for '" + query + "': " + e.getLocalizedMessage())));
              } else {
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                              GetPermsUsersResponse.withPlainInternalServerError("Internal server error")));
              }
        }
      });      
    } catch(Exception e) {
      logger.debug("Error running vertx on context:" + e.getLocalizedMessage());
      asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                        GetPermsUsersResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void postPermsUsers(PermissionUser entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void getPermsUsersByUsername(String username, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void putPermsUsersByUsername(String username, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void deletePermsUsersByUsername(String username, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void postPermsUsersByUsernamePermissions(String username, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void deletePermsUsersByUsernamePermissionsByPermissionname(String permissionname, String username, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }


  @Override
  public void postPermsPermissions(Permission entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void getPermsPermissionsByPermissionname(String permissionname, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void putPermsPermissionsByPermissionname(String permissionname, Permission entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void patchPermsPermissionsByPermissionname(String permissionname, PermissionPatch entity, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void deletePermsPermissionsByPermissionname(String permissionname, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void getPermsPermissions(String query, Integer length, Integer start, String sortBy, String memberOf, String ownedBy, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

}
