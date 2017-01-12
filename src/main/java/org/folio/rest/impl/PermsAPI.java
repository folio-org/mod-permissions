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
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.OutStream;
import org.folio.rest.tools.utils.TenantTool;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSON;

/**
 *
 * @author kurt
 */
public class PermsAPI implements PermsResource {
  
  private static final String TABLE_NAME_PERMS = "permissions";
  private static final String TABLE_NAME_PERMSUSERS = "permissions_users";
  private static final String OKAPI_TENANT_HEADER = "X-Okapi-Tenant";
  private static final String USER_NAME_FIELD = "'username'";
  private static final String PERMISSION_NAME_FIELD = "'permission_name'";
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
                  TABLE_NAME_PERMSUSERS, PermissionUser.class, fieldList, cql, true,
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
  public void postPermsUsers(PermissionUser entity, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        //Check for existing user
        Criteria nameCrit = new Criteria();
        nameCrit.addField(USER_NAME_FIELD);
        nameCrit.setOperation("=");
        nameCrit.setValue(entity.getUsername());
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  TABLE_NAME_PERMSUSERS, PermissionUser.class,
                  new Criterion(nameCrit), true, queryReply -> {
            if(queryReply.failed()) {
              logger.debug("Unable to query permissions users: " + queryReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError("Internal server error")));
            } else {
              List<PermissionUser> userList = (List<PermissionUser>)queryReply.result()[0];
              if(userList.size() > 0) {
                //This means that we have an existing user matching this username, error 400
                logger.debug("Permissions user" + entity.getUsername() + " already exists");
                asyncResultHandler.handle(Future.succeededFuture(
                        PostPermsUsersResponse.withPlainBadRequest(
                                "Username " + entity.getUsername() + " already exists")));
              } else {
                //Proceed to POST new user
                PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
                postgresClient.startTx(beginTx -> {
                  logger.debug("Starting transaction to save new permissions user");
                  try {
                    postgresClient.save(beginTx, TABLE_NAME_PERMSUSERS, entity, postReply -> {
                      try {
                        if(postReply.succeeded()) {
                          final PermissionUser permUser = entity;  
                          postgresClient.endTx(beginTx, endTx -> {
                            asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withJsonCreated(entity)));
                          });
                        } else {
                          logger.debug("Unable to save: " + postReply.cause().getLocalizedMessage());
                          asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError("Internal server error")));
                        }
                      } catch(Exception e) {
                        logger.debug("Error saving entity " + entity.toString() + ": " + e.getLocalizedMessage());
                        asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError("Internal server error")));
                      }
                     });
                  } catch(Exception e) {
                    logger.debug("Error in transaction for entity " + entity.toString() + ": " + e.getLocalizedMessage());
                    asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError("Internal server error")));
                  }
                });
              }
            }
          });
        } catch(Exception e) {
          logger.debug("Error querying existing permissions user: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void getPermsUsersByUsername(String username, Map<String,
          String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria nameCrit = new Criteria();
        nameCrit.addField(USER_NAME_FIELD);
        nameCrit.setOperation("=");
        nameCrit.setValue(username);
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  TABLE_NAME_PERMSUSERS, PermissionUser.class,
                  new Criterion(nameCrit), true, queryReply -> {
            if(queryReply.failed()) {
              logger.debug("queryReply failed: " + queryReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernameResponse.withPlainInternalServerError("Internal server error")));
            } else {
              List<PermissionUser> userList = (List<PermissionUser>)queryReply.result()[0];
              if(userList.size() < 1) {
                //no users found
                asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernameResponse.withPlainNotFound("No user with username: " + username)));
              } else if(userList.size() > 1) {
                //WTF, we got multiples for a single username? That ain't right
                logger.debug("Multiple permissions users matched for username: " + username);
                asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernameResponse.withPlainInternalServerError("Internal server error")));
              } else {
                //return the permissions user object
                asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernameResponse.withJsonOK(userList.get(0))));
              }
            }
          });
        } catch(Exception e) {
          logger.debug("Error getting query from Postgres: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernameResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernameResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void putPermsUsersByUsername(String username, PermissionUser entity, Map<String, 
          String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, 
          Context vertxContext) throws Exception {
    try {
       vertxContext.runOnContext(v -> {
         String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
         Criteria nameCrit = new Criteria();
         nameCrit.addField(USER_NAME_FIELD);
         nameCrit.setOperation("=");
         nameCrit.setValue(username);
         try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).update(
                   TABLE_NAME_PERMSUSERS, entity, new Criterion(nameCrit), true, putReply -> {
            try {
              if(putReply.failed()) {
                logger.debug("Error with put: " + putReply.cause().getLocalizedMessage());
              } else {
                asyncResultHandler.handle(Future.succeededFuture(PutPermsUsersByUsernameResponse.withJsonOK(entity)));
              }
            } catch(Exception e) {
              logger.debug("Error getting put reply: " + e.getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(PutPermsUsersByUsernameResponse.withPlainInternalServerError("Internal server error")));
            }   
          });
         } catch(Exception e) {
           logger.debug("Error using Postgres instance: " + e.getLocalizedMessage());
           asyncResultHandler.handle(Future.succeededFuture(PutPermsUsersByUsernameResponse.withPlainInternalServerError("Internal server error")));
         }
       });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(PutPermsUsersByUsernameResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void deletePermsUsersByUsername(String username, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria nameCrit = new Criteria();
        nameCrit.addField(USER_NAME_FIELD);
        nameCrit.setOperation("=");
        nameCrit.setValue(username);
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).delete(TABLE_NAME_PERMSUSERS, new Criterion(nameCrit), deleteReply-> {
            if(deleteReply.failed()) {
              logger.debug("deleteReply failed: " + deleteReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernameResponse.withPlainInternalServerError("Internal server error")));
            } else {
              //We need a way to detect for 404 not found here.
              asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernameResponse.withNoContent()));
            }
          });
        } catch(Exception e) {
          logger.debug("Error using Postgres instance: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernameResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernameResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void postPermsUsersByUsernamePermissions(String username,
          Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void deletePermsUsersByUsernamePermissionsByPermissionname(String permissionname, String username, Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }


  @Override
  public void postPermsPermissions(Permission entity, Map<String, String> okapiHeaders, 
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext)
          throws Exception {
    try {
      vertxContext.runOnContext(v-> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria nameCrit = new Criteria();
        nameCrit.addField(PERMISSION_NAME_FIELD);
        nameCrit.setOperation("=");
        nameCrit.setValue(entity.getPermissionName());
        try {
          PostgresClient.getInstance(vertxContext.owner(), TenantTool.calculateTenantId(tenantId)).get(
                  TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit), true, getReply-> {
            if(getReply.failed()) {
              logger.debug("Error getting existing permissions: " + getReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
            } else {
              List<Permission> permissionList = (List<Permission>)getReply.result()[0];
              if(permissionList.size() > 0) {
                logger.debug("Permission with this name already exists");
                asyncResultHandler.handle(Future.succeededFuture(
                        PostPermsPermissionsResponse.withPlainBadRequest(
                                "Permission with name " + entity.getPermissionName() + " already exists")));      
              } else {
                //Do the actual POST of the new permission
                PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
                postgresClient.startTx(beginTx-> {
                  logger.debug("Attempting to save new Permission");
                  try {
                    postgresClient.save(beginTx, TABLE_NAME_PERMS, entity, postReply -> {
                      if(postReply.failed()) {
                        logger.debug("Unable to save new permission: " + postReply.cause().getLocalizedMessage());
                        asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
                      } else {
                        asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withJsonCreated(entity)));
                      }
                    });
                  } catch(Exception e) {
                    logger.debug("Error with Postgres client while saving permission: " + e.getLocalizedMessage());
                    asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
                  }
                });                        
              }
            }
          });
        } catch(Exception e) {
          logger.debug("Error calling Postgresclient: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
    }
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void getPermsPermissionsByPermissionname(String permissionname, Map<String,
          String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria nameCrit = new Criteria();
        nameCrit.addField(PERMISSION_NAME_FIELD);
        nameCrit.setOperation("=");
        nameCrit.setValue(permissionname);
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit), true, false, getReply -> {
            if(getReply.failed()) {
              logger.debug("Error in getReply: " + getReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
            } else {
              List<Permission> permList = (List<Permission>)getReply.result()[0];
              if(permList.size() < 1) {
                //404'd!
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByPermissionnameResponse.withPlainNotFound("No permission by the name " + permissionname + " exists")));
              } else if(permList.size() > 1) {
                //Too many results!
                logger.debug("Multiple results found for permission name " + permissionname);
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByPermissionnameResponse.withJsonOK(permList.get(0))));
              }
            }         
          });
        } catch(Exception e) {
          logger.debug("Error getting Permission with Postgres client: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
    }
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
