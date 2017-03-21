package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.ws.rs.core.Response;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.PermissionListObject;
import org.folio.rest.jaxrs.model.PermissionNameObject;
import org.folio.rest.jaxrs.model.PermissionNameListObject;
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
import org.z3950.zing.cql.cql2pgjson.FieldException;

/**
 *
 * @author kurt
 */
public class PermsAPI implements PermsResource {

  private static final String TABLE_NAME_PERMS = "permissions";
  private static final String TABLE_NAME_PERMSUSERS = "permissions_users";
  private static final String OKAPI_TENANT_HEADER = "x-okapi-tenant";
  private static final String OKAPI_PERMISSIONS_HEADER = "x-okapi-permissions";
  private static final String OKAPI_TOKEN_HEADER = "x-okapi-token";
  private static final String USER_NAME_FIELD = "'username'";
  private static final String PERMISSION_NAME_FIELD = "'permissionName'";
  private static final String ID_FIELD = "'id'";
  private final Logger logger = LoggerFactory.getLogger(PermsAPI.class);
  private static final String READ_PERMISSION_USERS_NAME = "perms.users.read";

  private CQLWrapper getCQL(String query, int limit, int offset) throws FieldException{
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(TABLE_NAME_PERMS + ".jsonb");
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
  }

  private final Messages messages = Messages.getInstance();

  @Override
  public void getPermsUsers(int length, int start, String sortBy, String query,
          String hasPermissions, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext)
          throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        CQLWrapper cql;
        try {
          cql = getCQL(query, length, start-1);
        } catch(Exception e) {
          asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersResponse.withPlainBadRequest(
                          "CQL Parsing Error for '" + query + "': " + e.getLocalizedMessage())));
          return;
        }
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        String[] fieldList = {"*"};
        if(!allowAccessByPermission(okapiHeaders.get(OKAPI_PERMISSIONS_HEADER), READ_PERMISSION_USERS_NAME)) {
           asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersResponse.withPlainForbidden("Access Denied")));
        } else {
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
      String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
      vertxContext.runOnContext(v -> {
        //Check for existing user
        Criteria nameCrit = new Criteria();
        nameCrit.addField(USER_NAME_FIELD);
        nameCrit.setOperation("=");
        nameCrit.setValue(entity.getUsername());
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  TABLE_NAME_PERMSUSERS, PermissionUser.class,
                  new Criterion(nameCrit), true, false, queryReply -> {
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
        if(!allowAccessByNameorPermission(okapiHeaders.get(OKAPI_PERMISSIONS_HEADER),
                READ_PERMISSION_USERS_NAME,
                okapiHeaders.get(OKAPI_TOKEN_HEADER), username)) {
          asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernameResponse.withPlainForbidden("Access denied, insufficient permissions")));
        } else {
          try {
            PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                    TABLE_NAME_PERMSUSERS, PermissionUser.class,
                    new Criterion(nameCrit), true, false, queryReply -> {
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
              asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernameResponse.withPlainNoContent("")));
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
  public void getPermsUsersByUsernamePermissions(String username, String expanded, String full,
          Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {

    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria nameCrit = new Criteria();
        nameCrit.addField(USER_NAME_FIELD);
        nameCrit.setOperation("=");
        nameCrit.setValue(username);
        if(!allowAccessByNameorPermission(okapiHeaders.get(OKAPI_PERMISSIONS_HEADER),
                READ_PERMISSION_USERS_NAME,
                okapiHeaders.get(OKAPI_TOKEN_HEADER), username)) {
          asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernamePermissionsResponse.withPlainForbidden("Access denied, insufficient permissions")));
        } else {
          try {
            PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMSUSERS,
                    PermissionUser.class, new Criterion(nameCrit), true, false, getReply -> {
              if(getReply.failed()) {
                logger.debug("Error from get reply: " + getReply.cause().getLocalizedMessage());
                asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernamePermissionsResponse.withPlainInternalServerError("Internal server error")));
              } else {
                List<PermissionUser> userList = (List<PermissionUser>)getReply.result()[0];
                if(userList.isEmpty()) {
                  //404'd!
                  asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernamePermissionsResponse.withPlainNotFound("No user found by name " + username)));
                } else {
                  PermissionUser user = userList.get(0);
                  Future<List<String>> future;
                  List<String> permNameList = new ArrayList<>();
                    for(Object perm : user.getPermissions()) {
                      if(perm != null) {
                        permNameList.add((String)perm);
                      }
                  }
                  if(expanded == null || !expanded.equals("true")) {
                    future = Future.succeededFuture(permNameList);
                  } else {
                    future = this.getAllExpandedPermissions(permNameList, vertxContext, tenantId);
                  }
                  future.setHandler(res-> {
                    if(res.failed()) {
                      logger.debug("Error getting expanded permissions: " + res.cause().getLocalizedMessage());
                      asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernamePermissionsResponse.withPlainInternalServerError("Internal Server Error")));
                    } else {
                      if(full == null || !full.equals("true")) {
                        PermissionNameListObject pnlo = new PermissionNameListObject();
                        List<Object> objectList = new ArrayList<Object>();
                        for( String s : res.result() ) {
                          objectList.add(s);
                        }
                        pnlo.setPermissionNames(objectList);
                        pnlo.setTotalRecords(res.result().size());
                        asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernamePermissionsResponse.withJsonOK(pnlo)));
                      } else {
                        Future<PermissionNameListObject> pnloFuture = getAllFullPermissions(res.result(), vertxContext, tenantId);
                        pnloFuture.setHandler(fullRes -> {
                          if(fullRes.failed()) {
                            logger.debug("Error getting full permission objects: " + fullRes.cause().getLocalizedMessage());
                            asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernamePermissionsResponse.withPlainInternalServerError("Internal server error")));
                          } else {
                            asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernamePermissionsResponse.withJsonOK(fullRes.result())));
                          }
                        });
                      }
                    }
                  });
                }
              }
            });
          } catch(Exception e) {
            logger.debug("Error using Postgres instance: " + e.getLocalizedMessage());
            asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernamePermissionsResponse.withPlainInternalServerError("Internal server error")));
          }
        }
      });
    } catch(Exception e) {
      logger.debug("Error running on vertx context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByUsernamePermissionsResponse.withPlainInternalServerError("Internal server error")));
    }
  }
  @Override
  public void postPermsUsersByUsernamePermissions(String username, PermissionNameObject entity,
          Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria usernameCrit = new Criteria();
        usernameCrit.addField(USER_NAME_FIELD);
        usernameCrit.setOperation("=");
        usernameCrit.setValue(username);
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMSUSERS,
                  PermissionUser.class, new Criterion(usernameCrit), true, false, getReply-> {
            if(getReply.failed()) {
              logger.debug("Error checking for user: " + getReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByUsernamePermissionsResponse.withPlainInternalServerError("Internal server error")));
            } else {
              List<PermissionUser> userList = (List<PermissionUser>)getReply.result()[0];
              if(userList.size() == 0) {
                asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByUsernamePermissionsResponse.withPlainBadRequest("User " + username + " does not exist")));
              } else {
                //now we can actually add it
                String permissionName = entity.getPermissionName();
                PermissionUser user = userList.get(0);
                if(user.getPermissions().contains(permissionName)) {
                  asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByUsernamePermissionsResponse.withPlainBadRequest("User " + username + " already has permission " + permissionName)));
                } else {
                  user.getPermissions().add(permissionName);
                  try {
                  PostgresClient.getInstance(vertxContext.owner(), tenantId).update(TABLE_NAME_PERMSUSERS, user, new Criterion(usernameCrit), true, putReply -> {
                    if(putReply.failed()) {
                      logger.debug("Error attempting to update user: " + putReply.cause().getLocalizedMessage());
                      asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByUsernamePermissionsResponse.withPlainInternalServerError("Internal server error")));
                    } else {
                      asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByUsernamePermissionsResponse.withJsonOK(entity)));
                    }
                  });
                  } catch(Exception e) {
                    logger.debug("Error using Postgres instance to update user: " + e.getLocalizedMessage());
                    asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByUsernamePermissionsResponse.withPlainInternalServerError("Internal server error")));
                  }
                }
              }
            }
          });
        } catch(Exception e) {
          logger.debug("Error using Postgres instance to retrieve user: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByUsernamePermissionsResponse.withPlainInternalServerError("Internal server error")));
        }

      });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByUsernamePermissionsResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void deletePermsUsersByUsernamePermissionsByPermissionname(String permissionname,
          String username, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria usernameCrit = new Criteria();
        usernameCrit.addField(USER_NAME_FIELD);
        usernameCrit.setOperation("=");
        usernameCrit.setValue(username);
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMSUSERS,
                  PermissionUser.class, new Criterion(usernameCrit), true, false, getReply-> {
            if(getReply.failed()) {
              logger.debug("Error checking for user: " + getReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernamePermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
            } else {
              List<PermissionUser> userList = (List<PermissionUser>)getReply.result()[0];
              if(userList.size() == 0) {
                asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernamePermissionsByPermissionnameResponse.withPlainBadRequest("User " + username + " does not exist")));
              } else {
                //attempt to delete permission
                PermissionUser user = userList.get(0);
                if(!user.getPermissions().contains(permissionname)) {
                  asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernamePermissionsByPermissionnameResponse.withPlainBadRequest("User " + username + " does not contain " + permissionname)));
                } else {
                  try {
                    user.getPermissions().remove(permissionname);
                    PostgresClient.getInstance(vertxContext.owner(), tenantId).update(TABLE_NAME_PERMSUSERS, user, new Criterion(usernameCrit), true, putReply -> {
                      if(putReply.failed()) {
                         logger.debug("Error attempting to update user: " + putReply.cause().getLocalizedMessage());
                         asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernamePermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
                      } else {
                        asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernamePermissionsByPermissionnameResponse.withPlainNoContent("")));
                      }
                    });
                  } catch(Exception e) {
                    logger.debug("Error using Postgres instance to delete permission from user");
                    asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernamePermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
                  }
                }
              }
            }
          });
        } catch(Exception e) {
          logger.debug("Error using Postgres instance to retrieve user: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernamePermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByUsernamePermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
    }
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
                  TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit), true, false, getReply-> {
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
                  String newId = UUID.randomUUID().toString();
                  entity.setAdditionalProperty("id", newId);
                  try {
                    postgresClient.save(beginTx, TABLE_NAME_PERMS, entity, postReply -> {
                      if(postReply.failed()) {
                        logger.debug("Unable to save new permission: " + postReply.cause().getLocalizedMessage());
                        asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
                      } else {
                        postgresClient.endTx(beginTx, done -> {
                          asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withJsonCreated(entity)));
                        });
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
  }

  @Override
  public void getPermsPermissionsById(String id, Map<String,
          String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria idCrit = new Criteria();
        idCrit.addField(ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(id);
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS, Permission.class, new Criterion(idCrit), true, false, getReply -> {
            if(getReply.failed()) {
              logger.debug("Error in getReply: " + getReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
            } else {
              List<Permission> permList = (List<Permission>)getReply.result()[0];
              if(permList.size() < 1) {
                //404'd!
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withPlainNotFound("No permission with ID " + id + " exists")));
              } else if(permList.size() > 1) {
                //Too many results!
                logger.debug("Multiple results found for ID " + id);
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withJsonOK(permList.get(0))));
              }
            }
          });
        } catch(Exception e) {
          logger.debug("Error getting Permission with Postgres client: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void putPermsPermissionsById(String id,
          Permission entity, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v-> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria idCrit = new Criteria();
        idCrit.addField(ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(entity.getId());
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS, Permission.class, new Criterion(idCrit), true, false, getReply -> {
            if(getReply.failed()) {
              logger.debug("Error with get: " + getReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
            }
            List<Permission> permList = (List<Permission>)getReply.result()[0];
            if(permList.size() < 1) {
              asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainInternalServerError("No such permission")));
            } else {
              Permission perm = permList.get(0);
              if(perm.getPermissionName() != entity.getPermissionName()) {
                asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainBadRequest("permission name property cannot change")));
              } else {
                try {
                  PostgresClient.getInstance(vertxContext.owner(), tenantId).update(TABLE_NAME_PERMS, entity, new Criterion(idCrit), true, putReply -> {
                    if(putReply.failed()) {
                      logger.debug("Error with put: " + putReply.cause().getLocalizedMessage());
                      asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
                    } else {
                      asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withJsonOK(entity)));
                    }
                  });
                }
                catch(Exception e) {
                  logger.debug("Error using Postgres instance: " + e.getLocalizedMessage());
                  asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
                }
              }
            }
          });
        } catch(Exception e) {
          logger.debug("Error using Postgres instance: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void deletePermsPermissionsById(String id,
          Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria idCrit = new Criteria();
        idCrit.addField(ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(id);
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).delete(TABLE_NAME_PERMS, new Criterion(idCrit), deleteReply -> {
            if(deleteReply.failed()) {
              logger.debug("deleteReply failed: " + deleteReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(DeletePermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
            } else {
              asyncResultHandler.handle(Future.succeededFuture(DeletePermsPermissionsByIdResponse.withPlainNoContent("")));
            }
          });
        } catch(Exception e) {
          logger.debug("Error using Postgres instance: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(DeletePermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(DeletePermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void getPermsPermissions(String expanded, int length, int start, String sortBy, String query,
          String memberOf, String ownedBy, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext)
          throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        CQLWrapper cql;
        try {
          cql = getCQL(query, length, start-1);
        } catch(Exception e) {
          asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.withPlainBadRequest(
                        "CQL Parsing Error for '" + query + "': " + e.getLocalizedMessage())));
          return;
        }
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        String[] fieldList = {"*"};
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
                  Permission.class, fieldList, cql, true, false, getReply -> {
            if(getReply.succeeded()) {
              PermissionListObject permCollection = new PermissionListObject();
              List<Permission> permissions = (List<Permission>)getReply.result()[0];
              List<Future> futureList = new ArrayList<>();
              for(Permission permission : permissions) {
                List<Object> subPermList = permission.getSubPermissions();
                Future<Permission> permFuture = Future.succeededFuture(permission);
                futureList.add(permFuture);
              }
              CompositeFuture compositeFuture = CompositeFuture.join(futureList);
              compositeFuture.setHandler(compositeResult -> {
                if(compositeFuture.failed()) {
                  logger.debug("Error expanding permissions: " + compositeFuture.cause().getLocalizedMessage());
                  asyncResultHandler.handle(Future.succeededFuture(compositeResult.withPlainInternalServerError(getReply.cause().getLocalizedMessage())));
                } else {
                  List<Permission> newPermList = new ArrayList<>();
                  for(Future f : futureList) {
                    newPermList.add((Permission)(f.result()));
                  }
                  permCollection.setPermissions(newPermList);
                  permCollection.setTotalRecords(newPermList.size());
                  asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.withJsonOK(permCollection)));
                }
                
              });        
            } else {
              logger.debug("Error with getReply: " + getReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.withPlainInternalServerError(getReply.cause().getLocalizedMessage())));
            }
          });
        } catch(Exception e) {
          if(e.getCause() != null && e.getCause().getClass().getSimpleName().contains("CQLParseException")) {
                logger.debug("BAD CQL:" + e.getLocalizedMessage());
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.withPlainBadRequest(
                        "CQL Parsing Error for '" + query + "': " + e.getLocalizedMessage())));
          } else {
            logger.debug("Error getting Postgres client: " + e.getLocalizedMessage());
            asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                    GetPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
          }
        }
      });
    } catch(Exception e) {
      logger.debug("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  private Future<Boolean> checkPermissionExists(String permissionName, Context vertxContext, String tenantId) {
    Future<Boolean> future = Future.future();
    try {
      vertxContext.runOnContext(v -> {
        Criteria nameCrit = new Criteria();
        nameCrit.addField(PERMISSION_NAME_FIELD);
        nameCrit.setOperation("=");
        nameCrit.setValue(permissionName);
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
                  Permission.class, new Criterion(nameCrit), true, false, getReply -> {
            if(getReply.failed()) {
              logger.debug("Error in getReply: " + getReply.cause().getLocalizedMessage());
              future.fail(getReply.cause());
            } else {
              List<Permission> permList = (List<Permission>)getReply.result()[0];
              if(permList.size() == 0) {
                future.complete(Boolean.FALSE);
              } else {
                future.complete(Boolean.TRUE);
              }
            }
          });
        } catch(Exception e) {
          logger.debug("Error from PostgresClient instance: " + e.getLocalizedMessage());
          future.fail(e);
        }
      });
    } catch(Exception e) {
      logger.debug("Error running on vertx context: " + e.getLocalizedMessage());
      future.fail(e);
    }
    return future;
  }

  private Future<Boolean> checkPermissionListExists(List<Object> permissionList, Context vertxContext, String tenantId) {
    Future<Boolean> future = Future.future();
    List<Future> futureList = new ArrayList<>();
    for(Object permissionName : permissionList ) {
      Future<Boolean> checkFuture = checkPermissionExists((String)permissionName, vertxContext, tenantId);
      futureList.add(checkFuture);
    }
    CompositeFuture compositeFuture = CompositeFuture.all(futureList);
    compositeFuture.setHandler(res -> {
      if(res.failed()) {
        future.fail(res.cause());
      } else {
        boolean allPermissionsExist = true;
        for(Future<Boolean> permCheckFuture : futureList) {
          if(!permCheckFuture.result()) {
            allPermissionsExist = false;
            break;
          }
        }
        future.complete(allPermissionsExist);
      }
    });
    return future;
  }

  private Future<List<String>> getAllExpandedPermissions(List<String> permissionList, Context vertxContext, String tenantId) {
    Future<List<String>> future = Future.future();
    List<String> masterPermissionList = new ArrayList<>();
    List<Future> futureList = new ArrayList<>();
    for(String permission : permissionList) {
      if(permission != null) {
        Future permFuture = getExpandedPermissions(permission, vertxContext, tenantId);
        futureList.add(permFuture);
      }
    }
    CompositeFuture compositeFuture = CompositeFuture.all(futureList);
    compositeFuture.setHandler(res->{
      if(res.failed()) {
        future.fail(res.cause());
      } else {
        for(Future completedFuture : futureList) {
          List<String> subPermissionList = ((Future<List<String>>)completedFuture).result();
          for(String subPerm : subPermissionList) {
            if(!masterPermissionList.contains(subPerm)) {
              masterPermissionList.add(subPerm);
            }
          }
        }
        future.complete(masterPermissionList);
      }
    });
    return future;
  }

  private Future<List<String>> getExpandedPermissions(String permissionName, Context vertxContext, String tenantId) {
    logger.debug("Getting expanded permissions for permission '" + permissionName + "'");
    Future<List<String>> future = Future.future();
    List<String> expandedPermissions = new ArrayList<>();
    expandedPermissions.add(permissionName);
    try {
      vertxContext.runOnContext(v-> {
        Criteria nameCrit = new Criteria();
        nameCrit.addField(PERMISSION_NAME_FIELD);
        nameCrit.setOperation("=");
        nameCrit.setValue(permissionName);
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
                  Permission.class, new Criterion(nameCrit), true, false, getReply -> {
            if(getReply.failed()) {
              logger.debug("Error in get request: " + getReply.cause().getLocalizedMessage());
              future.fail(getReply.cause());
            } else {
              List<Permission> permList = (List<Permission>)getReply.result()[0];
              if(permList.isEmpty()) {
                 future.complete(new ArrayList<String>());
              } else {
                Permission permission = permList.get(0);
                if(!permission.getSubPermissions().isEmpty()) {
                  List<Future> futureList = new ArrayList<Future>();
                  for(Object subPermissionName : permission.getSubPermissions()) {
                    Future<List<String>> subPermFuture = getExpandedPermissions((String)subPermissionName, vertxContext, tenantId);
                    futureList.add(subPermFuture);
                  }
                  CompositeFuture compositeFuture = CompositeFuture.all(futureList);
                  compositeFuture.setHandler(compRes -> {
                    if(compRes.failed()) {
                      future.fail(compRes.cause());
                    } else {
                      for(Future finishedFuture : futureList) {
                        for(String subPermissionName : ((Future<List<String>>)finishedFuture).result()) {
                          if(!expandedPermissions.contains(subPermissionName)) {
                            expandedPermissions.add(subPermissionName);
                          }
                        }
                      }
                      future.complete(expandedPermissions);
                    }
                  });
                } else {
                  future.complete(expandedPermissions);
                }
              }
            }
          });
        } catch(Exception e) {
          logger.debug("Error getting users from Postgres: " + e.getLocalizedMessage());
          future.fail(e);
        }
      });
    } catch(Exception e) {
      logger.debug("Error running on vertx context: " + e.getLocalizedMessage());
      future.fail(e);
    }
    return future;
  }

  private Future<PermissionNameListObject> getAllFullPermissions(List<String> nameList, Context vertxContext, String tenantId) {
    Future<PermissionNameListObject> future = Future.future();
    List<Future> futureList = new ArrayList<>();
    for(String name : nameList) {
      Future<Permission> permissionFuture = getFullPermissions(name, vertxContext, tenantId);
      futureList.add(permissionFuture);
    }
    CompositeFuture compositeFuture = CompositeFuture.all(futureList);
    compositeFuture.setHandler(res -> {
      if(res.failed()) {
        future.fail(res.cause());
      } else {
        PermissionNameListObject pnlo = new PermissionNameListObject();
        List<Object> permList = new ArrayList<>();
        for(Future doneFuture : futureList) {
         Object result = doneFuture.result();
         permList.add(result);
        }
        pnlo.setPermissionNames(permList);
        future.complete(pnlo);
      }
    });
    return future;
  }

  private Future<Permission> getFullPermissions(String permissionName, Context vertxContext, String tenantId) {
   Future<Permission> future = Future.future();
   try {
     vertxContext.runOnContext(v-> {
       Criteria nameCrit = new Criteria();
       nameCrit.addField(PERMISSION_NAME_FIELD);
       nameCrit.setOperation("=");
       nameCrit.setValue(permissionName);
       try {
         PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
                  Permission.class, new Criterion(nameCrit), true, false, getReply -> {
           if(getReply.failed()) {
             future.fail(getReply.cause());
           } else {
             List<Permission> permList = (List<Permission>)getReply.result()[0];
             if(permList.isEmpty()) {
               future.fail("No permission object found for name '" + permissionName + "'");
             } else {
               future.complete(permList.get(0));
             }
           }
         });
       } catch(Exception e) {
        logger.debug("Error from PostgresClient: " + e.getLocalizedMessage());
        future.fail(e);
       }
     });
   } catch(Exception e) {
     logger.debug("Error running on vertx context: " + e.getLocalizedMessage());
     future.fail(e);
   }
   return future;
  }

  private JsonObject parseTokenPayload(String token) {
    String[] tokenParts = token.split("\\.");
    if(tokenParts.length == 3) {
      String encodedPayload = tokenParts[1];
      byte[] decodedJsonBytes = Base64.getDecoder().decode(encodedPayload);
      String decodedJson = new String(decodedJsonBytes);
      return new JsonObject(decodedJson);
    } else {
      return null;
    }
  }

  private String getUsername(String token) {
    JsonObject payload = parseTokenPayload(token);
    if(payload == null) { return null; }
    String username = payload.getString("sub");
    return username;
  }

  private boolean allowAccessByNameorPermission(String permissions, String permissionName, String token, String username) {
    String tokenUsername = getUsername(token);
    if(tokenUsername.equals(username)) {
      logger.debug("Permission allowed for own username (" + username + ")");
      return true;
    }
    return allowAccessByPermission(permissions, permissionName);
  }

  private boolean allowAccessByPermission(String permissions, String permissionName) {
    if(permissions == null || permissions.isEmpty()) {
      return false;
    }
    JsonArray permissionsArray = new JsonArray(permissions);
    if(permissionsArray != null && permissionsArray.contains(permissionName)) {
      logger.debug("Permission allowed for possessing permission bit '" + permissionName + "'");
      return true;
    }
    return false;
  }

}
