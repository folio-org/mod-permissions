package org.folio.rest.impl;

import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.net.URLDecoder;
import javax.ws.rs.core.Response;

import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.PermissionListObject;
import org.folio.rest.jaxrs.model.PermissionNameListObject;
import org.folio.rest.jaxrs.model.PermissionNameObject;
import org.folio.rest.jaxrs.model.PermissionUser;
import org.folio.rest.jaxrs.model.PermissionUserListObject;
import org.folio.rest.jaxrs.model.PermissionUploadJson;
import org.folio.rest.jaxrs.resource.PermsResource;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.tools.utils.ValidationHelper;
import org.z3950.zing.cql.cql2pgjson.CQL2PgJSON;
import org.z3950.zing.cql.cql2pgjson.FieldException;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

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
  private static final String USER_ID_FIELD = "'userId'";
  private static final String ID_FIELD = "'id'";
  private static final String PERMISSION_SCHEMA_PATH = "apidocs/raml-util/schemas/mod-permissions/permission.json";

  private static final String PERMISSION_NAME_FIELD = "'permissionName'";
  private final Logger logger = LoggerFactory.getLogger(PermsAPI.class);
  private static final String READ_PERMISSION_USERS_NAME = "perms.users.get";
  private static boolean suppressErrorResponse = false;
  private CQLWrapper getCQL(String query, String tableName, int limit, int offset) throws FieldException{
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(tableName + ".jsonb");
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
  }

  private final Messages messages = Messages.getInstance();

  private String getErrorResponse(String response) {
    if(suppressErrorResponse) {
      return "Internal Server Error: Please contact Admin";
    }
    return response;
  }

  @Override
  public void getPermsUsers(int length, int start, String sortBy, String query,
          String hasPermissions, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext)
          throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        CQLWrapper cql;
        try {
          cql = getCQL(query, TABLE_NAME_PERMSUSERS, length, start-1);
        } catch(Exception e) {
          logger.error(e.getMessage(), e);
          asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersResponse.withPlainBadRequest(
                          "CQL Parsing Error for '" + query + "': " + e.getLocalizedMessage())));
          return;
        }
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        String[] fieldList = {"*"};
        if(false) {
          //de nada
        } else {
          try {
            PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                    TABLE_NAME_PERMSUSERS, PermissionUser.class, fieldList, cql, true,
                    false, reply -> {
              try {
                if(reply.succeeded()) {
                  PermissionUserListObject permUserCollection = new PermissionUserListObject();
                  List<PermissionUser> permissionUsers = (List<PermissionUser>)reply.result().getResults();
                  permUserCollection.setPermissionUsers(permissionUsers);
                  permUserCollection.setTotalRecords(reply.result().getResultInfo().getTotalRecords());
                  asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersResponse.withJsonOK(permUserCollection)));
                } else {
                  String errStr = "Get operation from PostgresClient failed: " + reply.cause().getLocalizedMessage();
                  logger.error(errStr);
                  asyncResultHandler.handle(Future.succeededFuture(
                          GetPermsUsersResponse.withPlainInternalServerError(errStr)));
                }
              } catch(Exception e) {
                String errStr = "Error building response from reply: " + e.getLocalizedMessage();
                logger.error(errStr, e);
                asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersResponse.withPlainInternalServerError(getErrorResponse(errStr))));
              }
            });
          } catch(Exception e) {
            String errStr = e.getLocalizedMessage();
            logger.error(errStr, e);
            if(e.getCause() != null && e.getCause().getClass().getSimpleName().contains("CQLParseException")) {
                  asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersResponse.withPlainBadRequest(
                          "CQL Parsing Error for '" + query + "': " + errStr)));
                } else {
                  asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                                GetPermsUsersResponse.withPlainInternalServerError(getErrorResponse(errStr))));
                }
          }
        }
      });
    } catch(Exception e) {
      String errStr = "Error running vertx on context:" + e.getLocalizedMessage();
      logger.error(errStr, e);
      asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                        GetPermsUsersResponse.withPlainInternalServerError(getErrorResponse(errStr))));
    }
  }

  @Override
  public void postPermsUsers(PermissionUser entity, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    try {
      String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
      vertxContext.runOnContext(v -> {
        //Check for existing user
        Criteria userIdCrit = new Criteria();
        userIdCrit.addField(USER_ID_FIELD);
        userIdCrit.setOperation("=");
        userIdCrit.setValue(entity.getUserId());
        Criteria idCrit = new Criteria();
        idCrit.addField(ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(entity.getId());
        Criterion criterion = new Criterion();
        criterion.addCriterion(idCrit, "OR", userIdCrit);
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  TABLE_NAME_PERMSUSERS, PermissionUser.class,
                  criterion, true, false, queryReply -> {
            if(queryReply.failed()) {
              String errStr = "Unable to query permissions users: " + queryReply.cause().getLocalizedMessage();
              logger.error(errStr, queryReply.cause());
              asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError(getErrorResponse(errStr))));
            } else {
              List<PermissionUser> userList = (List<PermissionUser>)queryReply.result().getResults();
              if(userList.size() > 0) {
                //This means that we have an existing user matching this username, error 400
                logger.warn("Constraint violated for userId or id field (or both)");
                asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsUsersResponse.withJsonUnprocessableEntity(
                    ValidationHelper.createValidationErrorMessage(
                      ID_FIELD, entity.getId(),
                      "userId and id fields must not match values for any existing records"))));
              } else {
                //Proceed to POST new user
                if(entity.getId() == null) {
                  entity.setId(UUID.randomUUID().toString());
                }
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
                          String errStr = "Unable to save: " + postReply.cause().getLocalizedMessage();
                          logger.error(errStr, postReply.cause());
                          asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError(getErrorResponse(errStr))));
                        }
                      } catch(Exception e) {
                        String errStr = "Error saving entity " + entity.toString() + ": " + e.getLocalizedMessage();
                        logger.error(errStr, e);
                        asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError(getErrorResponse(errStr))));
                      }
                     });
                  } catch(Exception e) {
                    String errStr = "Error in transaction for entity " + entity.toString() + ": " + e.getLocalizedMessage();
                    logger.error(errStr, e);
                    asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError(getErrorResponse(errStr))));
                  }
                });
              }
            }
          });
        } catch(Exception e) {
          String errStr = "Error querying existing permissions user: " + e.getLocalizedMessage();
          logger.error(errStr, e);
          asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError(getErrorResponse(errStr))));
        }
      });
    } catch(Exception e) {
      String errStr = "Error running vertx on context: " + e.getLocalizedMessage();
      logger.error(errStr, e);
      asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withPlainInternalServerError(getErrorResponse(errStr))));
    }
  }

  @Override
  public void getPermsUsersById(String id, String indexField, Map<String,
          String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      String decodedId = URLDecoder.decode(id);
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        if(false) {
          //Do nothing, because it never happens
        } else {
          try {
            Criteria idCrit = getIdCriteria(indexField, "=", decodedId);
            PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMSUSERS, PermissionUser.class,
                    new Criterion(idCrit), true, false, queryReply -> {
              if(queryReply.failed()) {
                String errStr = "queryReply failed: " + queryReply.cause().getLocalizedMessage();
                logger.error(errStr);
                asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdResponse.withPlainInternalServerError(getErrorResponse(errStr))));
              } else {
                List<PermissionUser> userList = (List<PermissionUser>)queryReply.result().getResults();
                if(userList.size() < 1) {
                  //no users found
                  asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdResponse.withPlainNotFound("No user with id: " + id)));
                } else if(userList.size() > 1) {
                  //WTF, we got multiples for a single username? That ain't right
                  String errStr = "Multiple permissions users matched for id: " + id;
                  logger.error(errStr);
                  asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdResponse.withPlainInternalServerError(getErrorResponse(errStr))));
                } else {
                  //return the permissions user object
                  asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdResponse.withJsonOK(userList.get(0))));
                }
              }
            });
          } catch(Exception e) {
            String errStr = "Error getting query from Postgres: " + e.getLocalizedMessage();
            logger.error(errStr, e);
            asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdResponse.withPlainInternalServerError(getErrorResponse(errStr))));
          }
        }
      });
    } catch(Exception e) {
      String errStr = "Error running vertx on context: " + e.getLocalizedMessage();
      logger.error(errStr, e);
      asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdResponse.withPlainInternalServerError(getErrorResponse(errStr))));
    }
  }

  @Override
  public void putPermsUsersById(String userid, PermissionUser entity, Map<String,
          String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
       vertxContext.runOnContext(v -> {
         String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
         Criteria idCrit = new Criteria();
         idCrit.addField(ID_FIELD);
         idCrit.setOperation("=");
         idCrit.setValue(userid);
         try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).update(
                   TABLE_NAME_PERMSUSERS, entity, new Criterion(idCrit), true, putReply -> {
            try {
              if(putReply.failed()) {
                String errStr = "Error with put: " + putReply.cause().getLocalizedMessage();
                logger.error(errStr, putReply.cause());
                asyncResultHandler.handle(Future.succeededFuture(PutPermsUsersByIdResponse.withPlainInternalServerError(getErrorResponse(errStr))));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(PutPermsUsersByIdResponse.withJsonOK(entity)));
              }
            } catch(Exception e) {
              String errStr = "Error getting put reply: " + e.getLocalizedMessage();
              logger.error(errStr, e);
              asyncResultHandler.handle(Future.succeededFuture(PutPermsUsersByIdResponse.withPlainInternalServerError(getErrorResponse(errStr))));
            }
          });
         } catch(Exception e) {
           String errStr = "Error using Postgres instance: " + e.getLocalizedMessage();
           logger.error(errStr, e);
           asyncResultHandler.handle(Future.succeededFuture(PutPermsUsersByIdResponse.withPlainInternalServerError(getErrorResponse(errStr))));
         }
       });
    } catch(Exception e) {
      String errStr ="Error running vertx on context: " + e.getLocalizedMessage();
      logger.error(errStr, e);
      asyncResultHandler.handle(Future.succeededFuture(PutPermsUsersByIdResponse.withPlainInternalServerError(getErrorResponse(errStr))));
    }
  }

  @Override
  public void deletePermsUsersById(String userid, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext)
          throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria idCrit = new Criteria();
        idCrit.addField(ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(userid);
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).delete(
                  TABLE_NAME_PERMSUSERS, new Criterion(idCrit), deleteReply-> {
            if(deleteReply.failed()) {
              logger.error("deleteReply failed: " + deleteReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdResponse.withPlainInternalServerError("Internal server error")));
            } else {
              if(deleteReply.result().getUpdated() == 0) {
                asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdResponse.withPlainNotFound("Not found")));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdResponse.withPlainNoContent("")));
              }
            }
          });
        } catch(Exception e) {
          logger.error("Error using Postgres instance: " + e.getLocalizedMessage(), e);
          asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.error("Error running vertx on context: " + e.getLocalizedMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void getPermsUsersByIdPermissions(String id, String expanded,
          String full, String indexField, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        boolean fullBool, expandedBool;
        if(full == null || !full.equals("true")) { fullBool = false; } else { fullBool = true; }
        if(expanded == null || !expanded.equals("true")) { expandedBool = false; } else { expandedBool = true; }

        Future<PermissionNameListObject> pnloFuture = this.getPermissionsForUser(id, expandedBool, fullBool, indexField, tenantId, vertxContext);
        pnloFuture.setHandler(res -> {
          if(res.failed()) {
            String errStr = "Error from get reply: " + res.cause().getLocalizedMessage();
            logger.error(errStr, res.cause());
            asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdPermissionsResponse.withPlainInternalServerError(getErrorResponse(errStr))));
          } else {
            PermissionNameListObject pnlo = res.result();
            if(pnlo == null) { //404
              asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdPermissionsResponse.withPlainNotFound("No user found by id " + id)));
            } else {
              asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdPermissionsResponse.withJsonOK(pnlo)));
            }
          }
        });
      });
    } catch(Exception e) {
      String errStr = "Error running on vertx context: " + e.getLocalizedMessage();
      logger.debug(errStr);
      asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdPermissionsResponse.withPlainInternalServerError(getErrorResponse(errStr))));
    }
  }

  @Override
  public void postPermsUsersByIdPermissions(String id, String indexField, PermissionNameObject entity,
          Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        try {
          Criteria useridCrit = getIdCriteria(indexField, "=", id);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMSUSERS,
                  PermissionUser.class, new Criterion(useridCrit), true, false, getReply-> {
            try {
              if(getReply.failed()) {
                logger.error("Error checking for user: " + getReply.cause().getLocalizedMessage(), getReply.cause());
                asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsUsersByIdPermissionsResponse.withPlainInternalServerError("Internal server error")));
              } else {
                List<PermissionUser> userList = (List<PermissionUser>)getReply.result().getResults();
                if(userList.isEmpty()) {
                  asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByIdPermissionsResponse.withPlainBadRequest("User with id " + id + " does not exist")));
                } else {
                  //now we can actually add it
                  String permissionName = entity.getPermissionName();
                  PermissionUser user = userList.get(0);
                  if(user.getPermissions().contains(permissionName)) {
                    asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByIdPermissionsResponse
                        .withJsonUnprocessableEntity(ValidationHelper.createValidationErrorMessage(USER_NAME_FIELD, id,
                            "User with id " + id + " already has permission " + permissionName))));
                  } else {
                    user.getPermissions().add(permissionName);
                    try {
                    PostgresClient.getInstance(vertxContext.owner(), tenantId).update(TABLE_NAME_PERMSUSERS, user, new Criterion(useridCrit), true, putReply -> {
                      if(putReply.failed()) {
                        logger.error("Error attempting to update user: " + putReply.cause().getLocalizedMessage());
                        asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByIdPermissionsResponse.withPlainInternalServerError("Internal server error")));
                      } else {
                        asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByIdPermissionsResponse.withJsonOK(entity)));
                      }
                    });
                    } catch(Exception e) {
                      logger.error("Error using Postgres instance to update user: " + e.getLocalizedMessage());
                      asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByIdPermissionsResponse.withPlainInternalServerError("Internal server error")));
                    }
                  }
                }
              }
            } catch (Exception e) {
              logger.error("Error using Postgres instance to retrieve user: " + e.getLocalizedMessage(), e);
              asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByIdPermissionsResponse.withPlainInternalServerError("Internal server error")));
            }
          });
        } catch(Exception e) {
          logger.error("Error using Postgres instance to retrieve user: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByIdPermissionsResponse.withPlainInternalServerError("Internal server error")));
        }

      });
    } catch(Exception e) {
      logger.error("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersByIdPermissionsResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void deletePermsUsersByIdPermissionsByPermissionname(String permissionname,
          String id, String indexField, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        try {
          Criteria idCrit = getIdCriteria(indexField, "=", id);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMSUSERS,
                  PermissionUser.class, new Criterion(idCrit), true, false, getReply-> {
            if(getReply.failed()) {
              logger.error("Error checking for user: " + getReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdPermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
            } else {
              List<PermissionUser> userList = (List<PermissionUser>)getReply.result().getResults();
              if(userList.size() == 0) {
                asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdPermissionsByPermissionnameResponse.withPlainBadRequest("User with id " + id + " does not exist")));
              } else {
                //attempt to delete permission
                PermissionUser user = userList.get(0);
                if(!user.getPermissions().contains(permissionname)) {
                  asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdPermissionsByPermissionnameResponse.withPlainBadRequest("User with id " + id + " does not contain " + permissionname)));
                } else {
                  try {
                    user.getPermissions().remove(permissionname);
                    PostgresClient.getInstance(vertxContext.owner(), tenantId).update(TABLE_NAME_PERMSUSERS, user, new Criterion(idCrit), true, putReply -> {
                      if(putReply.failed()) {
                         logger.error("Error attempting to update user: " + putReply.cause().getLocalizedMessage());
                         asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdPermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
                      } else {
                        asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdPermissionsByPermissionnameResponse.withPlainNoContent("")));
                      }
                    });
                  } catch(Exception e) {
                    logger.error("Error using Postgres instance to delete permission from user");
                    asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdPermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
                  }
                }
              }
            }
          });
        } catch(Exception e) {
          logger.error("Error using Postgres instance to retrieve user: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdPermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.error("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdPermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
    }
  }


  @Override
  public void postPermsPermissions(PermissionUploadJson entity, Map<String, String> okapiHeaders,
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
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit), true, false, getReply-> {
            if(getReply.failed()) {
              logger.error("Error getting existing permissions: " + getReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
            } else {
              List<Permission> permissionList = (List<Permission>)getReply.result().getResults();
              if(permissionList.size() > 0) {
                asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsPermissionsResponse.withJsonUnprocessableEntity(
                  ValidationHelper.createValidationErrorMessage(
                    PERMISSION_NAME_FIELD, entity.getPermissionName(),
                    "Permission with name " + entity.getPermissionName() + " already exists"))));
                logger.debug("Permission with this name already exists");
              } else {
                //Do the actual POST of the new permission
                PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
                postgresClient.startTx(beginTx-> {
                  logger.debug("Attempting to save new Permission");
                  String newId = UUID.randomUUID().toString();
                  //entity.setAdditionalProperty("id", newId);
                  entity.setId(newId);
                  if(entity.getVisible() == null) {
                    entity.setVisible(true);
                  }
                  if(entity.getPermissionName() == null) {
                    entity.setPermissionName(newId);
                  }
                  try {
                    postgresClient.save(beginTx, TABLE_NAME_PERMS, entity, postReply -> {
                      if(postReply.failed()) {
                        logger.error("Unable to save new permission: " + postReply.cause().getLocalizedMessage());
                        asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
                      } else {
                        postgresClient.endTx(beginTx, done -> {
                          asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withJsonCreated(entity)));
                        });
                      }
                    });
                  } catch(Exception e) {
                    logger.error("Error with Postgres client while saving permission: " + e.getLocalizedMessage());
                    asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
                  }
                });
              }
            }
          });
        } catch(Exception e) {
          logger.error("Error calling Postgresclient: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.error("Error running vertx on context: " + e.getLocalizedMessage());
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
              logger.error("Error in getReply: " + getReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
            } else {
              List<Permission> permList = (List<Permission>)getReply.result().getResults();
              if(permList.size() < 1) {
                //404'd!
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withPlainNotFound("No permission with ID " + id + " exists")));
              } else if(permList.size() > 1) {
                //Too many results!
                logger.warn("Multiple results found for ID " + id);
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withJsonOK(permList.get(0))));
              }
            }
          });
        } catch(Exception e) {
          logger.error("Error getting Permission with Postgres client: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.error("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void putPermsPermissionsById(String id,
          PermissionUploadJson entity, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v-> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria idCrit = new Criteria();
        idCrit.addField(ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(entity.getId());
        if(entity.getId() == null || !entity.getId().equals(id)) {
          asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsPermissionsByIdResponse.withPlainBadRequest("Invalid id value")));
          return;
        }
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  TABLE_NAME_PERMS, Permission.class, new Criterion(idCrit),
                  true, false, getReply -> {
            if(getReply.failed()) {
              String message = "Error with get: " + getReply.cause().getLocalizedMessage();
              logger.error(message, getReply.cause());
              asyncResultHandler.handle(Future.succeededFuture(
                      PutPermsPermissionsByIdResponse.withPlainInternalServerError(getErrorResponse(message))));
            }
            List<Permission> permList = (List<Permission>)getReply.result().getResults();
            if(permList.size() < 1) {
              String message = "No permission found to match that id";
              asyncResultHandler.handle(Future.succeededFuture(
                      PutPermsPermissionsByIdResponse.withPlainNotFound(message)));
            } else {
              Permission perm = permList.get(0);
              if(!perm.getPermissionName().equals(entity.getPermissionName())) {
                asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainBadRequest("permission name property cannot change")));
              } else {
                try {
                  PostgresClient.getInstance(vertxContext.owner(), tenantId).update(TABLE_NAME_PERMS, entity, new Criterion(idCrit), true, putReply -> {
                    if(putReply.failed()) {
                      logger.error("Error with put: " + putReply.cause().getLocalizedMessage(), putReply.cause());
                      asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
                    } else {
                      asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withJsonOK(entity)));
                    }
                  });
                }
                catch(Exception e) {
                  logger.error("Error using Postgres instance: " + e.getLocalizedMessage());
                  asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
                }
              }
            }
          });
        } catch(Exception e) {
          logger.error("Error using Postgres instance: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.error("Error running vertx on context: " + e.getLocalizedMessage());
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
              logger.error("deleteReply failed: " + deleteReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(DeletePermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
            } else {
              if(deleteReply.result().getUpdated() == 0) {
                asyncResultHandler.handle(Future.succeededFuture(DeletePermsPermissionsByIdResponse.withPlainNotFound("Not found")));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(DeletePermsPermissionsByIdResponse.withPlainNoContent("")));
              }
            }
          });
        } catch(Exception e) {
          logger.error("Error using Postgres instance: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(DeletePermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
        }
      });
    } catch(Exception e) {
      logger.error("Error running vertx on context: " + e.getLocalizedMessage());
      asyncResultHandler.handle(Future.succeededFuture(DeletePermsPermissionsByIdResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  @Override
  public void getPermsPermissions(String expandSubs, int length, int start, String sortBy, String query,
          String memberOf, String ownedBy, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext)
          throws Exception {
    try {
      vertxContext.runOnContext(v -> {
        CQLWrapper cql;
        try {
          cql = getCQL(query, TABLE_NAME_PERMS, length, start-1);
        } catch(Exception e) {
          logger.error("Error parsing CQL: " + e.getLocalizedMessage());
          asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.withPlainBadRequest(
                        "CQL Parsing Error for '" + query + "': " + e.getLocalizedMessage())));
          return;
        }
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        String[] fieldList = {"*"};
        try {
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
                  Permission.class, fieldList, cql, true, false, getReply -> {
            try {
              if(getReply.succeeded()) {
                PermissionListObject permCollection = new PermissionListObject();
                List<Permission> permissions = (List<Permission>)getReply.result().getResults();
                List<Future> futureList = new ArrayList<>();
                for(Permission permission : permissions) {
                  List<Object> subPermList = permission.getSubPermissions();
                  Future<Permission> permFuture;
                  if(expandSubs != null && expandSubs.equals("true")) {
                    permFuture = expandSubPermissions(permission, vertxContext, tenantId);
                  } else {
                    permFuture = Future.succeededFuture(permission);
                  }
                  futureList.add(permFuture);
                }
                CompositeFuture compositeFuture = CompositeFuture.join(futureList);
                compositeFuture.setHandler(compositeResult -> {
                  if(compositeFuture.failed()) {
                    logger.error("Error expanding permissions: " + compositeFuture.cause().getLocalizedMessage());
                    asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.withPlainInternalServerError("Error getting expanded permissions: " + compositeResult.cause().getLocalizedMessage())));
                  } else {
                    List<Permission> newPermList = new ArrayList<>();
                    for(Future f : futureList) {
                      newPermList.add((Permission)(f.result()));
                    }
                    permCollection.setPermissions(newPermList);
                    permCollection.setTotalRecords(getReply.result().getResultInfo().getTotalRecords());
                    asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.withJsonOK(permCollection)));
                  }

                });
              } else {
                logger.error("Error with getReply: " + getReply.cause().getLocalizedMessage());
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.withPlainInternalServerError(getReply.cause().getLocalizedMessage())));
              }
            } catch (Exception e) {
              logger.error("Error getting Postgres client: " + e.getLocalizedMessage(), e);
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                      GetPermsPermissionsResponse.withPlainInternalServerError("Internal server error: " + e.getLocalizedMessage())));
            }
          });
        } catch(Exception e) {
          if(e.getCause() != null && e.getCause().getClass().getSimpleName().contains("CQLParseException")) {
                logger.error("BAD CQL:" + e.getLocalizedMessage());
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.withPlainBadRequest(
                        "CQL Parsing Error for '" + query + "': " + e.getLocalizedMessage())));
          } else {
            logger.error("Error getting Postgres client: " + e.getLocalizedMessage());
            asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                    GetPermsPermissionsResponse.withPlainInternalServerError("Internal server error: " + e.getLocalizedMessage())));
          }
        }
      });
    } catch(Exception e) {
      logger.error("Error running vertx on context: " + e.getLocalizedMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
    }
  }

  private Future<Boolean> checkPermissionExists(String permissionName, 
          Context vertxContext, String tenantId) {
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
              logger.error("Error in getReply: " + getReply.cause().getLocalizedMessage());
              future.fail(getReply.cause());
            } else {
              List<Permission> permList = (List<Permission>)getReply.result().getResults();
              if(permList.isEmpty()) {
                future.complete(Boolean.FALSE);
              } else {
                future.complete(Boolean.TRUE);
              }
            }
          });
        } catch(Exception e) {
          logger.error("Error from PostgresClient instance: " + e.getLocalizedMessage());
          future.fail(e);
        }
      });
    } catch(Exception e) {
      logger.error("Error running on vertx context: " + e.getLocalizedMessage());
      future.fail(e);
    }
    return future;
}

  /*
    Given a list of permissions, check to see if they all actually exist
  */
  private Future<Boolean> checkPermissionListExists(List<Object> permissionList,
          Context vertxContext, String tenantId) {
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

  private Future<List<String>> getAllExpandedPermissions(List<String> permissionList,
          Context vertxContext, String tenantId) {
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

  private Future<List<String>> getExpandedPermissions(String permissionName,
          Context vertxContext, String tenantId) {
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
              logger.error("Error in get request: " + getReply.cause().getLocalizedMessage());
              future.fail(getReply.cause());
            } else {
              List<Permission> permList = (List<Permission>)getReply.result().getResults();
              if(permList.isEmpty()) {
                 future.complete(new ArrayList<String>());
              } else {
                Permission permission = permList.get(0);
                if(!permission.getSubPermissions().isEmpty()) {
                  List<Future> futureList = new ArrayList<>();
                  for (Object subPermissionOb : permission.getSubPermissions()) {
                    String subPermissionName = null;
                    try {
                      subPermissionName = (String) subPermissionOb;
                      Future<List<String>> subPermFuture = getExpandedPermissions((String) subPermissionName, vertxContext, tenantId);
                      futureList.add(subPermFuture);
                    } catch (Exception e) {
                      String message = "Error getting string value of subpermissions from permission '"
                              + permission.getPermissionName() + "': " + e.getLocalizedMessage();
                      logger.error(message);
                      future.fail(message);
                      return;
                    }
                  }
                  CompositeFuture compositeFuture = CompositeFuture.all(futureList);
                  compositeFuture.setHandler(compRes -> {
                    if(compRes.failed()) {
                      logger.error("Error getting expanded permissions for '" + permissionName + "' : " + compRes.cause().getLocalizedMessage());
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
          logger.error("Error getting users from Postgres: " + e.getLocalizedMessage());
          future.fail(e);
        }
      });
    } catch(Exception e) {
      logger.error("Error running on vertx context: " + e.getLocalizedMessage());
      future.fail(e);
    }
    return future;
  }

  private Future<PermissionNameListObject> getAllFullPermissions(List<String> nameList,
          Context vertxContext, String tenantId) {
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
          if(result != null) {
            permList.add(result);
          }
        }
        pnlo.setPermissionNames(permList);
        future.complete(pnlo);
      }
    });
    return future;
  }

  private Future<Permission> getFullPermissions(String permissionName,
          Context vertxContext, String tenantId) {
   Future<Permission> future = Future.future();
   logger.debug("Getting full permissions for " + permissionName);
   try {
     vertxContext.runOnContext(v-> {
       Criteria nameCrit = new Criteria();
       nameCrit.addField(PERMISSION_NAME_FIELD);
       nameCrit.setOperation("=");
       nameCrit.setValue(permissionName);
       try {
        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
                 Permission.class, new Criterion(nameCrit), true, false, getReply -> {
          if (getReply.failed()) {
            logger.debug("postgres client 'get' failed: " + getReply.cause().getLocalizedMessage());
            future.fail(getReply.cause());
          } else {
            List<Permission> permList = (List<Permission>) getReply.result().getResults();
            if (permList.isEmpty()) {
              logger.debug("No permission object '" + permissionName + "' exists");
              //future.fail("No permission object found for name '" + permissionName + "'");
              future.complete(null);
            } else {
              logger.debug("Completing future for getFullPermissions for '" + permissionName + "'");
              future.complete(permList.get(0));
            }
          }
        });
       } catch(Exception e) {
        logger.error("Error from PostgresClient: " + e.getLocalizedMessage());
        future.fail(e);
       }
     });
   } catch(Exception e) {
     logger.error("Error running on vertx context: " + e.getLocalizedMessage());
     future.fail(e);
   }
   return future;
  }

  private Future<PermissionNameListObject> getPermissionsForUser(
    String userId,
    boolean expanded,
    boolean full,
    String indexField,
    String tenantId,
    Context vertxContext) {
    final Future<PermissionNameListObject> future = Future.future();
    try {
      Criteria idCrit = getIdCriteria(indexField, "=", userId);
      idCrit.setOperation("=");
      idCrit.setValue(userId);
      try {
        PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                TABLE_NAME_PERMSUSERS, PermissionUser.class, new Criterion(idCrit),
                true, false, getReply -> {
          if(getReply.failed()) {
            future.fail(getReply.cause());
          } else {
            List<PermissionUser> userList = (List<PermissionUser>)getReply.result().getResults();
            if(userList.isEmpty()) {
              future.complete(null);
              return;
            }
            Future<List<String>> interimFuture;
            List<String> permissionNameList = new ArrayList<>();
            for(Object perm : userList.get(0).getPermissions()) {
              if(perm != null) {
                permissionNameList.add((String)perm);
              }
            }
            if(!expanded) {
              interimFuture = Future.succeededFuture(permissionNameList);
            } else {
              interimFuture = getAllExpandedPermissions(permissionNameList,
                      vertxContext, tenantId);
            }
            interimFuture.setHandler(res -> {
              if(res.failed()) {
                future.fail(res.cause());
              } else {
                if(!full) {
                  PermissionNameListObject pnlo = new PermissionNameListObject();
                  List<Object> objectList = new ArrayList();
                  for(String s : res.result()) {
                    objectList.add(s);
                  }
                  pnlo.setPermissionNames(objectList);
                  pnlo.setTotalRecords(res.result().size());
                  future.complete(pnlo);
                } else {
                  getAllFullPermissions(res.result(), vertxContext, tenantId).setHandler(res2 -> {
                    if(res2.failed()) {
                      future.fail(res2.cause());
                    } else {
                      future.complete(res2.result());
                    }
                  });
                }
              }
            });
          }
        });
      } catch(Exception e) {
        future.fail(e);
      }
    } catch(Exception e) {
      future.fail(e);
    }
    return future;
  }

  private JsonObject parseTokenPayload(String token) {
    if(token == null) {
      return null;
    }
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

  private boolean allowAccessByNameorPermission(String permissions, String permissionName,
          String token, String username) {
    String tokenUsername = getUsername(token);
    if(tokenUsername != null && tokenUsername.equals(username)) {
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

  private Future<Permission> expandSubPermissions(Permission permission,
          Context vertxContext, String tenantId) {
    logger.debug("Expanding subPermissions for " + permission.getPermissionName());
    Future<Permission> future = Future.future();
    List<Object> subPerms = permission.getSubPermissions();
    if(subPerms.isEmpty()) {
      future.complete(permission);
    } else {
      List<Object> newSubPerms = new ArrayList<>();
      List<Future> futureList = new ArrayList<>();
      for(Object o : subPerms) {
        Future<Permission> subPermFuture = getFullPermissions((String)o, vertxContext, tenantId);
        futureList.add(subPermFuture);
      }
      CompositeFuture compositeFuture = CompositeFuture.join(futureList);
      compositeFuture.setHandler(compositeResult -> {
        if(compositeResult.failed()) {
          logger.error("Failed to expand subpermissions for '" + permission.getPermissionName() + "' : " + compositeResult.cause().getLocalizedMessage());
          future.fail(compositeResult.cause().getLocalizedMessage());
        } else {
          for(Future f : futureList) {
            if(f.result() != null) {
              newSubPerms.add(f.result());
            }
          }
          permission.setSubPermissions(newSubPerms);
          future.complete(permission);
        }
      });
    }

    return future;
  }
  
  /* If we are modifying (or creating) the subpermissions array of a permission
  object, check for any changes and for any newly declared subpermissions, add
  the permission name to the the 'childOf' field for those permisisons
  */
  private Future updateSubPermissions(String permissionName, JsonArray originalList,
          JsonArray newList, Context vertxContext, String tenantId) {
    Future future = Future.future();
    Future<Boolean> checkExistsFuture = checkPermissionListExists(newList.getList(),
            vertxContext, tenantId);
    checkExistsFuture.setHandler(res -> {
      if(res.failed()) {
        future.fail(res.cause());
      } else {
        
      }
    });
    return future;
  }

  private Future makeChildOf(String parentPermissionName, String childPermissionName,
          Context vertxContext, String tenantId) {
    Future future = Future.future();
    try {
      Criteria nameCrit = new Criteria()
              .addField(PERMISSION_NAME_FIELD)
              .setOperation("=")
              .setValue(childPermissionName);
      vertxContext.runOnContext( v -> {
        PostgresClient.getInstance(vertxContext.owner()).get(TABLE_NAME_PERMS,
                Permission.class, new Criterion(nameCrit), true, false,
                getReply -> {
          if(getReply.failed()) {
            future.fail(getReply.cause());
          } else {
            List<Permission> permList = (List<Permission>)getReply.result().getResults();
            if(permList.size() != 1) {
              future.fail("Expected one result for " + PERMISSION_NAME_FIELD + 
                      ": '" + childPermissionName + "', got " + permList.size() +
                      " results");
            } else {
              Permission childPermission = permList.indexOf(0);
              childPermission.
            }
          }
        });
      });
    }
    catch(Exception e) {
      future.fail(e);
    }
    return future;
  }
  
  private Criteria getIdCriteria(String indexField, String operation, String value)
          throws IllegalArgumentException, Exception {
    //Criteria crit = new Criteria(PERMISSION_SCHEMA_PATH);
    Criteria crit = new Criteria();
    if(indexField == null || indexField.equals("id")) {
      crit.addField(ID_FIELD);
    } else if(indexField.equals("userId")) {
      crit.addField(USER_ID_FIELD);
    } else {
      throw new IllegalArgumentException("Invalid value '" + indexField + "' for indexField");
    }
    crit.setOperation(operation);
    crit.setValue(value);
    return crit;
  }

}
