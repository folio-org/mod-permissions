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
  
  public enum Operation { ADD, DELETE };
  public enum PermissionField { CHILD_OF, GRANTED_TO };
  
  public static class InvalidPermissionsException extends Exception {
    public InvalidPermissionsException(String message) {
      super(message);
    }
  }
  
  public static class FieldUpdateValues {
    private String fieldValue;
    private String permissionName;
    private PermissionField field;
    private Operation operation;

    public FieldUpdateValues(String fieldValue, String permissionName,
            PermissionField field, Operation operation) {
      this.fieldValue = fieldValue;
      this.permissionName = permissionName;
      this.field = field;
      this.operation = operation;
    }

    public String getFieldValue() {
      return fieldValue;
    }

    public String getPermissionName() {
      return permissionName;
    }

    public PermissionField getField() {
      return field;
    }
    
    public Operation getOperation() {
      return operation;
    }    
  }

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
  
  private static CQLWrapper getCQL(String query, String tableName, int limit, int offset) throws FieldException{
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(tableName + ".jsonb");
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
  }
  
  private static CQLWrapper getCQL(String query, String tableName) throws FieldException{
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(tableName + ".jsonb");
    return new CQLWrapper(cql2pgJson, query);
  }

  private final Messages messages = Messages.getInstance();

  private String getErrorResponse(String response) {
    if(suppressErrorResponse) {
      return "Internal Server Error: Please contact Admin";
    }
    return response;
  }
    
  private void report(String noise) {

    logger.info(noise);
  }
  
  private static void report(String noise, Logger logger) {
    logger.info(noise);
  }

  @Override
  public void getPermsUsers(int length, int start, String sortBy, String query,
          String hasPermissions, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
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
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext)  {
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
                  postgresClient.save(beginTx, TABLE_NAME_PERMSUSERS, entity, postReply -> {
                    try {
                      if(postReply.succeeded()) {
                        final PermissionUser permUser = entity;
                        try {
                          updateUserPermissions(beginTx, permUser.getId(), new JsonArray(),
                            new JsonArray(permUser.getPermissions()), vertxContext,
                            tenantId, logger).setHandler(updatePermsRes -> {
                            if(updatePermsRes.failed()) {
                              postgresClient.rollbackTx(beginTx, rollbackTx -> {
                                logger.error("Error updating derived fields: " + updatePermsRes.cause());
                                if(updatePermsRes.cause() instanceof InvalidPermissionsException) {
                                   asyncResultHandler.handle(Future.succeededFuture(
                                    PostPermsUsersResponse.withJsonUnprocessableEntity(
                                      ValidationHelper.createValidationErrorMessage(
                                        ID_FIELD, permUser.getId(), getErrorResponse(
                                          "Unable to update derived fields: " + updatePermsRes.cause().getLocalizedMessage())))));
                                } else {
                                   asyncResultHandler.handle(Future.succeededFuture(
                                        PostPermsUsersResponse.withPlainInternalServerError(
                                                getErrorResponse(updatePermsRes.cause().getLocalizedMessage()))));
                                }    
                              });
                            } else {
                              postgresClient.endTx(beginTx, endTx -> {
                                asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.withJsonCreated(entity)));                                                   
                              });              
                            }
                          });
                        } catch(Exception e) {
                          postgresClient.rollbackTx(beginTx, rollbackTx -> {
                            logger.error(e.getLocalizedMessage());
                            if(e instanceof InvalidPermissionsException) {
                              asyncResultHandler.handle(Future.succeededFuture(
                                PostPermsUsersResponse.withJsonUnprocessableEntity(
                                  ValidationHelper.createValidationErrorMessage(
                                    ID_FIELD, permUser.getId(), getErrorResponse(
                                      "Unable to update derived fields: " + e.getLocalizedMessage())))));         
                            } else {
                              asyncResultHandler.handle(Future.succeededFuture(
                                        PostPermsUsersResponse.withPlainInternalServerError(                                                  
                                                getErrorResponse("Error updating permission derived fields: " + e.getLocalizedMessage()))));
                            }
                          });
                        }                            
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
          Context vertxContext)  {
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
          Context vertxContext) {
    try {
      vertxContext.runOnContext(v -> {
        try {
          String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
          Criteria idCrit = new Criteria();
          idCrit.addField(ID_FIELD);
          idCrit.setOperation("=");
          idCrit.setValue(userid);
          String query = "id==" + userid;
          CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMSUSERS);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMSUSERS,
                  PermissionUser.class, new Criterion(idCrit), true, false, getReply -> {
                    if (getReply.failed()) {
                      String errStr = getReply.cause().getLocalizedMessage();
                      logger.error(errStr, getReply.cause());
                      asyncResultHandler.handle(Future.succeededFuture(
                              PutPermsUsersByIdResponse.withPlainInternalServerError(
                                      getErrorResponse(errStr))));
                    } else {
                      List<PermissionUser> userList = (List<PermissionUser>) getReply.result().getResults();
                      if (userList.isEmpty()) {
                        asyncResultHandler.handle(Future.succeededFuture(
                                PutPermsUsersByIdResponse.withPlainNotFound(
                                        "No permissions user found with id " + userid)));
                      } else {
                        try {
                          PermissionUser originalUser = userList.get(0); 
                          PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
                          pgClient.startTx(beginTx -> {
                            pgClient.update(beginTx, TABLE_NAME_PERMSUSERS, entity,
                                    cqlFilter, true, updateReply -> {
                              if(updateReply.failed()) {
                                pgClient.rollbackTx(beginTx, done -> {
                                  String errStr = "Error with put: " 
                                          + updateReply.cause().getLocalizedMessage();
                                  logger.error(errStr, updateReply.cause());
                                  asyncResultHandler.handle(Future.succeededFuture(
                                          PutPermsUsersByIdResponse.withPlainInternalServerError(
                                          getErrorResponse(errStr))));
                                });
                              } else {
                                updateUserPermissions(beginTx, userid, 
                                        new JsonArray(originalUser.getPermissions()),
                                        new JsonArray(entity.getPermissions()),
                                        vertxContext, tenantId, logger).setHandler(
                                                updateUserPermsRes -> {
                                  if(updateUserPermsRes.failed()) {
                                    pgClient.rollbackTx(beginTx, done -> {
                                      if(updateUserPermsRes.cause() instanceof InvalidPermissionsException) {
                                        asyncResultHandler.handle(Future.succeededFuture(
                                              PutPermsUsersByIdResponse.withJsonUnprocessableEntity(
                                              ValidationHelper.createValidationErrorMessage(
                                              ID_FIELD, entity.getId(), getErrorResponse(
                                              "Unable to update derived fields: "
                                              + updateUserPermsRes.cause().getLocalizedMessage())))));
                                      } else {
                                        String errStr = "Error with derived field update: " + updateUserPermsRes.cause().getLocalizedMessage();
                                        logger.error(errStr, updateUserPermsRes.cause());
                                        asyncResultHandler.handle(Future.succeededFuture(
                                              PutPermsUsersByIdResponse.withPlainInternalServerError(
                                              getErrorResponse(errStr))));
                                      }
                                    });
                                  } else {
                                    //close Tx
                                    pgClient.endTx(beginTx, done -> {
                                      asyncResultHandler.handle(Future.succeededFuture(
                                            PutPermsUsersByIdResponse.withJsonOK(entity)));
                                    });
                                  }
                                });                                
                              }
                            });
                          });                        

                        } catch (Exception e) {
                          String errStr = "Error using Postgres instance: " + e.getLocalizedMessage();
                          logger.error(errStr, e);
                          asyncResultHandler.handle(Future.succeededFuture(
                                  PutPermsUsersByIdResponse.withPlainInternalServerError(
                                  getErrorResponse(errStr))));
                        }
                      }
                    }
                  });
        } catch (Exception e) {
          String errStr = "Error: " + e.getLocalizedMessage();
          logger.error(errStr, e);
          asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsUsersByIdResponse.withPlainInternalServerError(
                          getErrorResponse(errStr))));
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
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        Criteria idCrit = new Criteria();
        idCrit.addField(ID_FIELD);
        idCrit.setOperation("=");
        idCrit.setValue(userid);
        try {
          PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(),
                  tenantId);
          pgClient.startTx(connection -> {
            pgClient.get(TABLE_NAME_PERMSUSERS, PermissionUser.class,
                    new Criterion(idCrit), true, false, getReply -> {
              if(getReply.failed()) {
                //rollback
                pgClient.rollbackTx(connection, rollback -> {
                  String errStr = String.format("Error getting existing users: %s",
                          getReply.cause().getLocalizedMessage());
                  logger.error(errStr, getReply.cause());
                  asyncResultHandler.handle(Future.succeededFuture(
                      DeletePermsUsersByIdResponse.withPlainInternalServerError(
                      getErrorResponse(errStr))));
                });                
              } else {
                List<PermissionUser> permUsers = (List<PermissionUser>)getReply
                        .result().getResults();
                if(permUsers.size() < 1) {
                  //rollback, 404
                  pgClient.rollbackTx(connection, rollback -> {
                    asyncResultHandler.handle(Future.succeededFuture(
                            DeletePermsUsersByIdResponse.withPlainNotFound(
                            "Not found")));
                  });
                } else {
                  PermissionUser permUser = permUsers.get(0);
                  try {
                    updateUserPermissions(connection, userid, 
                            new JsonArray(permUser.getPermissions()), new JsonArray(),
                            vertxContext, tenantId, logger).setHandler(updateUserPermsRes -> {
                      if(updateUserPermsRes.failed()) {
                        pgClient.rollbackTx(connection, rollback -> {
                          String errStr = String.format("Error updating metadata: %s",
                                  updateUserPermsRes.cause().getLocalizedMessage());
                          logger.error(errStr, updateUserPermsRes.cause());
                          asyncResultHandler.handle(Future.succeededFuture(
                              DeletePermsUsersByIdResponse.withPlainInternalServerError(
                              getErrorResponse(errStr))));
                        });
                      } else {
                        pgClient.delete(connection, TABLE_NAME_PERMSUSERS, 
                                new Criterion(idCrit), deleteReply -> {
                          if(deleteReply.failed()) {
                            pgClient.rollbackTx(connection, rollback -> {
                              String errStr = String.format("Error deleting user: %s",
                                      deleteReply.cause().getLocalizedMessage());
                              logger.error(errStr, deleteReply.cause());
                              asyncResultHandler.handle(Future.succeededFuture(
                                      DeletePermsUsersByIdResponse.withPlainInternalServerError(
                                      getErrorResponse(errStr))));
                            });
                          } else {
                            if(deleteReply.result().getUpdated() == 0) {
                              pgClient.rollbackTx(connection, rollback -> {
                                asyncResultHandler.handle(Future.succeededFuture(
                                        DeletePermsUsersByIdResponse.withPlainNotFound(
                                        "Not found")));
                              });
                            } else {
                              pgClient.endTx(connection, done -> {
                                asyncResultHandler.handle(Future.succeededFuture(
                                        DeletePermsUsersByIdResponse.withPlainNoContent("")));
                              });
                            }
                          }
                        });
                      }
                    });
                  } catch(Exception e) {
                    //rollback
                    pgClient.rollbackTx(connection, rollback -> {
                      String errStr = String.format("Error deleting user: %s", 
                              e.getLocalizedMessage());
                      logger.error(errStr, e);
                      asyncResultHandler.handle(Future.succeededFuture(
                              DeletePermsUsersByIdResponse.withPlainInternalServerError(
                              getErrorResponse(errStr))));
                    });
                  }
                }
              }
            });
          });             
        } catch(Exception e) {
          String errStr = "Error using Postgres instance: " + e.getLocalizedMessage();
          logger.error(errStr, e);
          asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsUsersByIdResponse.withPlainInternalServerError(
                          getErrorResponse(errStr))));
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
          Context vertxContext) {
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
          Context vertxContext) {
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
                  asyncResultHandler.handle(Future.succeededFuture(
                          PostPermsUsersByIdPermissionsResponse.withPlainBadRequest("User with id " + id + " does not exist")));
                } else {
                  //now we can actually add it
                  String permissionName = entity.getPermissionName();
                  PermissionUser user = userList.get(0);
                  JsonArray originalPermissions = new JsonArray(
                          new ArrayList<Object>(user.getPermissions()));
                  if(user.getPermissions().contains(permissionName)) {
                    asyncResultHandler.handle(Future.succeededFuture(
                            PostPermsUsersByIdPermissionsResponse.withJsonUnprocessableEntity(
                            ValidationHelper.createValidationErrorMessage(USER_NAME_FIELD, id,
                            "User with id " + id + " already has permission " + permissionName))));
                  } else {
                    user.getPermissions().add(permissionName);
                    try {
                      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
                      String query = String.format("id==%s", id);
                      CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMSUSERS);
                      pgClient.startTx(connection -> {
                        pgClient.update(connection, TABLE_NAME_PERMSUSERS, user,
                                cqlFilter, true, putReply -> {
                          if(putReply.failed()) {
                            //rollback
                            pgClient.rollbackTx(connection, rollback -> {
                              String errStr = String.format("Error attempting to update user: %s",
                                      putReply.cause().getLocalizedMessage());
                              logger.error(errStr, putReply.cause());
                              asyncResultHandler.handle(Future.succeededFuture(
                                      PostPermsUsersByIdPermissionsResponse
                                      .withPlainInternalServerError(getErrorResponse(errStr))));
                            });
                          } else {
                            //update metadata
                            updateUserPermissions(connection, id, originalPermissions,
                                    new JsonArray(user.getPermissions()), vertxContext,
                                    tenantId, logger).setHandler(updateUserPermsRes -> {
                              if(updateUserPermsRes.failed()) {
                                //rollback
                                pgClient.rollbackTx(connection, rollback -> {
                                  String errStr = String.format(
                                      "Error attempting to update permissions metadata: %s",
                                      updateUserPermsRes.cause().getLocalizedMessage());
                                  logger.error(errStr, updateUserPermsRes.cause());
                                  asyncResultHandler.handle(Future.succeededFuture(
                                          PostPermsUsersByIdPermissionsResponse
                                          .withPlainInternalServerError(getErrorResponse(errStr))));
                                });
                              } else {
                                //close the transaction
                                pgClient.endTx(connection, done -> {
                                  asyncResultHandler.handle(Future.succeededFuture(
                                          PostPermsUsersByIdPermissionsResponse.withJsonOK(entity)));
                                });
                              }
                            });
                          }
                        });
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
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext)  {
    try {
      vertxContext.runOnContext(v -> {
        String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
        try {
          Criteria idCrit = getIdCriteria(indexField, "=", id);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMSUSERS,
                  PermissionUser.class, new Criterion(idCrit), true, false, getReply-> {
            if(getReply.failed()) {
              logger.error("Error checking for user: " + getReply.cause().getLocalizedMessage());
              asyncResultHandler.handle(Future.succeededFuture(
                      DeletePermsUsersByIdPermissionsByPermissionnameResponse.withPlainInternalServerError("Internal server error")));
            } else {
              List<PermissionUser> userList = (List<PermissionUser>)getReply.result().getResults();
              if(userList.size() == 0) {
                asyncResultHandler.handle(Future.succeededFuture(
                        DeletePermsUsersByIdPermissionsByPermissionnameResponse.withPlainBadRequest("User with id " + id + " does not exist")));
              } else {
                //attempt to delete permission
                PermissionUser user = userList.get(0);
                if(!user.getPermissions().contains(permissionname)) {
                  asyncResultHandler.handle(Future.succeededFuture(DeletePermsUsersByIdPermissionsByPermissionnameResponse.withPlainBadRequest("User with id " + id + " does not contain " + permissionname)));
                } else {
                  try {
                    String query = String.format("id==%s", id);
                    CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMSUSERS);
                    JsonArray originalPermissions = new JsonArray(
                            new ArrayList<Object>(user.getPermissions()));
                    user.getPermissions().remove(permissionname);
                    PostgresClient pgClient = PostgresClient.getInstance(
                            vertxContext.owner(), tenantId);
                    pgClient.startTx(connection -> {
                      pgClient.update(connection, TABLE_NAME_PERMSUSERS, user,
                              cqlFilter, true, putReply -> {
                        if(putReply.failed()) {
                          pgClient.rollbackTx(connection, rollback -> {
                            String errStr = String.format(
                                    "Error attempting to delete permission '%s' from user: %s",
                                    permissionname,
                                    putReply.cause().getLocalizedMessage());
                            logger.error(errStr, putReply.cause());
                            asyncResultHandler.handle(Future.succeededFuture(
                                    DeletePermsUsersByIdPermissionsByPermissionnameResponse
                                    .withPlainInternalServerError(getErrorResponse(errStr))));
                          });
                        } else {
                          updateUserPermissions(connection, user.getId(), originalPermissions,
                                  new JsonArray(user.getPermissions()), vertxContext,
                                  tenantId, logger).setHandler(updateUserPermsRes -> {
                            if(updateUserPermsRes.failed()) {
                              pgClient.rollbackTx(connection, rollback -> {
                                String errStr = String.format(
                                        "Error attempting to update permission metadata: %s",
                                        updateUserPermsRes.cause().getLocalizedMessage());
                                logger.error(errStr, updateUserPermsRes.cause());
                                asyncResultHandler.handle(Future.succeededFuture(
                                        DeletePermsUsersByIdPermissionsByPermissionnameResponse
                                        .withPlainInternalServerError(getErrorResponse(errStr))));
                              });
                            } else {
                              pgClient.endTx(connection, done -> {
                                asyncResultHandler.handle(Future.succeededFuture(
                                DeletePermsUsersByIdPermissionsByPermissionnameResponse
                                        .withPlainNoContent("")));
                              });
                            }
                          });
                        }
                      });
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
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
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
                        postgresClient.rollbackTx(beginTx, done -> {
                          logger.error("Unable to save new permission: " + postReply.cause().getLocalizedMessage());
                          asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withPlainInternalServerError("Internal server error")));
                        });
                      } else {
                        updateSubPermissions(beginTx, entity.getPermissionName(), new JsonArray(),
                                new JsonArray(entity.getSubPermissions()), vertxContext,
                                        tenantId, logger).setHandler(updateSubPermsRes -> {
                          if(updateSubPermsRes.failed()) {
                            postgresClient.rollbackTx(beginTx, done -> {
                              asyncResultHandler.handle(Future.succeededFuture(
                                PostPermsPermissionsResponse.withJsonUnprocessableEntity(
                                ValidationHelper.createValidationErrorMessage(
                                  PERMISSION_NAME_FIELD, entity.getPermissionName(),
                                  updateSubPermsRes.cause().getLocalizedMessage()))));
                            });
                          } else {
                            postgresClient.endTx(beginTx, done -> {
                              asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.withJsonCreated(entity)));
                            });
                          }
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
  public void getPermsPermissionsById(String id, Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) {
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
  public void putPermsPermissionsById(String id,  PermissionUploadJson entity,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String query = "id==" + id;
      CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMS);
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
              Permission updatePerm = getRealPermObject(entity);
              updatePerm.setId(entity.getId());   
              updatePerm.setChildOf(perm.getChildOf());
              updatePerm.setGrantedTo(perm.getGrantedTo());
              if(!perm.getPermissionName().equals(entity.getPermissionName())) {
                asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse.withPlainBadRequest("permission name property cannot change")));
              } else {
                try {
                  PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
                  pgClient.startTx(beginTx-> {
                    pgClient.update(beginTx, TABLE_NAME_PERMS, updatePerm,
                            cqlFilter, true, putReply -> {
                      if(putReply.failed()) {
                        pgClient.rollbackTx(beginTx, done -> {
                          logger.error("Error with put: " + putReply.cause().getLocalizedMessage(), putReply.cause());
                          asyncResultHandler.handle(Future.succeededFuture(
                                  PutPermsPermissionsByIdResponse.withPlainInternalServerError(
                                  "Internal server error")));
                        });
                      } else {
                        updateSubPermissions(beginTx, entity.getPermissionName(),
                                  new JsonArray(perm.getSubPermissions()),
                                  new JsonArray(entity.getSubPermissions()), 
                                  vertxContext, tenantId, logger)
                                    .setHandler(updateSubPermsRes -> {
                          if(updateSubPermsRes.failed()) {
                            pgClient.rollbackTx(beginTx, done -> {
                              if (updateSubPermsRes.cause() instanceof InvalidPermissionsException) {
                                asyncResultHandler.handle(Future.succeededFuture(
                                        PutPermsPermissionsByIdResponse.withJsonUnprocessableEntity(
                                        ValidationHelper.createValidationErrorMessage(
                                        ID_FIELD, entity.getId(), getErrorResponse(
                                        "Unable to update derived fields: "
                                        + updateSubPermsRes.cause().getLocalizedMessage())))));
                              } else {
                                String errStr = "Error with derived field update: "
                                        + updateSubPermsRes.cause().getLocalizedMessage();
                                logger.error(errStr, updateSubPermsRes.cause());
                                asyncResultHandler.handle(Future.succeededFuture(
                                        PutPermsPermissionsByIdResponse.withPlainInternalServerError(
                                                getErrorResponse(errStr))));
                              }
                            });
                          } else {
                            //close connection
                            pgClient.endTx(beginTx, done -> {
                              asyncResultHandler.handle(Future.succeededFuture(
                                      PutPermsPermissionsByIdResponse.withJsonOK(entity)));
                            });
                          }
                        });
                      }
                    });
                  });
                } catch(Exception e) {
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
          Context vertxContext) {
    try {
      vertxContext.runOnContext(v -> {
        try {
          String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_TENANT_HEADER));
          Criteria idCrit = new Criteria();
          idCrit.addField(ID_FIELD);
          idCrit.setOperation("=");
          idCrit.setValue(id);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
                  Permission.class, new Criterion(idCrit), true, false, getReply -> {
            if(getReply.failed()) {
              String errStr = getReply.cause().getLocalizedMessage();
              logger.error(errStr, getReply.cause());
               asyncResultHandler.handle(Future.succeededFuture(
                       DeletePermsPermissionsByIdResponse.withPlainInternalServerError(
                               getErrorResponse(errStr))));
            } else {
              List<Permission> permList = (List<Permission>)getReply.result().getResults();
              if(permList.isEmpty()) {
                asyncResultHandler.handle(Future.succeededFuture(
                        DeletePermsPermissionsByIdResponse
                        .withPlainNotFound("Not found")));
              } else {
                Permission perm = permList.get(0);
                if(!perm.getChildOf().isEmpty() || !perm.getGrantedTo().isEmpty()) {
                  PostgresClient pgClient = PostgresClient.getInstance(
                          vertxContext.owner(), tenantId);
                  pgClient.startTx(connection -> {
                    List<String> parentPermissionList = new ArrayList<>();
                    for(Object ob : perm.getChildOf()) {
                      parentPermissionList.add((String)ob);
                    }
                    List<String> userIdList = new ArrayList<>();
                    for(Object ob : perm.getGrantedTo()) {
                      userIdList.add((String)ob);
                    }
                    removePermissionFromUserList(connection, perm.getPermissionName(),
                            userIdList, vertxContext, tenantId).setHandler(rpfulRes -> {
                      if(rpfulRes.failed()) {
                        //rollback
                        pgClient.rollbackTx(connection, rollback -> {
                          String errStr = rpfulRes.cause().getLocalizedMessage();
                          logger.error(errStr, rpfulRes.cause());
                          asyncResultHandler.handle(Future.succeededFuture(
                                  DeletePermsPermissionsByIdResponse.withPlainInternalServerError(
                                  getErrorResponse(errStr))));
                        });
                      } else {
                        removeSubpermissionFromPermissionList(connection, 
                                perm.getPermissionName(), parentPermissionList,
                                vertxContext, tenantId).setHandler(rsfplRes-> {
                          if(rsfplRes.failed()) {
                            pgClient.rollbackTx(connection, rollback -> {
                              String errStr = rsfplRes.cause().getLocalizedMessage();
                              logger.error(errStr, rsfplRes.cause());
                              asyncResultHandler.handle(Future.succeededFuture(
                                      DeletePermsPermissionsByIdResponse.withPlainInternalServerError(
                                      getErrorResponse(errStr))));
                            });                           
                          } else {
                            pgClient.delete(connection, TABLE_NAME_PERMS,
                                    new Criterion(idCrit), deleteReply -> {
                              if(deleteReply.failed()) {
                                //rollback
                                pgClient.rollbackTx(connection, rollback -> {
                                  String errStr = deleteReply.cause().getLocalizedMessage();
                                  logger.error(errStr, deleteReply.cause());
                                  asyncResultHandler.handle(Future.succeededFuture(
                                          DeletePermsPermissionsByIdResponse.withPlainInternalServerError(
                                          getErrorResponse(errStr))));
                                });
                              } else {
                                //close tx
                                pgClient.endTx(connection, done -> {
                                  asyncResultHandler.handle(Future.succeededFuture(
                                          DeletePermsPermissionsByIdResponse
                                          .withPlainNoContent("")));
                                });
                              }
                            });              
                          }  
                        });
                      }
                    });
                  });
                } else {
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
                }
              }
            }
          });           
        } catch(Exception e) {
          String errStr = e.getLocalizedMessage();
          logger.error(errStr, e);
           asyncResultHandler.handle(Future.succeededFuture(
                   DeletePermsPermissionsByIdResponse.withPlainInternalServerError(
                           getErrorResponse(errStr))));
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
          Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
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

  private static Future<Boolean> checkPermissionExists(Object connection, 
          String permissionName, Context vertxContext, String tenantId) {
    Logger logger = LoggerFactory.getLogger(PermsAPI.class);
    Future<Boolean> future = Future.future();
    try {
      vertxContext.runOnContext(v -> {
        Criteria nameCrit = new Criteria();
        nameCrit.addField(PERMISSION_NAME_FIELD);
        nameCrit.setOperation("=");
        nameCrit.setValue(permissionName);
        try {
          report("Initiating PG Client get() request (in transaction)(cPE)", logger);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(connection,
                  TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit),
                  true, false, getReply -> {
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

  
  private static Future<Boolean> checkPermissionListExists(Object connection,
          List<Object> permissionList, Context vertxContext, String tenantId) {
    Future<Boolean> future = Future.future();
    if(permissionList.isEmpty()) {
      return Future.succeededFuture(true);
    }
    List<Object> permissionListCopy = new ArrayList<>(permissionList);
    Future<Boolean> checkPermissionExistsFuture;
    String permissionName = (String)permissionListCopy.get(0);
    permissionListCopy.remove(0); //pop
    checkPermissionExistsFuture = checkPermissionExists(connection, 
            permissionName, vertxContext, tenantId);
    checkPermissionExistsFuture.setHandler(res -> {
      if(res.failed()) {
        future.fail(res.cause());
      } else {
        future.complete(res.result());
      }
    });
    return future.compose( mapper -> { 
      if(!mapper) {
        return (Future<Boolean>)Future.succeededFuture(false);
      } else {
        return checkPermissionListExists(connection, permissionListCopy, 
                vertxContext, tenantId);
      }
    });
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
  
  /* If we are modifying a permissions user or creating a new one, we need to 
  check for any changes to the permissions list. For any changes, we need to 
  add or delete from the permission's "grantedTo" field
  */
  protected static Future<Void> updateUserPermissions(Object connection, String permUserId,
          JsonArray originalList, JsonArray newList, Context vertxContext,
          String tenantId, Logger logger) {
    Future<Void> future = Future.future();
    JsonArray missingFromOriginalList = new JsonArray();
    JsonArray missingFromNewList = new JsonArray();
    logger.info("Updating grantedTo fields pertaining to permissions user '" + permUserId +
              "' for old listing " + originalList.encode() +
              " and for new listing " + newList.encode());
    for(Object ob : newList) { 
      if(!originalList.contains(ob)) { missingFromOriginalList.add(ob); }
    }
    for(Object ob : originalList) {
      if(!newList.contains(ob)) { missingFromNewList.add(ob); }
    }
    Future<Boolean> checkExistsFuture = checkPermissionListExists(
            connection, missingFromOriginalList.getList(), vertxContext, tenantId);
    checkExistsFuture.setHandler(checkExistsRes -> {
      if(checkExistsFuture.failed()) {
        future.fail(checkExistsFuture.cause());
      } else if(!checkExistsFuture.result()) {
        future.fail(
                new InvalidPermissionsException(
                        "Attempting to add non-existent permissions to user"));
      } else {
        List<FieldUpdateValues> fuvList = new ArrayList<>();
        for(Object permissionNameOb : missingFromOriginalList) {
          FieldUpdateValues fuv = new FieldUpdateValues(permUserId, 
                  (String) permissionNameOb,
                  PermissionField.GRANTED_TO,
                  Operation.ADD);
          fuvList.add(fuv);
        }
        
        for(Object permissionNameOb : missingFromNewList) {
          FieldUpdateValues fuv = new FieldUpdateValues(permUserId,
                  (String)permissionNameOb,
                  PermissionField.GRANTED_TO,
                  Operation.DELETE);
          fuvList.add(fuv);
        }
        if(fuvList.isEmpty()) {
          future.complete(); //Nuthin' to do
        } else {
          modifyPermissionArrayFieldList(connection, fuvList, vertxContext,
                  tenantId, logger).setHandler(res -> {
            if(res.failed()) {
              future.fail(res.cause());
            } else {
              future.complete();
            }
          });
        }
      }
    });
    
    return future;
  }
  
  
  /* If we are modifying (or creating) the subpermissions array of a permission
  object, check for any changes and for any newly declared subpermissions, add
  the permission name to the the 'childOf' field for those permisisons
  */
  protected static Future<Void> updateSubPermissions(Object connection, 
          String permissionName, JsonArray originalList, JsonArray newList,
          Context vertxContext, String tenantId, Logger logger) {
    Future<Void> future = Future.future();
    try {
      logger.info("Updating childOf fields pertaining to permission '" + permissionName +
              "' for old listing " + originalList.encode() +
              " and for new listing " + newList.encode());
      JsonArray missingFromOriginalList = new JsonArray();
      JsonArray missingFromNewList = new JsonArray();
      for(Object ob : newList) { 
        if(!originalList.contains(ob)) { missingFromOriginalList.add(ob); }
      }
      for(Object ob : originalList) {
        if(!newList.contains(ob)) { missingFromNewList.add(ob); }
      }
      Future<Boolean> checkExistsFuture = checkPermissionListExists(connection,
              missingFromOriginalList.getList(), vertxContext, tenantId);
      checkExistsFuture.setHandler(res -> {
        if(res.failed()) {
          future.fail(res.cause());
        } else if(!res.result()) {
          future.fail(new InvalidPermissionsException(
                  "Attempting to add non-existent permissions as sub-permisisons"));
        } else {
          List<FieldUpdateValues> fuvList = new ArrayList<>();
          for(Object childPermissionNameOb : missingFromOriginalList) {
            FieldUpdateValues fuv = new FieldUpdateValues(
                    (String)permissionName,
                    (String)childPermissionNameOb, 
                    PermissionField.CHILD_OF,
                    Operation.ADD);
            fuvList.add(fuv);
          }
          
          for(Object childPermissionNameOb : missingFromNewList) {
            FieldUpdateValues fuv = new FieldUpdateValues(
                    (String)permissionName,
                    (String)childPermissionNameOb, 
                    PermissionField.CHILD_OF,
                    Operation.DELETE);
            fuvList.add(fuv);
          }
      
          if(fuvList.isEmpty()) {
            future.complete();
          } else {
            modifyPermissionArrayFieldList(connection, fuvList, vertxContext,
                    tenantId, logger).setHandler(res2 -> {
              if(res2.failed()) {
                future.fail(res2.cause());
              } else {
                future.complete();
              }
            });
          }
        }
      });
    } catch(Exception e) {
      future.fail(e);
    }
    return future;
  }

  private static Future<Void> modifyPermissionArrayField(Object connection, String fieldValue,
          String permissionName, PermissionField field, Operation operation,
          Context vertxContext, String tenantId, Logger logger) {
    Future<Void> future = Future.future();
    try {
      Criteria nameCrit = new Criteria()
              .addField(PERMISSION_NAME_FIELD)
              .setOperation("=")
              .setValue(permissionName);
      String query = "permissionName==" + permissionName;
      CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMS);
      vertxContext.runOnContext(v -> {
        try {
          report("Initiating PG Client get() request (in transaction)(mPAF)", logger);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  connection, TABLE_NAME_PERMS,
                  Permission.class, new Criterion(nameCrit), true, false,
                  getReply -> {
            if(getReply.failed()) {
              future.fail(getReply.cause());
            } else {
              List<Permission> permList = (List<Permission>)getReply.result().getResults();
              if(permList.size() != 1) {
                future.fail("Expected one result for " + PERMISSION_NAME_FIELD + 
                        ": '" + permissionName + "', got " + permList.size() +
                        " results");
              } else {
                Permission permission = permList.get(0);
                List valueList;
                if(field == PermissionField.CHILD_OF) {
                  valueList = permission.getChildOf();
                } else  {
                  valueList = permission.getGrantedTo();
                }
                logger.info("Performing " + operation.toString()
                  + " operation on " + field.toString() + " of permission " +
                        permissionName + " with value " + fieldValue);
                boolean modified = false;
                if(operation == Operation.ADD) {
                  if(!valueList.contains(fieldValue)) {
                    valueList.add(fieldValue);
                    modified = true;
                  }
                } else {
                  if(valueList.contains(fieldValue)) {
                    valueList.remove(fieldValue);
                    modified = true;
                  }
                }
                if(modified) {
                  try {
                    report("Initiating PG Client update() request (in transaction)(mPFAF)", logger);
                    PostgresClient.getInstance(vertxContext.owner(), tenantId).update(
                            connection, TABLE_NAME_PERMS, permission, cqlFilter,
                            true, updateReply -> {
                        if(updateReply.failed()) {
                          future.fail(updateReply.cause());
                        } else {
                          future.complete();
                        }
                      });
                    } catch(Exception e) {
                      future.fail(e);
                    }
                } else {
                  future.complete(); //Nothing more to do, no modification to list
                }
              }
            }
          });
        }
        catch(Exception e) {
          future.fail(e);         
        }
      });
    }
    catch(Exception e) {
      future.fail(e);
    }
    return future;
  }
  
  private static Future<Void> modifyPermissionArrayFieldList(Object connection,
          List<FieldUpdateValues> fuvList, Context vertxContext, String tenantId,
          Logger logger) {
    if(fuvList.isEmpty()) {
      return Future.succeededFuture();
    }
    Future future = Future.future();
    FieldUpdateValues fuv = fuvList.get(0);
    List<FieldUpdateValues> fuvListCopy = new ArrayList<>(fuvList);
    fuvListCopy.remove(0); //pop
    Future<Void> modifyPermArrayFieldFuture = modifyPermissionArrayField(connection,
            fuv.getFieldValue(), fuv.getPermissionName(), fuv.getField(),
            fuv.getOperation(), vertxContext, tenantId, logger);
    modifyPermArrayFieldFuture.setHandler(res -> {
      if(res.failed()) {
        future.fail(res.cause());
      } else {
        future.complete();
      }
    });
    return future.compose(mapper -> {
      return modifyPermissionArrayFieldList(connection,
            fuvListCopy, vertxContext, tenantId, logger);
    });
  }
  
  private Future<Void> removePermissionFromUserList(Object connection,
          String permissionName, List<String> userIdList, Context vertxContext,
          String tenantId) {
    if(userIdList.isEmpty()) {
      return Future.succeededFuture();
    }
    List<String> userIdListCopy = new ArrayList<>(userIdList);
    Future future = Future.future();
    String userId = userIdListCopy.get(0);
    userIdListCopy.remove(0);
    removePermissionFromUser(connection, permissionName, userId, vertxContext,
            tenantId).setHandler(rpfuRes -> {
      if(rpfuRes.failed()) {
        future.fail(rpfuRes.cause()); 
      } else {
        future.complete();
      }                   
    });
    return future.compose(res -> {
      return removePermissionFromUserList(connection, permissionName, userIdListCopy,
              vertxContext, tenantId);
    });
  }
  
  private Future<Void> removePermissionFromUser(Object connection, String permissionName,
          String userId, Context vertxContext, String tenantId) {
    Future future = Future.future();
    String query = String.format("id==%s", userId);
    Criteria idCrit = new Criteria()
            .addField(ID_FIELD)
            .setOperation("=")
            .setValue(userId);
    try {
      CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMSUSERS);
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(),
              tenantId);
      report("Initiating get() (in transaction) (removePermissionFromUser)");
      pgClient.get(connection, TABLE_NAME_PERMSUSERS, PermissionUser.class,
              new Criterion(idCrit), true, false, getReply -> {
        if(getReply.failed()) {
          future.fail(getReply.cause());
        } else {
          List<PermissionUser> permUserList = 
                  (List<PermissionUser>)getReply.result().getResults();
          if(permUserList.isEmpty()) {
            future.complete(); //No need to update non-existent user
          } else {
            PermissionUser user = permUserList.get(0);
            if(!user.getPermissions().contains(permissionName)) {
              future.complete(); //User already lacks the permissions
            } else {
              user.getPermissions().remove(permissionName);
              report("Initiating update() (in transaction) (removePermissionFromUser)");
              pgClient.update(connection, TABLE_NAME_PERMSUSERS, user, cqlFilter,
                      true, updateReply -> {
                if(updateReply.failed()) {
                  future.fail(updateReply.cause());
                } else {
                  future.complete();
                }
              });
            }
          }
        }
      });
    } catch(Exception e) {
      future.fail(e);
    }
    return future;
  }
  
  private Future<Void> removeSubpermissionFromPermissionList(Object connection,
          String subpermissionName, List<String> permissionNameList, 
          Context vertxContext, String tenantId) {
    if(permissionNameList.isEmpty()) {
      return Future.succeededFuture();
    }
    List<String> permissionNameListCopy = new ArrayList<>(permissionNameList);
    String permissionName = permissionNameListCopy.get(0);
    permissionNameListCopy.remove(0);
    Future future = Future.future();
    removeSubpermissionFromPermission(connection, subpermissionName, permissionName,
            vertxContext, tenantId).setHandler(rsfpRes -> {
      if(rsfpRes.failed()) {
        future.fail(rsfpRes.cause());
      } else {
        future.complete();
      }
    });
    return future.compose(res -> {
      return removeSubpermissionFromPermissionList(connection, subpermissionName,
              permissionNameListCopy, vertxContext, tenantId);
    });
  }
  
  private Future<Void> removeSubpermissionFromPermission(Object connection,
          String subpermissionName, String permissionName, Context vertxContext,
          String tenantId) {
    Future future = Future.future();
    try {
      Criteria nameCrit = new Criteria()
              .addField(PERMISSION_NAME_FIELD)
              .setOperation("=")
              .setValue(permissionName);
      String query = String.format("permissionName==%s", permissionName);
      CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMS);
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(),
              tenantId);
      report("Initiating get() (in transaction) (removeSubpermissionFromPermission");
      pgClient.get(connection, TABLE_NAME_PERMS, Permission.class,
              new Criterion(nameCrit), true, false, getReply-> {
        if(getReply.failed()) {
          future.fail(getReply.cause());
        } else {
          Permission permission = null;
          try {
            permission = ((List<Permission>)getReply.result().getResults()).get(0);
            permission.getSubPermissions().remove(subpermissionName);
          } catch(Exception e) {
            future.fail(String.format("Unable to get permission with name %s: %s",
                    permissionName, e.getLocalizedMessage()));
            return;
          }
          report("Initiating update() (in transaction) (removeSubpermissionFromPermission");
          pgClient.update(connection, TABLE_NAME_PERMS, permission, cqlFilter,
                  true,  updateReply -> {
            if(updateReply.failed()) {
              future.fail(updateReply.cause());
            } else {
              future.complete();
            }
          });
        }
      });
    } catch(Exception e) {
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
  
  private Permission getRealPermObject(PermissionUploadJson entity) {
    Permission perm = new Permission();
    perm.setId(entity.getId());
    perm.setPermissionName(entity.getPermissionName());
    perm.setDisplayName(entity.getDisplayName());
    perm.setDescription(entity.getDescription());
    List<Object> subPerms = new ArrayList<>();
    for(String s : entity.getSubPermissions()) {
      subPerms.add(s);
    }
    perm.setSubPermissions(subPerms);
    perm.setMutable(entity.getMutable());
    perm.setVisible(entity.getVisible());
    perm.setTags(entity.getTags());
    return perm;
  }

}
