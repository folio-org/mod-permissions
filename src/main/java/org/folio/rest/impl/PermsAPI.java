package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonArray;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.web.RoutingContext;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.ws.rs.core.Response;
import java.util.Objects;
import java.util.stream.Collectors;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.cql2pgjson.exception.FieldException;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.PermissionListObject;
import org.folio.rest.jaxrs.model.PermissionNameListObject;
import org.folio.rest.jaxrs.model.PermissionNameObject;
import org.folio.rest.jaxrs.model.PermissionUser;
import org.folio.rest.jaxrs.model.PermissionUpload;
import org.folio.rest.jaxrs.resource.Perms;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.SQLConnection;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.persist.interfaces.Results;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.tools.utils.ValidationHelper;

/**
 * @author kurt
 */
@SuppressWarnings("java:S3740")
public class PermsAPI implements Perms {

  public enum Operation {
    ADD, DELETE
  }

  public enum PermissionField {
    CHILD_OF, GRANTED_TO
  }

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
  private static final String USER_NAME_FIELD = "'username'";
  private static final String USER_ID_FIELD = "'userId'";
  private static final String ID_FIELD = "id";
  private static final String UNABLE_TO_UPDATE_DERIVED_FIELDS = "Unable to update derived fields: ";
  protected static final String PERMISSION_NAME_FIELD = "'permissionName'";
  private static final Logger logger = LoggerFactory.getLogger(PermsAPI.class);

  private static CQLWrapper getCQL(String query, String tableName, int limit, int offset) throws FieldException {
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(tableName + ".jsonb");
    return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
  }

  private static CQLWrapper getCQL(String query, String tableName) throws FieldException {
    CQL2PgJSON cql2pgJson = new CQL2PgJSON(tableName + ".jsonb");
    return new CQLWrapper(cql2pgJson, query);
  }

  @Validate
  @Override
  public void getPermsUsers(int length, int start, String sortBy, String query,
                            String hasPermissions, RoutingContext routingContext, Map<String, String> okapiHeaders,
                            Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    PgUtil.streamGet(TABLE_NAME_PERMSUSERS, PermissionUser.class, query, start - 1, length, null, "permissionUsers",
        routingContext, okapiHeaders, vertxContext);
  }

  @Validate
  @Override
  public void postPermsUsers(PermissionUser entity, RoutingContext routingContext, Map<String, String> okapiHeaders,
                             Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      //Check for existing user
      Criteria userIdCrit = new Criteria();
      userIdCrit.addField(USER_ID_FIELD);
      userIdCrit.setOperation("=");
      userIdCrit.setVal(entity.getUserId());
      Criterion criterion = new Criterion();
      if (entity.getId() != null) {
        Criteria idCrit = new Criteria();
        idCrit.addField(ID_FIELD);
        idCrit.setJSONB(false);
        idCrit.setOperation("=");
        idCrit.setVal(entity.getId());
        criterion.addCriterion(idCrit, "OR", userIdCrit);
      } else {
        criterion.addCriterion(userIdCrit);
      }
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
          TABLE_NAME_PERMSUSERS, PermissionUser.class,
          criterion, true, false, queryReply -> {
            if (queryReply.failed()) {
              String errStr = queryReply.cause().getMessage();
              logger.error(errStr, queryReply.cause());
              asyncResultHandler.handle(Future.succeededFuture(PostPermsUsersResponse.respond400WithTextPlain(errStr)));
              return;
            }
            List<PermissionUser> userList = queryReply.result().getResults();
            if (!userList.isEmpty()) {
              //This means that we have an existing user matching this username, error 400
              logger.warn("Constraint violated for userId or id field (or both)");
              asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsUsersResponse.respond422WithApplicationJson(
                      ValidationHelper.createValidationErrorMessage(
                          ID_FIELD, entity.getId(),
                          "userId and id fields must not match values for any existing records"))));
              return;
            }
            //Proceed to POST new user
            if (entity.getId() == null) {
              entity.setId(UUID.randomUUID().toString());
            }
            postPermsUsersTrans(entity, vertxContext, tenantId, asyncResultHandler);
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          PostPermsUsersResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  void postPermsUsersTrans(PermissionUser entity, Context vertxContext,
                           String tenantId, Handler<AsyncResult<Response>> asyncResultHandler) {

    PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    postgresClient.startTx(beginTx -> {
      logger.debug("Starting transaction to save new permissions user");
      postgresClient.save(beginTx, TABLE_NAME_PERMSUSERS, entity.getId(), entity, postReply -> {
        try {
          if (postReply.failed()) {
            String errStr = postReply.cause().getMessage();
            logger.error(errStr, postReply.cause());
            asyncResultHandler.handle(
                Future.succeededFuture(PostPermsUsersResponse.respond400WithTextPlain(errStr)));
            return;
          }
          final PermissionUser permUser = entity;
          updateUserPermissions(beginTx, permUser.getId(), new JsonArray(),
              new JsonArray(permUser.getPermissions()), vertxContext,
              tenantId).onComplete(updatePermsRes -> {
            if (updatePermsRes.failed()) {
              postgresClient.rollbackTx(beginTx, rollbackTx -> {
                logger.error("Error updating derived fields: " + updatePermsRes.cause());
                if (updatePermsRes.cause() instanceof InvalidPermissionsException) {
                  asyncResultHandler.handle(Future.succeededFuture(
                      PostPermsUsersResponse.respond422WithApplicationJson(
                          ValidationHelper.createValidationErrorMessage(
                              ID_FIELD, permUser.getId(),
                              UNABLE_TO_UPDATE_DERIVED_FIELDS + updatePermsRes.cause().getMessage()))));
                } else {
                  asyncResultHandler.handle(Future.succeededFuture(
                      PostPermsUsersResponse.respond500WithTextPlain(
                          updatePermsRes.cause().getMessage())));
                }
              });
              return;
            }
            postgresClient.endTx(beginTx, endTx -> {
              asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsUsersResponse.respond201WithApplicationJson(entity)));
            });
          });
        } catch (Exception e) {
          String errStr = "Error saving entity " + entity.toString() + ": " + e.getMessage();
          logger.error(errStr, e);
          asyncResultHandler.handle(Future.succeededFuture(
              PostPermsUsersResponse.respond500WithTextPlain(errStr)));
        }
      });
    });
  }

  @Validate
  @Override
  public void getPermsUsersById(String id, String indexField, Map<String, String> okapiHeaders,
                                Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      Criterion idCrit = getIdCriterion(indexField, id);
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMSUSERS, PermissionUser.class,
          idCrit, true, false, queryReply -> {
            if (queryReply.failed()) {
              String errStr = queryReply.cause().getMessage();
              logger.error(errStr, queryReply.cause());
              asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdResponse.respond400WithTextPlain(errStr)));
              return;
            }
            List<PermissionUser> userList = queryReply.result().getResults();
            if (userList.isEmpty()) {
              asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdResponse.respond404WithTextPlain("No user with id: " + id)));
              return;
            }
            asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdResponse.respond200WithApplicationJson(userList.get(0))));
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          GetPermsUsersByIdResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  @Validate
  @Override
  public void putPermsUsersById(String userid, PermissionUser entity, Map<String, String> okapiHeaders,
                                Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      checkPermlistForDummy(entity.getPermissions(), vertxContext, tenantId)
          .onComplete(cpfdRes -> {
            try {
              if (cpfdRes.failed()) {
                String errStr = cpfdRes.cause().getMessage();
                logger.error(errStr, cpfdRes.cause());
                asyncResultHandler.handle(Future.succeededFuture(
                    PutPermsUsersByIdResponse.respond400WithTextPlain(errStr)));
                return;
              }
              if (Boolean.TRUE.equals(cpfdRes.result())) {
                asyncResultHandler.handle(Future.succeededFuture(
                    PutPermsUsersByIdResponse.respond400WithTextPlain(
                        "Cannot add permissions flagged as 'dummy' to users")));
                return;
              }
              String query = "id==" + userid;
              CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMSUSERS);

              PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
                  TABLE_NAME_PERMSUSERS, PermissionUser.class, cqlFilter, true,
                  putPermsUsersbyIdHandle(userid, entity, asyncResultHandler, vertxContext, tenantId, cqlFilter));
            } catch (Exception e) {
              logger.error(e.getMessage(), e);
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsUsersByIdResponse.respond500WithTextPlain(e.getMessage())));
            }
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(
          Future.succeededFuture(PutPermsUsersByIdResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  private Handler<AsyncResult<Results<PermissionUser>>> putPermsUsersbyIdHandle(
      String userid, PermissionUser entity, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext,
      String tenantId, CQLWrapper cqlFilter) {

    return getReply -> {
      if (getReply.failed()) {
        String errStr = getReply.cause().getMessage();
        logger.error(errStr, getReply.cause());
        asyncResultHandler.handle(Future.succeededFuture(
            PutPermsUsersByIdResponse.respond400WithTextPlain(errStr)));
        return;
      }
      List<PermissionUser> userList = getReply.result().getResults();
      if (userList.isEmpty()) {
        asyncResultHandler.handle(Future.succeededFuture(
            PutPermsUsersByIdResponse.respond404WithTextPlain(
                "No permissions user found with id " + userid)));
        return;
      }
      try {
        PermissionUser originalUser = userList.get(0);
        PostgresClient pgClient = PostgresClient
            .getInstance(vertxContext.owner(), tenantId);
        pgClient.startTx(connection -> {
          pgClient.update(connection, TABLE_NAME_PERMSUSERS,
              entity, cqlFilter, true, updateReply -> {
                if (updateReply.failed()) {
                  pgClient.rollbackTx(connection, done -> {
                    String errStr = "Error with put: "
                        + updateReply.cause().getMessage();
                    logger.error(errStr, updateReply.cause());
                    asyncResultHandler.handle(Future.succeededFuture(
                        PutPermsUsersByIdResponse.respond500WithTextPlain(errStr)));
                  });
                  return;
                }
                updateUserPermissions(connection, userid,
                    new JsonArray(originalUser.getPermissions()),
                    new JsonArray(entity.getPermissions()),
                    vertxContext, tenantId).onComplete(updateUserPermsRes -> {
                  if (updateUserPermsRes.failed()) {
                    pgClient.rollbackTx(connection, done -> {
                      if (updateUserPermsRes.cause() instanceof InvalidPermissionsException) {
                        asyncResultHandler.handle(Future.succeededFuture(
                            PutPermsUsersByIdResponse.respond422WithApplicationJson(
                                ValidationHelper.createValidationErrorMessage(
                                    ID_FIELD, entity.getId(),
                                    UNABLE_TO_UPDATE_DERIVED_FIELDS + updateUserPermsRes.cause().getMessage()))));
                      } else {
                        String errStr = "Error with derived field update: " + updateUserPermsRes.cause().getMessage();
                        logger.error(errStr, updateUserPermsRes.cause());
                        asyncResultHandler.handle(Future.succeededFuture(
                            PutPermsUsersByIdResponse.respond500WithTextPlain(errStr)));
                      }
                    });
                    return;
                  }
                  //close Tx
                  pgClient.endTx(connection, done -> {
                    // https://issues.folio.org/browse/MODPERMS-99
                    // Remove inconsistent metadata - the update trigger uses different data
                    entity.setMetadata(null);
                    asyncResultHandler.handle(Future.succeededFuture(
                        PutPermsUsersByIdResponse.respond200WithApplicationJson(entity)));
                  });
                });
              });
        });
      } catch (Exception e) {
        logger.error(e.getMessage(), e);
        asyncResultHandler.handle(Future.succeededFuture(
            PutPermsUsersByIdResponse.respond500WithTextPlain(e.getMessage())));
      }
    };
  }

  @Validate
  @Override
  public void deletePermsUsersById(String userid, Map<String, String> okapiHeaders,
                                   Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      Criterion idCrit = getIdCriterion(userid);
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
      pgClient.startTx(connection -> {
        pgClient.get(connection, TABLE_NAME_PERMSUSERS, PermissionUser.class,
            idCrit, true, false, getReply -> {
              if (getReply.failed()) {
                //rollback
                pgClient.rollbackTx(connection, rollback -> {
                  String errStr = String.format("Error getting existing users: %s",
                      getReply.cause().getMessage());
                  logger.error(errStr, getReply.cause());
                  asyncResultHandler.handle(Future.succeededFuture(
                      DeletePermsUsersByIdResponse.respond400WithTextPlain(errStr)));
                });
                return;
              }
              List<PermissionUser> permUsers = getReply.result().getResults();
              if (permUsers.isEmpty()) {
                //rollback, 404
                pgClient.rollbackTx(connection, rollback -> {
                  asyncResultHandler.handle(Future.succeededFuture(
                      DeletePermsUsersByIdResponse.respond404WithTextPlain(
                          String.format("No permissions user found with id %s", userid))));
                });
                return;
              }
              PermissionUser permUser = permUsers.get(0);
              try {
                updateUserPermissions(connection, userid,
                    new JsonArray(permUser.getPermissions()), new JsonArray(),
                    vertxContext, tenantId).onComplete(updateUserPermsRes -> {
                  if (updateUserPermsRes.failed()) {
                    pgClient.rollbackTx(connection, rollback -> {
                      String errStr = String.format("Error updating metadata: %s",
                          updateUserPermsRes.cause().getMessage());
                      logger.error(errStr, updateUserPermsRes.cause());
                      asyncResultHandler.handle(Future.succeededFuture(
                          DeletePermsUsersByIdResponse.respond500WithTextPlain(errStr)));
                    });
                    return;
                  }
                  pgClient.delete(connection, TABLE_NAME_PERMSUSERS,
                      userid, deleteReply -> {
                        if (deleteReply.failed()) {
                          pgClient.rollbackTx(connection, rollback -> {
                            String errStr = String.format("Error deleting user: %s",
                                deleteReply.cause().getMessage());
                            logger.error(errStr, deleteReply.cause());
                            asyncResultHandler.handle(Future.succeededFuture(
                                DeletePermsUsersByIdResponse.respond500WithTextPlain(errStr)));
                          });
                          return;
                        }
                        if (deleteReply.result().rowCount() == 0) {
                          pgClient.rollbackTx(connection, rollback -> {
                            asyncResultHandler.handle(Future.succeededFuture(
                                DeletePermsUsersByIdResponse.respond404WithTextPlain(userid)));
                          });
                          return;
                        }
                        pgClient.endTx(connection, done -> {
                          asyncResultHandler.handle(Future.succeededFuture(
                              DeletePermsUsersByIdResponse.respond204()));
                        });
                      });
                });
              } catch (Exception e) {
                //rollback
                pgClient.rollbackTx(connection, rollback -> {
                  String errStr = String.format("Error deleting user: %s",
                      e.getMessage());
                  logger.error(errStr, e);
                  asyncResultHandler.handle(Future.succeededFuture(
                      DeletePermsUsersByIdResponse.respond500WithTextPlain(errStr)));
                });
              }
            });
      });
    } catch (Exception e) {
      String errStr = "Error using Postgres instance: " + e.getMessage();
      logger.error(errStr, e);
      asyncResultHandler.handle(Future.succeededFuture(
          DeletePermsUsersByIdResponse.respond500WithTextPlain(errStr)));
    }
  }

  @Validate
  @Override
  public void getPermsUsersPermissionsById(String id, String expanded,
                                           String full, String indexField, Map<String, String> okapiHeaders,
                                           Handler<AsyncResult<Response>> asyncResultHandler,
                                           Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      boolean fullBool = "true".equals(full);
      boolean expandedBool = "true".equals(expanded);

      Future<PermissionNameListObject> pnloFuture = this.getPermissionsForUser(
          id, expandedBool, fullBool, indexField, tenantId, vertxContext);

      pnloFuture.onComplete(res -> {
        if (res.failed()) {
          String errStr = res.cause().getMessage();
          logger.error(errStr, res.cause());
          asyncResultHandler.handle(Future.succeededFuture(
              GetPermsUsersPermissionsByIdResponse.respond400WithTextPlain(errStr)));
          return;
        }
        PermissionNameListObject pnlo = res.result();
        if (pnlo == null) { //404
          asyncResultHandler.handle(Future.succeededFuture(
              GetPermsUsersPermissionsByIdResponse.respond404WithTextPlain(
                  "No user found by id " + id)));
        } else {
          asyncResultHandler.handle(Future.succeededFuture(
              GetPermsUsersPermissionsByIdResponse.respond200WithApplicationJson(pnlo)));
        }
      });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          GetPermsUsersPermissionsByIdResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  @Validate
  @Override
  public void postPermsUsersPermissionsById(String id, String indexField,
                                            PermissionNameObject entity, Map<String, String> okapiHeaders,
                                            Handler<AsyncResult<Response>> asyncResultHandler,
                                            Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      Criterion useridCrit = getIdCriterion(indexField, id);
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
          TABLE_NAME_PERMSUSERS,
          PermissionUser.class, useridCrit, true, false,
          getReply -> {
            try {
              if (getReply.failed()) {
                String errStr = "Error checking for user: " + getReply.cause().getMessage();
                logger.error(errStr, getReply.cause());
                asyncResultHandler.handle(Future.succeededFuture(
                    PostPermsUsersPermissionsByIdResponse.respond400WithTextPlain(errStr)));
                return;
              }
              List<PermissionUser> userList = getReply.result().getResults();
              if (userList.isEmpty()) {
                asyncResultHandler.handle(Future.succeededFuture(
                    PostPermsUsersPermissionsByIdResponse
                        .respond400WithTextPlain("User with id " + id
                            + " does not exist")));
                return;
              }
              //now we can actually add it
              String permissionName = entity.getPermissionName();
              PermissionUser user = userList.get(0);
              String actualId = user.getId();
              JsonArray originalPermissions = new JsonArray(
                  new ArrayList<Object>(user.getPermissions()));
              if (user.getPermissions().contains(permissionName)) {
                asyncResultHandler.handle(Future.succeededFuture(
                    PostPermsUsersPermissionsByIdResponse
                        .respond422WithApplicationJson(ValidationHelper
                            .createValidationErrorMessage(USER_NAME_FIELD, actualId,
                                "User with id " + actualId + " already has permission "
                                    + permissionName))));
                return;
              }
              updatePermissionsForUser(entity, vertxContext, tenantId, permissionName, user, actualId, originalPermissions, asyncResultHandler);
            } catch (Exception e) {
              logger.error("Error using Postgres instance to retrieve user: "
                  + e.getMessage(), e);
              asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsUsersPermissionsByIdResponse
                      .respond500WithTextPlain(e.getMessage())));
            }
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          PostPermsUsersPermissionsByIdResponse
              .respond500WithTextPlain(e.getMessage())));
    }
  }

  @SuppressWarnings({"squid:S00107"})   // Method has more than 7 parameters
  private void updatePermissionsForUser(PermissionNameObject entity,
                                        Context vertxContext, String tenantId, String permissionName,
                                        PermissionUser user, String actualId, JsonArray originalPermissions,
                                        Handler<AsyncResult<Response>> asyncResultHandler) {

    retrievePermissionByName(permissionName, vertxContext, tenantId)
        .onComplete(rpbnRes -> {
          if (rpbnRes.failed()) {
            String errStr = String.format("Error attempting to update user: %s",
                rpbnRes.cause().getMessage());
            logger.error(errStr, rpbnRes.cause());
            asyncResultHandler.handle(Future.succeededFuture(
                PostPermsUsersPermissionsByIdResponse
                    .respond500WithTextPlain(errStr)));
            return;
          }
          if (rpbnRes.result() == null) {
            asyncResultHandler.handle(Future.succeededFuture(
                PostPermsUsersPermissionsByIdResponse.respond400WithTextPlain(
                    String.format("Permission by name '%s' does not exist",
                        permissionName))));
            return;
          }
          if (Boolean.TRUE.equals(rpbnRes.result().getDummy())) {
            asyncResultHandler.handle(Future.succeededFuture(
                PostPermsUsersPermissionsByIdResponse.respond400WithTextPlain(
                    String.format("'%s' is flagged as a dummy permission"
                        + " and cannot be assigned to a user", permissionName))));
            return;
          }
          try {
            user.getPermissions().add(permissionName);
            PostgresClient pgClient = PostgresClient.getInstance(
                vertxContext.owner(), tenantId);
            String query = String.format("id==%s", actualId);
            CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMSUSERS);
            pgClient.startTx(connection -> {
              pgClient.update(connection, TABLE_NAME_PERMSUSERS, user,
                  cqlFilter, true, putReply -> {
                    if (putReply.failed()) {
                      //rollback
                      pgClient.rollbackTx(connection, rollback -> {
                        String errStr = String.format(
                            "Error attempting to update user: %s",
                            putReply.cause().getMessage());
                        logger.error(errStr, putReply.cause());
                        asyncResultHandler.handle(Future.succeededFuture(
                            PostPermsUsersPermissionsByIdResponse
                                .respond500WithTextPlain(errStr)));
                      });
                      return;
                    }
                    //update metadata
                    updateUserPermissions(connection, actualId, originalPermissions,
                        new JsonArray(user.getPermissions()), vertxContext,
                        tenantId).onComplete(updateUserPermsRes -> {
                      if (updateUserPermsRes.failed()) {
                        //rollback
                        pgClient.rollbackTx(connection, rollback -> {
                          String errStr = String.format(
                              "Error attempting to update permissions metadata: %s",
                              updateUserPermsRes.cause().getMessage());
                          logger.error(errStr, updateUserPermsRes.cause());
                          asyncResultHandler.handle(Future.succeededFuture(
                              PostPermsUsersPermissionsByIdResponse
                                  .respond500WithTextPlain(errStr)));
                        });
                        return;
                      }
                      //close the transaction
                      pgClient.endTx(connection, done -> {
                        asyncResultHandler.handle(Future.
                            succeededFuture(
                                PostPermsUsersPermissionsByIdResponse
                                    .respond200WithApplicationJson(entity)));
                      });

                    });
                  });
            });
          } catch (Exception e) {
            logger.error(e.getMessage(), e);
            asyncResultHandler.handle(Future.succeededFuture(
                PostPermsUsersPermissionsByIdResponse
                    .respond500WithTextPlain(e.getMessage())));
          }
        });
  }

  @Validate
  @Override
  public void deletePermsUsersPermissionsByIdAndPermissionname(
      String id, String permissionName,
      String indexField, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      Criterion idCrit = getIdCriterion(indexField, id);
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMSUSERS,
          PermissionUser.class, idCrit, true, false, getReply -> {
            if (getReply.failed()) {
              String errStr = getReply.cause().getMessage();
              logger.error(errStr, getReply.cause());
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsUsersPermissionsByIdAndPermissionnameResponse.respond400WithTextPlain(errStr)));
              return;
            }
            List<PermissionUser> userList = getReply.result().getResults();
            if (userList.isEmpty()) {
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsUsersPermissionsByIdAndPermissionnameResponse.respond404WithTextPlain(
                      "User with id " + id + " does not exist")));
              return;
            }
            //attempt to delete permission
            PermissionUser user = userList.get(0);
            if (!user.getPermissions().contains(permissionName)) {
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsUsersPermissionsByIdAndPermissionnameResponse.respond400WithTextPlain(
                      "User with id " + id + " does not contain " + permissionName)));
              return;
            }
            try {
              String query = String.format("id==%s", user.getId());
              CQLWrapper cqlFilter = getCQL(query, TABLE_NAME_PERMSUSERS);
              JsonArray originalPermissions = new JsonArray(
                  new ArrayList<Object>(user.getPermissions()));
              user.getPermissions().remove(permissionName);
              PostgresClient pgClient = PostgresClient.getInstance(
                  vertxContext.owner(), tenantId);
              pgClient.startTx(connection -> {
                pgClient.update(connection, TABLE_NAME_PERMSUSERS, user,
                    cqlFilter, true, putReply -> {
                      if (putReply.failed()) {
                        pgClient.rollbackTx(connection, rollback -> {
                          String errStr = String.format(
                              "Error attempting to delete permission '%s' from user: %s",
                              permissionName,
                              putReply.cause().getMessage());
                          logger.error(errStr, putReply.cause());
                          asyncResultHandler.handle(Future.succeededFuture(
                              DeletePermsUsersPermissionsByIdAndPermissionnameResponse
                                  .respond500WithTextPlain(errStr)));
                        });
                        return;
                      }
                      updateUserPermissions(connection, user.getId(), originalPermissions,
                          new JsonArray(user.getPermissions()), vertxContext,
                          tenantId).onComplete(updateUserPermsRes -> {
                        if (updateUserPermsRes.failed()) {
                          pgClient.rollbackTx(connection, rollback -> {
                            String errStr = String.format(
                                "Error attempting to update permission metadata: %s",
                                updateUserPermsRes.cause().getMessage());
                            logger.error(errStr, updateUserPermsRes.cause());
                            asyncResultHandler.handle(Future.succeededFuture(
                                DeletePermsUsersPermissionsByIdAndPermissionnameResponse
                                    .respond500WithTextPlain(errStr)));
                          });
                          return;
                        }
                        pgClient.endTx(connection, done -> {
                          asyncResultHandler.handle(Future.succeededFuture(
                              DeletePermsUsersPermissionsByIdAndPermissionnameResponse.respond204()));
                        });
                      });
                    });
              });
            } catch (Exception e) {
              logger.error(e.getMessage(), e);
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsUsersPermissionsByIdAndPermissionnameResponse.respond500WithTextPlain(
                      e.getMessage())));
            }
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          DeletePermsUsersPermissionsByIdAndPermissionnameResponse.respond500WithTextPlain(
              e.getMessage())));
    }
  }

  @Validate
  @Override
  public void postPermsPermissions(PermissionUpload entity, Map<String, String> okapiHeaders,
                                   Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      Criteria nameCrit = new Criteria();
      nameCrit.addField(PERMISSION_NAME_FIELD);
      if (entity.getPermissionName() == null) {
        nameCrit.setOperation("IS NULL");
      } else {
        nameCrit.setOperation("=");
        nameCrit.setVal(entity.getPermissionName());
      }
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
          TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit), true, false, getReply -> {
            if (getReply.failed()) {
              String errStr = getReply.cause().getMessage();
              logger.error(errStr, getReply.cause());
              asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse.respond400WithTextPlain(errStr)));
              return;
            }
            List<Permission> permissionList = getReply.result().getResults();
            if (!permissionList.isEmpty()) {
              asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsPermissionsResponse.respond422WithApplicationJson(
                      ValidationHelper.createValidationErrorMessage(
                          PERMISSION_NAME_FIELD, entity.getPermissionName(),
                          "Permission with name " + entity.getPermissionName() + " already exists"))));
              return;
            }
            //Do the actual POST of the new permission
            PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
            postgresClient.startTx(connection -> {
              logger.debug("Attempting to save new Permission");
              if (entity.getVisible() == null) {
                entity.setVisible(true);
              }
              if (entity.getId() == null) {
                entity.setId(UUID.randomUUID().toString());
              }
              String newId = entity.getId();
              if (entity.getPermissionName() == null) {
                entity.setPermissionName(newId);
              }
              Permission realPerm = getRealPermObject(entity);
              realPerm.setDummy(false);
              try {
                postgresClient.save(connection, TABLE_NAME_PERMS, newId, realPerm, postReply -> {
                  if (postReply.failed()) {
                    postgresClient.rollbackTx(connection, done -> {
                      logger.error("Unable to save new permission: "
                          + postReply.cause().getMessage(), postReply.cause());
                      asyncResultHandler.handle(Future.succeededFuture(
                          PostPermsPermissionsResponse
                              .respond500WithTextPlain(postReply.cause().getMessage())));
                    });
                    return;
                  }
                  updateSubPermissions(connection, entity.getPermissionName(), new JsonArray(),
                      new JsonArray(entity.getSubPermissions()), vertxContext,
                      tenantId).onComplete(updateSubPermsRes -> {
                    if (updateSubPermsRes.failed()) {
                      postgresClient.rollbackTx(connection, done -> {
                        asyncResultHandler.handle(Future.succeededFuture(
                            PostPermsPermissionsResponse.respond422WithApplicationJson(
                                ValidationHelper.createValidationErrorMessage(
                                    PERMISSION_NAME_FIELD, entity.getPermissionName(),
                                    updateSubPermsRes.cause().getMessage()))));
                      });
                      return;
                    }
                    postgresClient.endTx(connection, done -> {
                      asyncResultHandler.handle(Future.succeededFuture(PostPermsPermissionsResponse
                          .respond201WithApplicationJson(entity)));
                    });
                  });
                });
              } catch (Exception e) {
                postgresClient.rollbackTx(connection, done -> {
                  logger.error(e.getMessage(), e);
                  asyncResultHandler.handle(Future.succeededFuture(
                      PostPermsPermissionsResponse.respond500WithTextPlain(e.getMessage())));
                });
              }
            });
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          PostPermsPermissionsResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  @Validate
  @Override
  public void getPermsPermissionsById(String id, Map<String, String> okapiHeaders,
                                      Handler<AsyncResult<Response>> asyncResultHandler,
                                      Context vertxContext) {
    PgUtil.getById(TABLE_NAME_PERMS, Permission.class, id, okapiHeaders, vertxContext,
        GetPermsPermissionsByIdResponse.class, asyncResultHandler);
  }

  @Validate
  @Override
  public void putPermsPermissionsById(String id, PermissionUpload entity,
                                      Map<String, String> okapiHeaders,
                                      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      if (entity.getId() == null || !entity.getId().equals(id)) {
        asyncResultHandler.handle(Future.succeededFuture(
            PutPermsPermissionsByIdResponse.respond400WithTextPlain("Invalid id value")));
        return;
      }
      Criterion criterion = getIdCriterion(id);
      CQLWrapper cqlFilter = new CQLWrapper(criterion);
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
          TABLE_NAME_PERMS, Permission.class, criterion,
          true, false, getReply -> {
            if (getReply.failed()) {
              String message = getReply.cause().getMessage();
              logger.error(message, getReply.cause());
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsPermissionsByIdResponse.respond400WithTextPlain(message)));
              return;
            }
            List<Permission> permList = getReply.result().getResults();
            if (permList.isEmpty()) {
              String message = "No permission found to match that id";
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsPermissionsByIdResponse.respond404WithTextPlain(message)));
              return;
            }
            Permission perm = permList.get(0);
            Permission updatePerm = getRealPermObject(entity);
            updatePerm.setId(entity.getId());
            updatePerm.setChildOf(perm.getChildOf());
            updatePerm.setGrantedTo(perm.getGrantedTo());
            if (!perm.getPermissionName().equals(entity.getPermissionName())) {
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsPermissionsByIdResponse.respond400WithTextPlain(
                      "permission name property cannot change")));
              return;
            }
            if (perm.getDummy() != null && perm.getDummy()) {
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsPermissionsByIdResponse.respond400WithTextPlain(
                      "dummy permissions cannot be modified")));
              return;
            }
            try {
              updatePerm.setDummy(false);
              PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
              pgClient.startTx(connection -> {
                pgClient.update(connection, TABLE_NAME_PERMS, updatePerm,
                    cqlFilter, true, putReply -> {
                      if (putReply.failed()) {
                        pgClient.rollbackTx(connection, done -> {
                          logger.error("Error with put: " + putReply.cause().getMessage(), putReply.cause());
                          asyncResultHandler.handle(Future.succeededFuture(
                              PutPermsPermissionsByIdResponse.respond500WithTextPlain(
                                  putReply.cause().getMessage())));
                        });
                        return;
                      }
                      updateSubPermissions(connection, entity.getPermissionName(),
                          new JsonArray(perm.getSubPermissions()),
                          new JsonArray(entity.getSubPermissions()),
                          vertxContext, tenantId)
                          .onComplete(updateSubPermsRes -> {
                            if (updateSubPermsRes.failed()) {
                              pgClient.rollbackTx(connection, done -> {
                                if (updateSubPermsRes.cause() instanceof InvalidPermissionsException) {
                                  asyncResultHandler.handle(Future.succeededFuture(
                                      PutPermsPermissionsByIdResponse.respond422WithApplicationJson(
                                          ValidationHelper.createValidationErrorMessage(
                                              ID_FIELD, entity.getId(),
                                              UNABLE_TO_UPDATE_DERIVED_FIELDS + updateSubPermsRes.cause().getMessage()))));
                                } else {
                                  String errStr = "Error with derived field update: "
                                      + updateSubPermsRes.cause().getMessage();
                                  logger.error(errStr, updateSubPermsRes.cause());
                                  asyncResultHandler.handle(Future.succeededFuture(
                                      PutPermsPermissionsByIdResponse.respond500WithTextPlain(
                                          errStr)));
                                }
                              });
                              return;
                            }
                            //close connection
                            pgClient.endTx(connection, done -> {
                              asyncResultHandler.handle(Future.succeededFuture(
                                  PutPermsPermissionsByIdResponse.respond200WithApplicationJson(entity)));
                            });
                          });
                    });
              });
            } catch (Exception e) {
              logger.error(e.getMessage(), e);
              asyncResultHandler.handle(Future.succeededFuture(PutPermsPermissionsByIdResponse
                  .respond500WithTextPlain(e.getMessage())));
            }
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          PutPermsPermissionsByIdResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  @Validate
  @Override
  public void deletePermsPermissionsById(String id,
                                         Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
                                         Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      Criterion idCrit = getIdCriterion(id);
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
          Permission.class, idCrit, true, false, getReply -> {
            if (getReply.failed()) {
              String errStr = getReply.cause().getMessage();
              logger.error(errStr, getReply.cause());
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsPermissionsByIdResponse.respond400WithTextPlain(errStr)));
              return;
            }
            List<Permission> permList = getReply.result().getResults();
            if (permList.isEmpty()) {
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsPermissionsByIdResponse.respond404WithTextPlain(id)));
              return;
            }
            Permission perm = permList.get(0);
            if (!perm.getChildOf().isEmpty() || !perm.getGrantedTo().isEmpty()) {
              PostgresClient pgClient = PostgresClient.getInstance(
                  vertxContext.owner(), tenantId);
              pgClient.startTx(connection -> {
                List<String> parentPermissionList = new ArrayList<>();
                for (Object ob : perm.getChildOf()) {
                  parentPermissionList.add((String) ob);
                }
                List<String> userIdList = new ArrayList<>();
                for (Object ob : perm.getGrantedTo()) {
                  userIdList.add((String) ob);
                }
                removePermissionFromUserList(connection, perm.getPermissionName(),
                    userIdList, vertxContext, tenantId).onComplete(rpfulRes -> {
                  if (rpfulRes.failed()) {
                    //rollback
                    pgClient.rollbackTx(connection, rollback -> {
                      String errStr = rpfulRes.cause().getMessage();
                      logger.error(errStr, rpfulRes.cause());
                      asyncResultHandler.handle(Future.succeededFuture(
                          DeletePermsPermissionsByIdResponse.respond500WithTextPlain(
                              errStr)));
                    });
                  } else {
                    removeSubpermissionFromPermissionList(connection,
                        perm.getPermissionName(), parentPermissionList,
                        vertxContext, tenantId).onComplete(rsfplRes -> {
                      if (rsfplRes.failed()) {
                        pgClient.rollbackTx(connection, rollback -> {
                          String errStr = rsfplRes.cause().getMessage();
                          logger.error(errStr, rsfplRes.cause());
                          asyncResultHandler.handle(Future.succeededFuture(
                              DeletePermsPermissionsByIdResponse.respond500WithTextPlain(
                                  errStr)));
                        });
                        return;
                      }
                      pgClient.delete(connection, TABLE_NAME_PERMS,
                          id, deleteReply -> {
                            if (deleteReply.failed()) {
                              //rollback
                              pgClient.rollbackTx(connection, rollback -> {
                                String errStr = deleteReply.cause().getMessage();
                                logger.error(errStr, deleteReply.cause());
                                asyncResultHandler.handle(Future.succeededFuture(
                                    DeletePermsPermissionsByIdResponse.respond500WithTextPlain(
                                        errStr)));
                              });
                              return;
                            }
                            //close tx
                            pgClient.endTx(connection, done -> {
                              asyncResultHandler.handle(Future.succeededFuture(
                                  DeletePermsPermissionsByIdResponse
                                      .respond204()));
                            });
                          });
                    });
                  }
                });
              });
            } else {
              try {
                PostgresClient.getInstance(vertxContext.owner(), tenantId).delete(TABLE_NAME_PERMS,
                    id, deleteReply -> {
                      if (deleteReply.failed()) {
                        logger.error("deleteReply failed: " + deleteReply.cause().getMessage());
                        asyncResultHandler.handle(Future.succeededFuture(
                            DeletePermsPermissionsByIdResponse.respond500WithTextPlain(
                                deleteReply.cause().getMessage())));
                        return;
                      }
                      if (deleteReply.result().rowCount() == 0) {
                        asyncResultHandler.handle(Future.succeededFuture(
                            DeletePermsPermissionsByIdResponse.respond404WithTextPlain(id)));
                        return;
                      }
                      asyncResultHandler.handle(Future.succeededFuture(DeletePermsPermissionsByIdResponse.respond204()));
                    });
              } catch (Exception e) {
                logger.error(e.getMessage(), e);
                asyncResultHandler.handle(Future.succeededFuture(
                    DeletePermsPermissionsByIdResponse.respond500WithTextPlain(e.getMessage())));
              }
            }
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(DeletePermsPermissionsByIdResponse.respond500WithTextPlain(
          e.getMessage())));
    }
  }

  @SuppressWarnings("java:S3776")
  @Validate
  @Override
  public void getPermsPermissions(String expandSubs, String expanded, String includeDummy,
                                  int length, int start, String sortBy, String query, String memberOf,
                                  String ownedBy, Map<String, String> okapiHeaders,
                                  Handler<AsyncResult<Response>> asyncResultHandler,
                                  Context vertxContext) {

    try {
      boolean includeDummyPerms = "true".equals(includeDummy);
      String[] queryArr = new String[]{""};
      if (!includeDummyPerms) {
        //filter out all dummy perms from query
        if (query == null || query.isEmpty()) {
          queryArr[0] = "(dummy == false)";
        } else {
          queryArr[0] = String.format("(%s) AND (dummy==false)", query);
        }
      } else {
        queryArr[0] = query;
      }
      CQLWrapper cql;
      logger.info(String.format("Generating cql to request rows from table '%s' with query '%s'",
          TABLE_NAME_PERMS, queryArr[0]));
      cql = getCQL(queryArr[0], TABLE_NAME_PERMS, length, start - 1);
      String tenantId = TenantTool.tenantId(okapiHeaders);
      String[] fieldList = {"*"};
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
          Permission.class, fieldList, cql, true, false, getReply -> {
            try {
              if (getReply.failed()) {
                logger.error(getReply.cause().getMessage());
                asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.respond400WithTextPlain(getReply.cause().getMessage())));
                return;
              }
              PermissionListObject permCollection = new PermissionListObject();
              List<Permission> permissions = getReply.result().getResults();
              List<Future> futureList = new ArrayList<>();
              for (Permission permission : permissions) {
                Future<Permission> permFuture;
                if ("true".equals(expandSubs)) {
                  permFuture = expandSubPermissions(permission, vertxContext, tenantId);
                } else if ("true".equals(expanded)) {
                  Promise<Permission> promise = Promise.promise();
                  permFuture = promise.future();
                  List<String> subperms = new ArrayList<>(permission.getSubPermissions().size());
                  permission.getSubPermissions().forEach(sub -> subperms.add(sub.toString()));
                  Future<List<String>> expandedSubPerms = PermsCache.expandPerms(subperms, vertxContext, tenantId);
                  expandedSubPerms.onComplete(ar -> {
                    if (ar.succeeded()) {
                      List<Object> list = new ArrayList<>(ar.result().size());
                      ar.result().forEach(list::add);
                      permission.setSubPermissions(list);
                      promise.complete(permission);
                    } else {
                      promise.fail(ar.cause());
                    }
                  });
                } else {
                  permFuture = Future.succeededFuture(permission);
                }
                futureList.add(permFuture);
              }
              CompositeFuture compositeFuture = CompositeFuture.join(futureList);
              compositeFuture.onComplete(compositeResult -> {
                if (compositeFuture.failed()) {
                  logger.error("Error expanding permissions: " + compositeFuture.cause().getMessage());
                  asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.respond500WithTextPlain("Error getting expanded permissions: " + compositeResult.cause().getMessage())));
                } else {
                  List<Permission> newPermList = new ArrayList<>();
                  for (Future f : futureList) {
                    newPermList.add((Permission) (f.result()));
                  }
                  permCollection.setPermissions(newPermList);
                  permCollection.setTotalRecords(getReply.result().getResultInfo().getTotalRecords());
                  asyncResultHandler.handle(Future.succeededFuture(GetPermsPermissionsResponse.respond200WithApplicationJson(permCollection)));
                }

              });
            } catch (Exception e) {
              logger.error(e.getMessage(), e);
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                  GetPermsPermissionsResponse.respond500WithTextPlain(e.getMessage())));
            }
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
          GetPermsPermissionsResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  protected static Future<Boolean> checkPermissionExists(AsyncResult<SQLConnection> connection,
                                                         String permissionName, Context vertxContext, String tenantId) {

    Logger logger = LoggerFactory.getLogger(PermsAPI.class);
    Promise<Boolean> promise = Promise.promise();
    try {
      Criteria nameCrit = new Criteria();
      nameCrit.addField(PERMISSION_NAME_FIELD);
      nameCrit.setOperation("=");
      nameCrit.setVal(permissionName);
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(connection,
          TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit),
          true, false, getReply -> {
            if (getReply.failed()) {
              promise.fail(getReply.cause());
              return;
            }
            List<Permission> permList = getReply.result().getResults();
            promise.complete(!permList.isEmpty());
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      promise.fail(e);
    }
    return promise.future();
  }

  private static Future<List<String>> findMissingPermissionsFromList(
      AsyncResult<SQLConnection> connection, List<Object> permissionList, Context vertxContext, String tenantId,
      List<String> missingPermissions) {

    Promise<List<String>> promise = Promise.promise();
    if (missingPermissions == null) {
      missingPermissions = new ArrayList<>();
    }
    final List<String> finalMissingPermissions = missingPermissions;

    if (permissionList.isEmpty()) {
      return Future.succeededFuture(finalMissingPermissions);
    }
    List<Object> permissionListCopy = new ArrayList<>(permissionList);
    Future<Boolean> checkPermissionExistsFuture;
    String permissionName = (String) permissionListCopy.get(0);
    permissionListCopy.remove(0); //pop
    checkPermissionExistsFuture = checkPermissionExists(connection,
        permissionName, vertxContext, tenantId);
    checkPermissionExistsFuture.onComplete(res -> {
      if (res.failed()) {
        promise.fail(res.cause());
        return;
      }
      if (Boolean.FALSE.equals(res.result())) {
        finalMissingPermissions.add(permissionName);
      }
      promise.complete(finalMissingPermissions);
    });
    return promise.future().compose(mapper -> findMissingPermissionsFromList(connection, permissionListCopy,
        vertxContext, tenantId, finalMissingPermissions));
  }

  private Future<List<String>> getAllExpandedPermissionsSequential(
      List<List<String>> listOfPermissionLists, Context vertxContext,
      String tenantId, List<String> returnedPermissions) {

    if (returnedPermissions == null) {
      returnedPermissions = new ArrayList<>();
    }
    final List<String> finalReturnedPermissions = returnedPermissions;

    if (listOfPermissionLists.isEmpty()) {
      return Future.succeededFuture(new ArrayList<>(finalReturnedPermissions));
    }

    Promise<List<String>> promise = Promise.promise();

    List<List<String>> listOfListsCopy = new ArrayList<>(listOfPermissionLists);
    List<String> permissionList = listOfListsCopy.get(0);
    listOfListsCopy.remove(0); //pop
    getExpandedPermissionsSequential(permissionList, vertxContext, tenantId)
        .onComplete(gepsRes -> {
          if (gepsRes.failed()) {
            promise.fail(gepsRes.cause());
            return;
          }
          List<String> combinedResult = new ArrayList<>(finalReturnedPermissions);
          combinedResult.addAll(gepsRes.result());
          promise.complete(combinedResult);
        });

    return promise.future().compose(res -> getAllExpandedPermissionsSequential(listOfListsCopy,
        vertxContext, tenantId, res));
  }

  private Future<List<String>> getExpandedPermissionsSequential(List<String> permissionList,
                                                                Context vertxContext, String tenantId) {

    if (permissionList.isEmpty()) {
      return Future.succeededFuture(new ArrayList<>());
    }

    // use cache by default unless set to false explicitly
    Boolean usePermsCache = vertxContext.config().getBoolean(PermsCache.CACHE_HEADER);
    if (usePermsCache == null || usePermsCache) {
      return PermsCache.expandPerms(permissionList, vertxContext, tenantId);
    }

    Promise<List<String>> promise = Promise.promise();

    try {
      Criterion criterion = buildPermissionNameListQuery(permissionList);
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(),
          tenantId);
      pgClient.get(TABLE_NAME_PERMS, Permission.class, criterion, true, false,
          getReply -> {
            if (getReply.failed()) {
              promise.fail(getReply.cause());
              return;
            }
            List<String> allSubPermList = new ArrayList<>();
            List<String> foundPermNameList = new ArrayList<>();
            List<Permission> foundPermList = getReply.result().getResults();
            for (Permission perm : foundPermList) {
              foundPermNameList.add(perm.getPermissionName());
              List<String> subPermList = perm.getSubPermissions().stream()
                  .map(object -> Objects.toString(object, null))
                  .collect(Collectors.toList());
              for (String subPerm : subPermList) {
                if (!allSubPermList.contains(subPerm)) {
                  allSubPermList.add(subPerm);
                }
              }
            }
            int splitSize = 15;
            if (splitSize < permissionList.size()) {
              splitSize = permissionList.size();
            }
            List<List<String>> listOfSubPermLists = splitStringList(
                allSubPermList, splitSize);
            Future<List<String>> listFuture;
            if (listOfSubPermLists.isEmpty()) {
              listFuture = Future.succeededFuture(foundPermNameList);
            } else {
              listFuture = getAllExpandedPermissionsSequential(listOfSubPermLists, vertxContext,
                  tenantId, foundPermNameList);
            }
            listFuture.onComplete(promise::handle);
          });
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<PermissionNameListObject> getAllFullPermissions(List<String> nameList,
                                                                 Context vertxContext, String tenantId) {
    Promise<PermissionNameListObject> promise = Promise.promise();
    List<Future> futureList = new ArrayList<>();
    for (String name : nameList) {
      Future<Permission> permissionFuture = getFullPermissions(name, vertxContext, tenantId);
      futureList.add(permissionFuture);
    }
    CompositeFuture compositeFuture = CompositeFuture.all(futureList);
    compositeFuture.onComplete(res -> {
      if (res.failed()) {
        promise.fail(res.cause());
        return;
      }
      PermissionNameListObject pnlo = new PermissionNameListObject();
      List<Object> permList = new ArrayList<>();
      for (Future doneFuture : futureList) {
        Object result = doneFuture.result();
        if (result != null) {
          permList.add(result);
        }
      }
      pnlo.setPermissionNames(permList);
      promise.complete(pnlo);
    });
    return promise.future();
  }

  private Future<Permission> getFullPermissions(String permissionName,
                                                Context vertxContext, String tenantId) {
    logger.debug("Getting full permissions for " + permissionName);

    // use cache by default unless set to false explicitly
    Boolean usePermsCache = vertxContext.config().getBoolean(PermsCache.CACHE_HEADER);
    if (usePermsCache == null || usePermsCache) {
      return PermsCache.getFullPerms(permissionName, vertxContext, tenantId);
    }

    Promise<Permission> promise = Promise.promise();
    try {
      Criteria nameCrit = new Criteria();
      nameCrit.addField(PERMISSION_NAME_FIELD);
      nameCrit.setOperation("=");
      nameCrit.setVal(permissionName);
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
          Permission.class, new Criterion(nameCrit), true, false, getReply -> {
            if (getReply.failed()) {
              logger.debug("postgres client 'get' failed: " + getReply.cause().getMessage());
              promise.fail(getReply.cause());
              return;
            }
            List<Permission> permList = getReply.result().getResults();
            if (permList.isEmpty()) {
              logger.debug("No permission object '" + permissionName + "' exists");
              promise.complete(null);
            } else {
              logger.debug("Completing promise for getFullPermissions for '" + permissionName + "'");
              promise.complete(permList.get(0));
            }
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<PermissionNameListObject> getPermissionsForUser(
      String userId, boolean expanded, boolean full,
      String indexField, String tenantId, Context vertxContext) {

    Promise<PermissionNameListObject> promise = Promise.promise();
    try {
      Criterion idCrit = getIdCriterion(indexField, userId);
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
          TABLE_NAME_PERMSUSERS, PermissionUser.class, idCrit,
          true, false, getReply -> {
            if (getReply.failed()) {
              promise.fail(getReply.cause());
              return;
            }
            List<PermissionUser> userList = getReply.result().getResults();
            if (userList.isEmpty()) {
              promise.complete(null);
              return;
            }
            Future<List<String>> interimFuture;
            List<String> permissionNameList = new ArrayList<>();
            for (Object perm : userList.get(0).getPermissions()) {
              if (perm != null) {
                permissionNameList.add((String) perm);
              }
            }
            if (!expanded) {
              interimFuture = Future.succeededFuture(permissionNameList);
            } else {
              List<List<String>> listOfPermNamesList
                  = splitStringList(permissionNameList, 10);
              interimFuture = getAllExpandedPermissionsSequential(listOfPermNamesList,
                  vertxContext, tenantId, null);
            }
            interimFuture.onComplete(res -> {
              if (res.failed()) {
                promise.fail(res.cause());
                return;
              }
              if (!full) {
                PermissionNameListObject pnlo = new PermissionNameListObject();
                List<Object> objectList = new ArrayList();
                for (String s : res.result()) {
                  objectList.add(s);
                }
                pnlo.setPermissionNames(objectList);
                pnlo.setTotalRecords(res.result().size());
                promise.complete(pnlo);
              } else {
                getAllFullPermissions(res.result(), vertxContext, tenantId).onComplete(res2 -> {
                  if (res2.failed()) {
                    promise.fail(res2.cause());
                    return;
                  }
                  promise.complete(res2.result());
                });
              }
            });
          });
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<Permission> expandSubPermissions(Permission permission,
                                                  Context vertxContext, String tenantId) {

    logger.debug("Expanding subPermissions for " + permission.getPermissionName());
    List<Object> subPerms = permission.getSubPermissions();
    if (subPerms.isEmpty()) {
      return Future.succeededFuture(permission);
    }
    Promise<Permission> promise = Promise.promise();
    List<Object> newSubPerms = new ArrayList<>();
    List<Future> futureList = new ArrayList<>();
    for (Object o : subPerms) {
      Future<Permission> subPermFuture = getFullPermissions((String) o, vertxContext, tenantId);
      futureList.add(subPermFuture);
    }
    CompositeFuture compositeFuture = CompositeFuture.join(futureList);
    compositeFuture.onComplete(compositeResult -> {
      if (compositeResult.failed()) {
        logger.error("Failed to expand subpermissions for '" + permission.getPermissionName()
                + "' : " + compositeResult.cause().getMessage(),
            compositeResult.cause());
        promise.fail(compositeResult.cause().getMessage());
        return;
      }
      for (Future f : futureList) {
        if (f.result() != null) {
          newSubPerms.add(f.result());
        }
      }
      permission.setSubPermissions(newSubPerms);
      promise.complete(permission);
    });
    return promise.future();
  }

  /* If we are modifying a permissions user or creating a new one, we need to
  check for any changes to the permissions list. For any changes, we need to
  add or delete from the permission's "grantedTo" field
   */
  protected static Future<Void> updateUserPermissions(AsyncResult<SQLConnection> connection, String permUserId,
                                                      JsonArray originalList, JsonArray newList, Context vertxContext,
                                                      String tenantId) {
    Promise<Void> promise = Promise.promise();
    JsonArray missingFromOriginalList = new JsonArray();
    JsonArray missingFromNewList = new JsonArray();
    for (Object ob : newList) {
      if (!originalList.contains(ob)) {
        missingFromOriginalList.add(ob);
      }
    }
    for (Object ob : originalList) {
      if (!newList.contains(ob)) {
        missingFromNewList.add(ob);
      }
    }
    Future<List<String>> checkExistsFuture = findMissingPermissionsFromList(
        connection, missingFromOriginalList.getList(), vertxContext,
        tenantId, null);
    checkExistsFuture.onComplete(checkExistsRes -> {
      if (checkExistsFuture.failed()) {
        promise.fail(checkExistsFuture.cause());
        return;
      }
      if (!checkExistsFuture.result().isEmpty()) {
        promise.fail(
            new InvalidPermissionsException(String.format(
                "Attempting to add non-existent permissions %s to permission user with id %s",
                String.join(",", checkExistsFuture.result()), permUserId)));
        return;
      }
      List<FieldUpdateValues> fuvList = new ArrayList<>();
      for (Object permissionNameOb : missingFromOriginalList) {
        FieldUpdateValues fuv = new FieldUpdateValues(permUserId,
            (String) permissionNameOb,
            PermissionField.GRANTED_TO,
            Operation.ADD);
        fuvList.add(fuv);
      }
      for (Object permissionNameOb : missingFromNewList) {
        FieldUpdateValues fuv = new FieldUpdateValues(permUserId,
            (String) permissionNameOb,
            PermissionField.GRANTED_TO,
            Operation.DELETE);
        fuvList.add(fuv);
      }
      if (fuvList.isEmpty()) {
        promise.complete(); //Nuthin' to do
      } else {
        modifyPermissionArrayFieldList(connection, fuvList, vertxContext,
            tenantId).onComplete(res -> promise.handle(res.mapEmpty()));
      }
    });
    return promise.future();
  }

  /* If we are modifying (or creating) the subpermissions array of a permission
  object, check for any changes and for any newly declared subpermissions, add
  the permission name to the the 'childOf' field for those permisisons
   */
  protected static Future<Void> updateSubPermissions(AsyncResult<SQLConnection> connection,
                                                     String permissionName, JsonArray originalList, JsonArray newList,
                                                     Context vertxContext, String tenantId) {

    Promise<Void> promise = Promise.promise();
    try {
      JsonArray missingFromOriginalList = new JsonArray();
      JsonArray missingFromNewList = new JsonArray();
      for (Object ob : newList) {
        if (!originalList.contains(ob)) {
          missingFromOriginalList.add(ob);
        }
      }
      for (Object ob : originalList) {
        if (!newList.contains(ob)) {
          missingFromNewList.add(ob);
        }
      }
      Future<List<String>> checkExistsFuture = findMissingPermissionsFromList(
          connection, missingFromOriginalList.getList(), vertxContext,
          tenantId, null);
      checkExistsFuture.onComplete(res -> {
        if (res.failed()) {
          promise.fail(res.cause());
          return;
        }
        if (!res.result().isEmpty()) {
          promise.fail(new InvalidPermissionsException(String.format(
              "Attempting to add non-existent permissions %s as sub-permissions to permission %s",
              String.join(",", res.result()), permissionName)));
          return;
        }
        List<FieldUpdateValues> fuvList = new ArrayList<>();
        for (Object childPermissionNameOb : missingFromOriginalList) {
          FieldUpdateValues fuv = new FieldUpdateValues(
              (String) permissionName,
              (String) childPermissionNameOb,
              PermissionField.CHILD_OF,
              Operation.ADD);
          fuvList.add(fuv);
        }

        for (Object childPermissionNameOb : missingFromNewList) {
          FieldUpdateValues fuv = new FieldUpdateValues(
              (String) permissionName,
              (String) childPermissionNameOb,
              PermissionField.CHILD_OF,
              Operation.DELETE);
          fuvList.add(fuv);
        }

        if (fuvList.isEmpty()) {
          promise.complete();
        } else {
          modifyPermissionArrayFieldList(connection, fuvList, vertxContext,
              tenantId).onComplete(res2 -> promise.handle(res2.mapEmpty()));
        }
      });
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  protected static Future<Void> modifyPermissionArrayField(AsyncResult<SQLConnection> connection, String fieldValue,
                                                         String permissionName, PermissionField field, Operation operation,
                                                         Context vertxContext, String tenantId) {
    Promise<Void> promise = Promise.promise();
    try {
      Criteria nameCrit = new Criteria()
          .addField(PERMISSION_NAME_FIELD)
          .setOperation("=")
          .setVal(permissionName);
      Criterion criterion = new Criterion(nameCrit);
      CQLWrapper cqlFilter = new CQLWrapper(criterion);
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
          connection, TABLE_NAME_PERMS,
          Permission.class, criterion, true, false,
          getReply -> {
            if (getReply.failed()) {
              promise.fail(getReply.cause());
              return;
            }
            List<Permission> permList = getReply.result().getResults();
            if (permList.size() != 1) {
              promise.fail("Expected one result for " + PERMISSION_NAME_FIELD
                  + ": '" + permissionName + "', got " + permList.size()
                  + " results");
              return;
            }
            Permission permission = permList.get(0);
            List valueList;
            if (field == PermissionField.CHILD_OF) {
              valueList = permission.getChildOf();
            } else {
              valueList = permission.getGrantedTo();
            }
            logger.info("Performing " + operation.toString()
                + " operation on " + field.toString() + " of permission "
                + permissionName + " with value " + fieldValue);
            boolean modified = false;
            if (operation == Operation.ADD) {
              if (!valueList.contains(fieldValue)) {
                valueList.add(fieldValue);
                modified = true;
              }
            } else {
              if (valueList.contains(fieldValue)) {
                valueList.remove(fieldValue);
                modified = true;
              }
            }
            if (modified) {
              try {
                PostgresClient.getInstance(vertxContext.owner(), tenantId).update(
                    connection, TABLE_NAME_PERMS, permission, cqlFilter,
                    true, updateReply -> promise.handle(updateReply.mapEmpty()));
              } catch (Exception e) {
                promise.fail(e);
              }
            } else {
              promise.complete(); //Nothing more to do, no modification to list
            }
          });
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private static Future<Void> modifyPermissionArrayFieldList(
      AsyncResult<SQLConnection> connection, List<FieldUpdateValues> fuvList, Context vertxContext,
      String tenantId) {

    if (fuvList.isEmpty()) {
      return Future.succeededFuture();
    }
    Promise<Void> promise = Promise.promise();
    FieldUpdateValues fuv = fuvList.get(0);
    List<FieldUpdateValues> fuvListCopy = new ArrayList<>(fuvList);
    fuvListCopy.remove(0); //pop
    Future<Void> modifyPermArrayFieldFuture = modifyPermissionArrayField(connection,
        fuv.getFieldValue(), fuv.getPermissionName(), fuv.getField(),
        fuv.getOperation(), vertxContext, tenantId);
    modifyPermArrayFieldFuture.onComplete(res -> promise.handle(res.mapEmpty()));
    return promise.future().compose(mapper -> modifyPermissionArrayFieldList(connection,
        fuvListCopy, vertxContext, tenantId));
  }

  private Future<Void> removePermissionFromUserList(
      AsyncResult<SQLConnection> connection, String permissionName, List<String> userIdList, Context vertxContext,
      String tenantId) {

    if (userIdList.isEmpty()) {
      return Future.succeededFuture();
    }
    List<String> userIdListCopy = new ArrayList<>(userIdList);
    Promise<Void> promise = Promise.promise();
    String userId = userIdListCopy.get(0);
    userIdListCopy.remove(0);
    removePermissionFromUser(connection, permissionName, userId, vertxContext,
        tenantId).onComplete(rpfuRes -> promise.handle(rpfuRes.mapEmpty()));
    return promise.future().compose(res -> removePermissionFromUserList(connection, permissionName, userIdListCopy,
        vertxContext, tenantId));
  }

  private Future<PermissionUser> getPermissionUserByUserId(AsyncResult<SQLConnection> connection,
      Criterion criterion, Context vertxContext, String tenantId) {
    Promise<PermissionUser> promise = Promise.promise();
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    pgClient.get(connection, TABLE_NAME_PERMSUSERS, PermissionUser.class, criterion, true, false,
        getReply -> {
          if (getReply.failed()) {
            promise.fail(getReply.cause());
            return;
          }
          List<PermissionUser> permUserList = getReply.result().getResults();
          if (permUserList.isEmpty()) {
            promise.complete(); // No need to update non-existent user
            return;
          }
          promise.complete(permUserList.get(0));          
        });
    return promise.future();
  }
  
  protected Future<Void> renamePermissionForUser(AsyncResult<SQLConnection> connection,
      String currentPermName, String newPermName, String userId, Context vertxContext,
      String tenantId) {
    Promise<Void> promise = Promise.promise();
    try {
      Criterion criterion = getIdCriterion(userId);
      CQLWrapper cqlFilter = new CQLWrapper(criterion);
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
      getPermissionUserByUserId(connection, criterion, vertxContext, tenantId)
        .onComplete(ar -> {
          if (ar.failed()) {
            promise.fail(ar.cause());
            return;
          }
          PermissionUser user = ar.result();
          user.getPermissions().remove(currentPermName);
          user.getPermissions().add(newPermName);
          pgClient.update(connection, TABLE_NAME_PERMSUSERS, user, cqlFilter, true, updateReply -> {
            if (updateReply.failed()) {
              promise.fail(updateReply.cause());
              return;
            }
            promise.complete();            
          });
      });
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  protected Future<Void> removePermissionFromUser(AsyncResult<SQLConnection> connection,
      String permissionName, String userId, Context vertxContext, String tenantId) {

    Promise<Void> promise = Promise.promise();
    try {
      Criterion criterion = getIdCriterion(userId);
      CQLWrapper cqlFilter = new CQLWrapper(criterion);
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
      getPermissionUserByUserId(connection, criterion, vertxContext, tenantId)
        .onComplete(ar -> {
          if (ar.failed()) {
            promise.fail(ar.cause());
            return;
          }
          PermissionUser user = ar.result();
          if (!user.getPermissions().contains(permissionName)) {
            promise.complete(); // User already lacks the permissions
          } else {
            user.getPermissions().remove(permissionName);
            pgClient.update(connection, TABLE_NAME_PERMSUSERS, user, cqlFilter, true,
                updateReply -> promise.handle(updateReply.mapEmpty()));
          }
        });
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<Void> removeSubpermissionFromPermissionList(
      AsyncResult<SQLConnection> connection, String subpermissionName, List<String> permissionNameList,
      Context vertxContext, String tenantId) {

    if (permissionNameList.isEmpty()) {
      return Future.succeededFuture();
    }
    List<String> permissionNameListCopy = new ArrayList<>(permissionNameList);
    String permissionName = permissionNameListCopy.get(0);
    permissionNameListCopy.remove(0);
    Promise promise = Promise.promise();
    removeSubpermissionFromPermission(connection, subpermissionName, permissionName,
        vertxContext, tenantId).onComplete(rsfpRes -> promise.handle(rsfpRes.mapEmpty()));
    return promise.future().compose(res -> removeSubpermissionFromPermissionList(connection, subpermissionName,
        permissionNameListCopy, vertxContext, tenantId));
  }

  private Future<Void> removeSubpermissionFromPermission(
      AsyncResult<SQLConnection> connection, String subpermissionName, String permissionName,
      Context vertxContext, String tenantId) {

    Promise promise = Promise.promise();
    try {
      Criteria nameCrit = new Criteria()
          .addField(PERMISSION_NAME_FIELD)
          .setOperation("=")
          .setVal(permissionName);
      Criterion criterion = new Criterion(nameCrit);
      CQLWrapper cqlFilter = new CQLWrapper(criterion);
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(),
          tenantId);
      pgClient.get(connection, TABLE_NAME_PERMS, Permission.class,
          criterion, true, false, getReply -> {
            if (getReply.failed()) {
              promise.fail(getReply.cause());
              return;
            }
            if (getReply.result().getResults().isEmpty()) {
              promise.complete();
              return;
            }
            Permission permission = getReply.result().getResults().get(0);
            permission.getSubPermissions().remove(subpermissionName);
              pgClient.update(connection, TABLE_NAME_PERMS, permission, cqlFilter,
                  true, updateReply -> promise.handle(updateReply.mapEmpty()));
          });
    } catch (Exception e) {
      promise.fail(e);
    }
    return promise.future();
  }

  private Future<Permission> retrievePermissionByName(String permissionName, Context vertxContext, String tenantId) {

    Promise<Permission> promise = Promise.promise();
    Criteria nameCrit = new Criteria()
        .addField(PERMISSION_NAME_FIELD)
        .setOperation("=")
        .setVal(permissionName);
    PostgresClient.getInstance(vertxContext.owner(), tenantId)
        .get(TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit),
            true, false, getReply -> {
              if (getReply.failed()) {
                promise.fail(getReply.cause());
              } else {
                Permission permission = null;
                try {
                  permission = getReply.result().getResults().get(0);
                } catch (Exception e) {
                  permission = null;
                }
                promise.complete(permission);
              }
            });
    return promise.future();
  }

  private Future<Boolean> checkPermlistForDummy(List<Object> permList,
                                                Context vertxContext, String tenantId) {

    Promise<Boolean> promise = Promise.promise();
    if (permList.isEmpty()) {
      return Future.succeededFuture(false);
    }
    List<Object> permListCopy = new ArrayList<>(permList);
    String permissionName = (String) permListCopy.get(0);
    permListCopy.remove(0);
    retrievePermissionByName(permissionName, vertxContext, tenantId).onComplete(
        rpbnRes -> {
          if (rpbnRes.failed()) {
            promise.fail(rpbnRes.cause());
            return;
          }
          if (rpbnRes.result() == null) {
            promise.complete(false);
          } else {
            promise.complete(Boolean.TRUE.equals(rpbnRes.result().getDummy()));
          }
        });
    return promise.future().compose(next -> {
      if (Boolean.TRUE.equals(next)) {
        return Future.succeededFuture(true);
      } else {
        return checkPermlistForDummy(permListCopy, vertxContext, tenantId);
      }
    });
  }

  private static Criterion getIdCriterion(String id) {
    Criteria idCrit = new Criteria();
    idCrit.addField(ID_FIELD);
    idCrit.setJSONB(false);
    idCrit.setOperation("=");
    idCrit.setVal(id);
    return new Criterion(idCrit);
  }

  private static Criterion getIdCriterion(String indexField, String value) {
    Criteria crit = new Criteria();
    if (indexField == null || indexField.equals("id")) {
      crit.addField(ID_FIELD);
      crit.setJSONB(false);
    } else if (indexField.equals("userId")) {
      crit.addField(USER_ID_FIELD);
    } else {
      throw new IllegalArgumentException("Invalid value '" + indexField + "' for indexField");
    }
    crit.setOperation("=");
    crit.setVal(value);
    return new Criterion(crit);
  }

  private static Permission getRealPermObject(PermissionUpload entity) {
    Permission perm = new Permission();
    perm.setId(entity.getId());
    perm.setPermissionName(entity.getPermissionName());
    perm.setDisplayName(entity.getDisplayName());
    perm.setDescription(entity.getDescription());
    List<Object> subPerms = new ArrayList<>();
    for (String s : entity.getSubPermissions()) {
      subPerms.add(s);
    }
    perm.setSubPermissions(subPerms);
    perm.setMutable(entity.getMutable());
    perm.setVisible(entity.getVisible());
    perm.setTags(entity.getTags());
    perm.setMetadata(entity.getMetadata());
    return perm;
  }

  static List<List<String>> splitStringList(List<String> stringList, int chunkSize) {
    List<List<String>> listOfLists = new ArrayList<>();
    int count = 0;
    List<String> currentChunk = new ArrayList<>();
    for (String string : stringList) {
      count++;
      currentChunk.add(string);
      if (count == chunkSize) {
        listOfLists.add(currentChunk);
        currentChunk = new ArrayList<>();
        count = 0;
      }
    }
    if (!currentChunk.isEmpty()) {
      listOfLists.add(currentChunk);
    }
    return listOfLists;
  }

  private static Criterion buildPermissionNameListQuery(List<String> permissionNameList) {
    Criterion criterion = null;
    for (String permissionName : permissionNameList) {
      Criteria nameCrit = new Criteria()
          .addField(PERMISSION_NAME_FIELD)
          .setOperation("=")
          .setVal(permissionName);
      if (criterion == null) {
        criterion = new Criterion(nameCrit);
      } else {
        criterion.addCriterion(nameCrit, "OR");
      }
    }
    return criterion;
  }
}
