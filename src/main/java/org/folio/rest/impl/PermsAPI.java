package org.folio.rest.impl;

import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import javax.ws.rs.core.Response;

import io.vertx.core.json.DecodeException;
import io.vertx.core.json.JsonObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.cql2pgjson.CQL2PgJSON;
import org.folio.cql2pgjson.exception.FieldException;
import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.PermissionListObject;
import org.folio.rest.jaxrs.model.PermissionNameListObject;
import org.folio.rest.jaxrs.model.PermissionNameObject;
import org.folio.rest.jaxrs.model.PermissionUpload;
import org.folio.rest.jaxrs.model.PermissionUser;
import org.folio.rest.jaxrs.resource.Perms;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.PgUtil;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.cql.CQLWrapper;
import org.folio.rest.tools.utils.TenantTool;
import org.folio.rest.tools.utils.ValidationHelper;
import io.vertx.core.AsyncResult;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonArray;
import io.vertx.ext.web.RoutingContext;

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

  public static class InvalidPermissionsException extends RuntimeException {
    private String field;
    private String value;

    String getField(){
      return field;
    }

    String getValue(){
      return value;
    }

    public InvalidPermissionsException(String field, String value, String message) {
      super(message);
      this.field = field;
      this.value = value;
    }
  }

  public static class NotFoundException extends RuntimeException {

    public NotFoundException(String message) {
      super(message);
    }
  }

  public static class OperatingUserException extends RuntimeException {

    public OperatingUserException(String message) {
      super(message);
    }
  }

  public static class FieldUpdateValues {

    private final String fieldValue;
    private final String permissionName;
    private final PermissionField field;
    private final Operation operation;

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
  private static final Logger logger = LogManager.getLogger(PermsAPI.class);

  private static CQLWrapper getCQL(String query, String tableName, int limit, int offset) {
    try {
      CQL2PgJSON cql2pgJson = new CQL2PgJSON(tableName + ".jsonb");
      return new CQLWrapper(cql2pgJson, query).setLimit(new Limit(limit)).setOffset(new Offset(offset));
    } catch (FieldException e) {
      throw new RuntimeException(e);
    }
  }

  protected static CQLWrapper getCQL(String query, String tableName) {
    try {
      CQL2PgJSON cql2pgJson = new CQL2PgJSON(tableName + ".jsonb");
      return new CQLWrapper(cql2pgJson, query);
    } catch (FieldException e) {
      throw new RuntimeException(e);
    }
  }

  @Validate
  @Override
  public void getPermsUsers(int offset, int limit, int length, int start, String sortBy, String query,
      String hasPermissions, RoutingContext routingContext, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    if (length != 10) {
      limit = length;
    }
    if (start != 1) {
      offset = start - 1;
    }
    PgUtil.streamGet(TABLE_NAME_PERMSUSERS, PermissionUser.class, query, offset, limit, null, "permissionUsers",
        routingContext, okapiHeaders, vertxContext);
  }

  @Validate
  @Override
  public void postPermsUsers(PermissionUser entity, RoutingContext routingContext, Map<String, String> okapiHeaders,
                             Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      if (entity.getId() == null) {
        entity.setId(UUID.randomUUID().toString());
      }
      postPermsUsersTrans(entity, vertxContext, okapiHeaders, asyncResultHandler);
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          PostPermsUsersResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  void postPermsUsersTrans(PermissionUser entity, Context vertxContext,
      Map<String,String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler) {
    String tenantId = TenantTool.tenantId(okapiHeaders);
    final PermissionUser permUser = entity;
    PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    postgresClient.withTrans(conn ->
            conn.save(TABLE_NAME_PERMSUSERS, entity.getId(), entity).compose(
                postReply ->
                    updateUserPermissions(conn, permUser.getId(), new JsonArray(),
                        new JsonArray(permUser.getPermissions()), vertxContext, tenantId, okapiHeaders))
        )
        .onFailure(cause -> {
          logger.error("Error updating derived fields: {}",
              cause.getMessage(), cause);
          if (cause instanceof InvalidPermissionsException) {
            asyncResultHandler.handle(Future.succeededFuture(
                PostPermsUsersResponse.respond422WithApplicationJson(
                    ValidationHelper.createValidationErrorMessage(
                        ID_FIELD, permUser.getId(),
                        UNABLE_TO_UPDATE_DERIVED_FIELDS + cause.getMessage()))));
          } else if (cause instanceof OperatingUserException) {
            asyncResultHandler.handle(Future.succeededFuture(
                PostPermsUsersResponse.respond403WithTextPlain(cause.getMessage())));
          } else {
            asyncResultHandler.handle(Future.succeededFuture(
                PostPermsUsersResponse.respond400WithTextPlain(
                    cause.getMessage())));
          }})
        .onSuccess(res -> asyncResultHandler.handle(Future.succeededFuture(
            PostPermsUsersResponse.respond201WithApplicationJson(entity)))
        );
  }

  static Future<PermissionUser> lookupPermsUsersById(String id, String indexField, String tenantId, Context vertxContext) {
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    return pgClient.withConn(connection -> lookupPermsUsersById(id, indexField, connection));
  }

  static Future<PermissionUser> lookupPermsUsersById(String id, String indexField, Conn connection) {
    Criterion idCrit = getIdCriterion(indexField, id);
    return connection.get(TABLE_NAME_PERMSUSERS, PermissionUser.class, idCrit, true)
        .map(x -> x.getResults().isEmpty() ? null : x.getResults().get(0));
  }

  @Validate
  @Override
  public void getPermsUsersById(String id, String indexField, Map<String, String> okapiHeaders,
                                Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      lookupPermsUsersById(id, indexField, tenantId, vertxContext)
          .onSuccess(user -> {
            if (user == null) {
              asyncResultHandler.handle(Future.succeededFuture(
                  GetPermsUsersByIdResponse.respond404WithTextPlain("No user with " + getUserIdMessage(indexField, id))));
            } else {
              asyncResultHandler.handle(Future.succeededFuture(
                  GetPermsUsersByIdResponse.respond200WithApplicationJson(user)));
            }
          })
          .onFailure(cause -> {
            String errStr = cause.getMessage();
            logger.error(errStr, cause);
            asyncResultHandler.handle(Future.succeededFuture(GetPermsUsersByIdResponse.respond400WithTextPlain(errStr)));
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          GetPermsUsersByIdResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  @Validate
  @Override
  public void putPermsUsersById(String id, PermissionUser entity, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      checkPermlistForDummy(entity.getPermissions(), vertxContext, tenantId)
          .compose(result -> {
            if (Boolean.TRUE.equals(result)) {
              throw new RuntimeException("Cannot add permissions flagged as 'dummy' to users");
            }
            return PostgresClient.getInstance(vertxContext.owner(), tenantId)
                .withConn(conn -> conn.getById(TABLE_NAME_PERMSUSERS, id, PermissionUser.class))
                .compose(getUser -> putPermsUsersbyIdHandle(getUser, id, entity,
                    vertxContext, tenantId, okapiHeaders)
                );
          })
          .onSuccess(res -> {
            // https://issues.folio.org/browse/MODPERMS-99
            // Remove inconsistent metadata - the update trigger uses different data
            entity.setMetadata(null);
            asyncResultHandler.handle(Future.succeededFuture(
                PutPermsUsersByIdResponse.respond200WithApplicationJson(entity)));
          })
          .onFailure(cause -> {
            if (cause instanceof InvalidPermissionsException) {
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsUsersByIdResponse.respond422WithApplicationJson(
                      ValidationHelper.createValidationErrorMessage(
                          ID_FIELD, entity.getId(),
                          UNABLE_TO_UPDATE_DERIVED_FIELDS + cause.getMessage()))));
            } else if (cause instanceof OperatingUserException) {
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsUsersByIdResponse.respond403WithTextPlain(cause.getMessage())));
            } else if (cause instanceof NotFoundException) {
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsUsersByIdResponse.respond404WithTextPlain(cause.getMessage())));
            } else {
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsUsersByIdResponse.respond400WithTextPlain(cause.getMessage())));
            }
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(
          Future.succeededFuture(PutPermsUsersByIdResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  private Future<Void> putPermsUsersbyIdHandle(PermissionUser originalUser,
      String id, PermissionUser entity, Context vertxContext,
      String tenantId, Map<String,String> okapiHeaders) {

    if (originalUser == null) {
      throw new NotFoundException("No permissions user found with id " + id);
    }
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    return pgClient.withTrans(connection ->
        connection.update(TABLE_NAME_PERMSUSERS, entity, id)
            .compose(updateReply ->
                updateUserPermissions(connection, id, new JsonArray(originalUser.getPermissions()),
                    new JsonArray(entity.getPermissions()), vertxContext, tenantId, okapiHeaders)
            ));
  }

  @Validate
  @Override
  public void deletePermsUsersById(String id, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
      pgClient
          .withTrans(connection ->
              connection.getById(TABLE_NAME_PERMSUSERS, id, PermissionUser.class)
                  .compose(permUser -> {
                    if (permUser == null) {
                      throw new NotFoundException("No permissions user found with id " + id);
                    }
                    return updateUserPermissions(connection, id,
                        new JsonArray(permUser.getPermissions()), new JsonArray(),
                        vertxContext, tenantId, okapiHeaders);
                  })
                  .compose(x -> connection.delete(TABLE_NAME_PERMSUSERS, id))
          )
          .onSuccess(res ->
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsUsersByIdResponse.respond204()))
          )
          .onFailure(cause -> {
            if (cause instanceof NotFoundException) {
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsUsersByIdResponse.respond404WithTextPlain(cause.getMessage())));
            } else {
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsUsersByIdResponse.respond400WithTextPlain(cause.getMessage())));
            }
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
                  "No user found by " + getUserIdMessage(indexField, id))));
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
          PermissionUser.class, useridCrit, true)
          .compose(result -> {
            List<PermissionUser> userList = result.getResults();
            if (userList.isEmpty()) {
              throw new RuntimeException("User with " + getUserIdMessage(indexField, id) + " does not exist");
            }
            //now we can actually add it
            String permissionName = entity.getPermissionName();
            PermissionUser user = userList.get(0);
            String actualId = user.getId();
            JsonArray originalPermissions = new JsonArray(
                new ArrayList<>(user.getPermissions()));
            if (user.getPermissions().contains(permissionName)) {
              throw new InvalidPermissionsException(USER_ID_FIELD, actualId,
                  "User with " + getUserIdMessage(indexField, id) + " already has permission " + permissionName);
            }
            return updatePermissionsForUser(entity, vertxContext, tenantId, okapiHeaders,
                permissionName, user, actualId, originalPermissions);
          })
          .onSuccess(res -> {
            asyncResultHandler.handle(Future.
                succeededFuture(
                    PostPermsUsersPermissionsByIdResponse
                        .respond200WithApplicationJson(entity)));
          })
          .onFailure(cause -> {
            if (cause instanceof OperatingUserException) {
              asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsUsersPermissionsByIdResponse.respond403WithTextPlain(cause.getMessage())
              ));
            } else if (cause instanceof InvalidPermissionsException) {
              InvalidPermissionsException epe = (InvalidPermissionsException) cause;
              asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsUsersPermissionsByIdResponse.respond422WithApplicationJson(ValidationHelper
                      .createValidationErrorMessage(epe.getField(), epe.getValue(), cause.getMessage()))));
            } else {
              asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsUsersPermissionsByIdResponse.respond400WithTextPlain(cause.getMessage())));
            }
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          PostPermsUsersPermissionsByIdResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  @SuppressWarnings({"squid:S00107"})   // Method has more than 7 parameters
  private Future<Void> updatePermissionsForUser(PermissionNameObject entity,
      Context vertxContext, String tenantId, Map<String,String> okapiHeaders, String permissionName,
      PermissionUser user, String actualId, JsonArray originalPermissions) {

    return retrievePermissionByName(permissionName, vertxContext, tenantId)
        .compose(result -> {
          if (result == null) {
            throw new RuntimeException(String.format("Permission by name '%s' does not exist",
                permissionName));
          }
          if (Boolean.TRUE.equals(result.getDummy())) {
            throw new RuntimeException(String.format("'%s' is flagged as a dummy permission"
                + " and cannot be assigned to a user", permissionName));
          }
          user.getPermissions().add(permissionName);
          PostgresClient pgClient = PostgresClient.getInstance(
              vertxContext.owner(), tenantId);
          return pgClient.withTrans(connection ->
              connection.update(TABLE_NAME_PERMSUSERS, user, actualId)
                  .compose(reply ->
                      updateUserPermissions(connection, actualId, originalPermissions,
                          new JsonArray(user.getPermissions()), vertxContext,
                          tenantId, okapiHeaders))
          );
        });
  }

  static String getUserIdMessage(String indexField, String id) {
    return (indexField == null ? "id" : indexField) + " " + id;
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
              PermissionUser.class, idCrit, true)
          .compose(result -> {
            List<PermissionUser> userList = result.getResults();
            if (userList.isEmpty()) {
              throw new NotFoundException("User with " + getUserIdMessage(indexField, id) + " does not exist");
            }
            //attempt to delete permission
            PermissionUser user = userList.get(0);
            if (!user.getPermissions().contains(permissionName)) {
              throw new RuntimeException("User with " + getUserIdMessage(indexField, id) + " does not contain " + permissionName);
            }
            JsonArray originalPermissions = new JsonArray(
                new ArrayList<>(user.getPermissions()));
            user.getPermissions().remove(permissionName);
            PostgresClient pgClient = PostgresClient.getInstance(
                vertxContext.owner(), tenantId);

            return pgClient.withTrans(connection ->
                connection.update(TABLE_NAME_PERMSUSERS, user, user.getId())
                    .compose(res -> updateUserPermissions(connection, user.getId(), originalPermissions,
                        new JsonArray(user.getPermissions()), vertxContext,
                        tenantId, okapiHeaders)
                    ));
          }).onFailure(cause -> {
            if (cause instanceof NotFoundException) {
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsPermissionsByIdResponse.respond404WithTextPlain(cause.getMessage())));
            } else {
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsPermissionsByIdResponse.respond400WithTextPlain(cause.getMessage())));
            }
          }).onSuccess(res ->
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsPermissionsByIdResponse.respond204()))
          );
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
      PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
      postgresClient.get(TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit), false)
          .compose(result -> {
            List<Permission> permissionList = result.getResults();
            if (!permissionList.isEmpty()) {
              throw new InvalidPermissionsException(PERMISSION_NAME_FIELD, entity.getPermissionName(),
                  "Permission with name " + entity.getPermissionName() + " already exists");
            }
            //Do the actual POST of the new permission
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
            entity.setMutable(true); //MODPERMS-126
            Permission realPerm = getRealPermObject(entity);
            realPerm.setDummy(false);
            return postgresClient.withTrans(connection ->
                connection.save(TABLE_NAME_PERMS, newId, realPerm)
                    .compose(res -> updateSubPermissions(connection, entity.getPermissionName(),
                        new JsonArray(), new JsonArray(entity.getSubPermissions()), null,
                        vertxContext,  tenantId)));
          }).onFailure(cause -> {
            if (cause instanceof InvalidPermissionsException) {
              InvalidPermissionsException epe = (InvalidPermissionsException) cause;
              asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsPermissionsResponse.respond422WithApplicationJson(ValidationHelper
                      .createValidationErrorMessage(epe.getField(), epe.getValue(), cause.getMessage()))));
            } else {
              asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsPermissionsResponse.respond400WithTextPlain(cause.getMessage())));
            }
          }).onSuccess(res ->
              asyncResultHandler.handle(Future.succeededFuture(
                  PostPermsPermissionsResponse.respond201WithApplicationJson(entity)))
          );
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
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
      pgClient.getById(TABLE_NAME_PERMS, id, Permission.class)
          .compose(perm -> {
            if (perm == null) {
              throw new NotFoundException("No permission found to match id " + id);
            }
            entity.setMutable(true); // MODPERMS-126
            Permission updatePerm = getRealPermObject(entity);
            updatePerm.setId(entity.getId());
            updatePerm.setChildOf(perm.getChildOf());
            updatePerm.setGrantedTo(perm.getGrantedTo());
            if (!perm.getPermissionName().equals(entity.getPermissionName())) {
              throw new RuntimeException("permission name property cannot change");
            }
            if (Boolean.FALSE.equals(perm.getMutable())) {
              throw new RuntimeException("cannot change an immutable permission");
            }
            updatePerm.setDummy(false);
            return pgClient.withTrans(connection ->
                connection.update(TABLE_NAME_PERMS, updatePerm, id)
                    .compose(res ->
                        updateSubPermissions(connection, entity.getPermissionName(),
                            new JsonArray(perm.getSubPermissions()),
                            new JsonArray(entity.getSubPermissions()),
                            okapiHeaders, vertxContext, tenantId)));
          })
          .onFailure(cause -> {
            if (cause instanceof InvalidPermissionsException) {
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsPermissionsByIdResponse.respond422WithApplicationJson(
                      ValidationHelper.createValidationErrorMessage(
                          ID_FIELD, entity.getId(),
                          UNABLE_TO_UPDATE_DERIVED_FIELDS + cause.getMessage()))));
            } else if (cause instanceof OperatingUserException) {
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsPermissionsByIdResponse.respond403WithTextPlain(cause.getMessage())));
            } else if (cause instanceof NotFoundException) {
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsPermissionsByIdResponse.respond404WithTextPlain(cause.getMessage())));
            } else {
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsPermissionsByIdResponse.respond400WithTextPlain(cause.getMessage())));
            }
          })
          .onSuccess(res ->
              asyncResultHandler.handle(Future.succeededFuture(
                  PutPermsPermissionsByIdResponse.respond200WithApplicationJson(entity)))
          );
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
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
      pgClient.getById(TABLE_NAME_PERMS, id, Permission.class)
          .compose(perm -> {
            if (perm == null) {
              throw new NotFoundException("No permission found to match id " + id);
            }
            if (Boolean.FALSE.equals(perm.getMutable())) {
              throw new RuntimeException("cannot delete an immutable permission");
            }
            if (!perm.getChildOf().isEmpty() || !perm.getGrantedTo().isEmpty()) {
              return pgClient.withTrans(connection -> {
                List<String> parentPermissionList = new ArrayList<>();
                for (Object ob : perm.getChildOf()) {
                  parentPermissionList.add((String) ob);
                }
                List<String> userIdList = new ArrayList<>();
                for (Object ob : perm.getGrantedTo()) {
                  userIdList.add((String) ob);
                }
                return removePermissionFromUserList(connection, perm.getPermissionName(), userIdList)
                    .compose(rpfulRes ->
                        removeSubpermissionFromPermissionList(connection,
                            perm.getPermissionName(), parentPermissionList)
                            .compose(rsfplRes -> connection.delete(TABLE_NAME_PERMS, id))
                    );
              });
            } else {
              return pgClient.delete(TABLE_NAME_PERMS, id)
                  .map(permResult -> {
                    if (permResult.rowCount() == 0) {
                      throw new NotFoundException(id);
                    }
                    return null;
                  }).mapEmpty();
            }
          }).onSuccess(res ->
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsPermissionsByIdResponse.respond204()))
          ).onFailure(cause -> {
            if (cause instanceof NotFoundException) {
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsPermissionsByIdResponse.respond404WithTextPlain(cause.getMessage())));
            } else {
              asyncResultHandler.handle(Future.succeededFuture(
                  DeletePermsPermissionsByIdResponse.respond400WithTextPlain(cause.getMessage())));
            }
          });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(Future.succeededFuture(
          DeletePermsPermissionsByIdResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  @SuppressWarnings("java:S3776")
  @Validate
  @Override
  public void getPermsPermissions(String expandSubs, String expanded, String includeDummy,
      int offset, int limit, int length, int start, String sortBy,
      String query0, String memberOf, String ownedBy, Map<String, String> okapiHeaders,
      Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) {

    try {
      if (length != 10) {
        limit = length;
      }
      if (start != 1) {
        offset = start - 1;
      }
      boolean includeDummyPerms = "true".equals(includeDummy);
      String query = query0 == null ? "" : query0;
      if (!includeDummyPerms) {
        //filter out all dummy perms from query
        if (query.isEmpty()) {
          query = "(dummy == false)";
        } else {
          query = String.format("(%s) AND (dummy==false)", query);
        }
      }
      logger.info("Generating cql to request rows from table '{}' with query '{}'",
          TABLE_NAME_PERMS, query);
      CQLWrapper cql = getCQL(query, TABLE_NAME_PERMS, limit, offset);
      String tenantId = TenantTool.tenantId(okapiHeaders);
      String[] fieldList = {"*"};
      PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
          Permission.class, fieldList, cql, true).compose(result -> {
        PermissionListObject permCollection = new PermissionListObject();
        List<Permission> permissions = result.getResults();
        List<Future<Permission>> futureList = new ArrayList<>();
        for (Permission permission : permissions) {
          Future<Permission> permFuture;
          if ("true".equals(expandSubs)) {
            permFuture = expandSubPermissions(permission, vertxContext, tenantId);
          } else if ("true".equals(expanded)) {
            List<String> subperms = new ArrayList<>(permission.getSubPermissions().size());
            permission.getSubPermissions().forEach(sub -> subperms.add(sub.toString()));
            permFuture = PermsCache.expandPerms(subperms, vertxContext, tenantId)
                .map(ar -> {
                  List<Object> list = new ArrayList<>(ar);
                  permission.setSubPermissions(list);
                  return permission;
                });
          } else {
            permFuture = Future.succeededFuture(permission);
          }
          futureList.add(permFuture);
        }
        CompositeFuture compositeFuture = GenericCompositeFuture.join(futureList);
        return compositeFuture.compose(compositeResult -> {
          List<Permission> newPermList = new ArrayList<>();
          for (Future<Permission> f : futureList) {
            newPermList.add(f.result());
          }
          permCollection.setPermissions(newPermList);
          permCollection.setTotalRecords(result.getResultInfo().getTotalRecords());
          return Future.succeededFuture(permCollection);
        });
      }).onSuccess(res ->
        asyncResultHandler.handle(Future.succeededFuture(
            GetPermsPermissionsResponse.respond200WithApplicationJson(res)))
      ).onFailure(cause ->
        asyncResultHandler.handle(Future.succeededFuture(
            GetPermsPermissionsResponse.respond400WithTextPlain(cause.getMessage())))
      );
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
          GetPermsPermissionsResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

  protected static Future<Boolean> checkPermissionExists(Conn connection, String permissionName) {
    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permissionName);
    return connection.get(TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit),
        false).map(result -> !result.getResults().isEmpty());
  }

  private static Future<List<String>> findMissingPermissionsFromList(
      Conn connection, List<Object> permissionList) {

    Future<List<String>> future = Future.succeededFuture(new ArrayList<>());
    for (Object o : permissionList) {
      String permissionName = (String) o;
      future = future.compose(x -> checkPermissionExists(connection, permissionName)
          .map(result -> {
            if (Boolean.FALSE.equals(result)) {
              x.add(permissionName);
            }
            return x;
          }));
    }
    return future;
  }

  private Future<List<String>> getAllExpandedPermissionsSequential(
      List<List<String>> listOfPermissionLists, Context vertxContext,
      String tenantId, List<String> returnedPermissions) {

    if (returnedPermissions == null) {
      returnedPermissions = new ArrayList<>();
    }
    if (listOfPermissionLists.isEmpty()) {
      return Future.succeededFuture(returnedPermissions);
    }
    Future<List<String>> future = Future.succeededFuture(returnedPermissions);
    for (List<String> permissionList: listOfPermissionLists) {
      future = future.compose(x -> getExpandedPermissionsSequential(permissionList, vertxContext, tenantId)
          .map(result -> {
            x.addAll(result);
            return x;
          }));
    }
    return future;
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

    Criterion criterion = buildPermissionNameListQuery(permissionList);
    PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    return pgClient.get(TABLE_NAME_PERMS, Permission.class, criterion, false).compose(result -> {

      Set<String> subPermSet = new HashSet<>();
      List<String> foundPermNameList = new ArrayList<>();
      List<Permission> foundPermList = result.getResults();
      for (Permission perm : foundPermList) {
        foundPermNameList.add(perm.getPermissionName());
        perm.getSubPermissions().stream()
            .map(object -> Objects.toString(object, null))
            .forEach(subPermSet::add);
      }
      List<List<String>> listOfSubPermLists = splitStringList(subPermSet, 15);
      return getAllExpandedPermissionsSequential(listOfSubPermLists, vertxContext,
          tenantId, foundPermNameList);
    });
  }

  private Future<PermissionNameListObject> getAllFullPermissions(List<String> nameList,
                                                                 Context vertxContext, String tenantId) {
    List<Future<Permission>> futureList = new ArrayList<>();
    for (String name : nameList) {
      futureList.add(getFullPermissions(name, vertxContext, tenantId));
    }
    CompositeFuture compositeFuture = GenericCompositeFuture.all(futureList);
    return compositeFuture.compose(res -> {
      PermissionNameListObject pnlo = new PermissionNameListObject();
      List<Object> permList = new ArrayList<>();
      for (Future<Permission> doneFuture : futureList) {
        Object result = doneFuture.result();
        if (result != null) {
          permList.add(result);
        }
      }
      pnlo.setPermissionNames(permList);
      return Future.succeededFuture(pnlo);
    });
  }

  private Future<Permission> getFullPermissions(String permissionName,
      Context vertxContext, String tenantId) {
    logger.debug("Getting full permissions for {}", permissionName);

    // use cache by default unless set to false explicitly
    Boolean usePermsCache = vertxContext.config().getBoolean(PermsCache.CACHE_HEADER);
    if (usePermsCache == null || usePermsCache) {
      return PermsCache.getFullPerms(permissionName, vertxContext, tenantId);
    }

    Criteria nameCrit = new Criteria();
    nameCrit.addField(PERMISSION_NAME_FIELD);
    nameCrit.setOperation("=");
    nameCrit.setVal(permissionName);
    return PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TABLE_NAME_PERMS,
        Permission.class, new Criterion(nameCrit), false).map(result -> {
      List<Permission> permList = result.getResults();
      return permList.isEmpty() ? null : permList.get(0);
    });
  }

  private Future<PermissionNameListObject> getPermissionsForUser(
      String userId, boolean expanded, boolean full,
      String indexField, String tenantId, Context vertxContext) {

    Criterion idCrit = getIdCriterion(indexField, userId);
    return PostgresClient.getInstance(vertxContext.owner(), tenantId).get(
        TABLE_NAME_PERMSUSERS, PermissionUser.class, idCrit,
        false).compose(result -> {
      List<PermissionUser> userList = result.getResults();
      if (userList.isEmpty()) {
        return Future.succeededFuture(null);
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
      return interimFuture.compose(res -> {
        if (!full) {
          PermissionNameListObject pnlo = new PermissionNameListObject();
          List<Object> objectList = new ArrayList(res);
          pnlo.setPermissionNames(objectList);
          pnlo.setTotalRecords(res.size());
          return Future.succeededFuture(pnlo);
        } else {
          return getAllFullPermissions(res, vertxContext, tenantId);
        }
      });
    });
  }

  private Future<Permission> expandSubPermissions(Permission permission,
      Context vertxContext, String tenantId) {

    logger.debug("Expanding subPermissions for {}", permission.getPermissionName());
    List<Object> subPerms = permission.getSubPermissions();
    if (subPerms.isEmpty()) {
      return Future.succeededFuture(permission);
    }
    List<Object> newSubPerms = new ArrayList<>();
    List<Future<Permission>> futureList = new ArrayList<>();
    for (Object o : subPerms) {
      Future<Permission> subPermFuture = getFullPermissions((String) o, vertxContext, tenantId);
      futureList.add(subPermFuture);
    }
    CompositeFuture compositeFuture = GenericCompositeFuture.join(futureList);
    return compositeFuture.map(compositeResult -> {
      for (Future<Permission> f : futureList) {
        if (f.result() != null) {
          newSubPerms.add(f.result());
        }
      }
      permission.setSubPermissions(newSubPerms);
      return permission;
    });
  }

  /* should use utility from Okapi, but it's not public */
  static JsonObject getPayloadWithoutValidation(String token) {
    if (token == null) {
      return null;
    }
    int idx1 = token.indexOf('.');
    if (idx1 == -1) {
      throw new IllegalArgumentException("Missing . separator for token");
    }
    idx1++;
    int idx2 = token.indexOf('.', idx1);
    if (idx2 == -1) {
      throw new IllegalArgumentException("Missing . separator for token");
    }
    String encodedJson = token.substring(idx1, idx2);
    String decodedJson = new String(Base64.getDecoder().decode(encodedJson));
    try {
      return new JsonObject(decodedJson);
    } catch (DecodeException e) {
      throw new IllegalArgumentException(e.getMessage());
    }
  }

  static void checkPermList(String type, List<String> failedPerms, String operatingUser,
      List<String> modulePermissions) {
    if (!failedPerms.isEmpty()) {
      throw new OperatingUserException(
          "Cannot add " + type + (failedPerms.size() > 1 ? " permissions " : " permission ")
              + String.join(", ", failedPerms) + " not owned by operating user "
              + operatingUser + (modulePermissions == null || modulePermissions.isEmpty() ?
              "" : ", modulePermissions: " + String.join(", ", modulePermissions)));
    }
  }

  static Future<Void> checkOperatingPermissions(JsonArray addedPermissions, String tenantId,
      Map<String,String> okapiHeaders, Context vertxContext) {

    if (okapiHeaders == null) { // when POSTing permission sets
      return Future.succeededFuture();
    }
    String token = okapiHeaders.get(XOkapiHeaders.TOKEN);
    if (token == null) { // auth not enabled
      return Future.succeededFuture();
    }
    String operatingUser = okapiHeaders.getOrDefault(XOkapiHeaders.USER_ID, "null");
    return getOperatingPermissions(vertxContext, tenantId, operatingUser)
        .compose(operatingPermissions -> {
          JsonObject tokenObject = getPayloadWithoutValidation(token);
          Future<List<String>> futurePerms;
          if (tokenObject != null) {
            JsonArray extra_permissions = tokenObject.getJsonArray("extra_permissions");
            List<String> extraPerms = new ArrayList<>();
            if (extra_permissions != null) {
              for (int i = 0; i < extra_permissions.size(); i++) {
                extraPerms.add(extra_permissions.getString(i));
              }
            }
            futurePerms = PermsCache.expandPerms(extraPerms, vertxContext, tenantId);
          } else {
            futurePerms = Future.succeededFuture(null);
          }
          return futurePerms.compose(modulePermissions -> {
            Set<String> combinedPermissions = new HashSet<>();
            combinedPermissions.addAll(operatingPermissions);
            if (modulePermissions != null) {
              combinedPermissions.addAll(modulePermissions);
            }
            boolean hasImmutable = combinedPermissions.contains(PermissionUtils.PERMS_USERS_ASSIGN_IMMUTABLE);
            boolean hasMutable = combinedPermissions.contains(PermissionUtils.PERMS_USERS_ASSIGN_MUTABLE);
            boolean hasOkapi = combinedPermissions.contains(PermissionUtils.PERMS_USERS_ASSIGN_OKAPI);
            Future<Void> future = Future.succeededFuture();
            List<String> failedImmutable = new ArrayList<>();
            List<String> failedMutable = new ArrayList<>();
            List<String> failedOkapi = new ArrayList<>();
            for (Object ob : addedPermissions) {
              String newPerm = (String) ob;
              if (!combinedPermissions.contains(newPerm)) {
                future = future.compose(x -> PermsCache.getFullPerms(newPerm, vertxContext, tenantId)
                    .map(permission -> {
                      if (permission == null) {
                        // unknown permission will eventually result in error, but not here
                        return null;
                      }
                      boolean mutable = Boolean.TRUE.equals(permission.getMutable());
                      if ((newPerm.startsWith("okapi.") || newPerm.equals(PermissionUtils.PERMS_USERS_ASSIGN_OKAPI))
                          && !hasOkapi) {
                        failedOkapi.add(newPerm);
                      }
                      if (mutable) {
                        if (!hasMutable) {
                          failedMutable.add(newPerm);
                        }
                      } else {
                        if (!hasImmutable) {
                          failedImmutable.add(newPerm);
                        }
                      }
                      return null;
                    }));
              }
            }
            return future.map(x -> {
              checkPermList("okapi", failedOkapi, operatingUser, modulePermissions);
              checkPermList("immutable", failedImmutable, operatingUser, modulePermissions);
              checkPermList("mutable", failedMutable, operatingUser, modulePermissions);
              return null;
            });
          });
        });
  }

  /* If we are modifying a permissions user or creating a new one, we need to
  check for any changes to the permissions list. For any changes, we need to
  add or delete from the permission's "grantedTo" field
   */
  protected static Future<Void> updateUserPermissions(
      Conn connection,
      String permUserId, JsonArray originalList, JsonArray newList,
      Context vertxContext, String tenantId, Map<String,String> okapiHeaders) {

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
    return checkOperatingPermissions(missingFromOriginalList, tenantId, okapiHeaders, vertxContext)
        .compose(x -> {
          Future<List<String>> checkExistsResF = findMissingPermissionsFromList(
              connection, missingFromOriginalList.getList());
          return checkExistsResF.compose(checkExistsRes -> {
            if (!checkExistsRes.isEmpty()) {
              throw new InvalidPermissionsException("permissions", permUserId, String.format(
                  "Attempting to add non-existent permissions %s to permission user with id %s",
                  String.join(",", checkExistsRes), permUserId));
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
            return modifyPermissionArrayFieldList(connection, fuvList);
          });
        });
  }

  private static Future<List<String>> getOperatingPermissions(Context vertxContext, String tenantId, String operatingUser) {
    return lookupPermsUsersById(operatingUser, "userId", tenantId, vertxContext)
        .compose(permissionUser -> {
          if (permissionUser == null) {
            return Future.succeededFuture(Collections.emptyList());
          }
          List<String> expandedSubs = new ArrayList<>();
          Future<Void> future = Future.succeededFuture();
          for (Object p : permissionUser.getPermissions()) {
            String perm = (String) p;
            List<String> subPerm = new ArrayList<>();
            subPerm.add(perm);
            expandedSubs.add(perm);
            future = future.compose(x1 -> PermsCache.expandPerms(subPerm, vertxContext, tenantId)
                .onSuccess(subs -> expandedSubs.addAll(subs))
                .mapEmpty());
          }
          return future.map(expandedSubs);
        });
  }

  /* If we are modifying (or creating) the subpermissions array of a permission
  object, check for any changes and for any newly declared subpermissions, add
  the permission name to the the 'childOf' field for those permisisons
   */
  protected static Future<Void> updateSubPermissions(Conn connection,
      String permissionName, JsonArray originalList, JsonArray newList, Map<String,String> okapiHeaders,
      Context vertxContext, String tenantId) {

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
      return checkOperatingPermissions(missingFromOriginalList, tenantId, okapiHeaders, vertxContext)
          .compose(x ->
              (Future<List<String>>) findMissingPermissionsFromList(
                  connection, missingFromOriginalList.getList())
          )
          .compose(res -> {
            if (!res.isEmpty()) {
              throw new InvalidPermissionsException(PERMISSION_NAME_FIELD, permissionName, String.format(
                  "Attempting to add non-existent permissions %s as sub-permissions to permission %s",
                  String.join(",", res), permissionName));
            }
            List<FieldUpdateValues> fuvList = new ArrayList<>();
            for (Object childPermissionNameOb : missingFromOriginalList) {
              FieldUpdateValues fuv = new FieldUpdateValues(
                  permissionName,
                  (String) childPermissionNameOb,
                  PermissionField.CHILD_OF,
                  Operation.ADD);
              fuvList.add(fuv);
            }
            for (Object childPermissionNameOb : missingFromNewList) {
              FieldUpdateValues fuv = new FieldUpdateValues(
                  permissionName,
                  (String) childPermissionNameOb,
                  PermissionField.CHILD_OF,
                  Operation.DELETE);
              fuvList.add(fuv);
            }
            return modifyPermissionArrayFieldList(connection, fuvList);
          });
    } catch (Exception e) {
      return Future.failedFuture(e);
    }
  }

  private static Future<Void> modifyPermissionArrayField(Conn connection, String fieldValue,
      String permissionName, PermissionField field, Operation operation) {
    try {
      Criteria nameCrit = new Criteria()
          .addField(PERMISSION_NAME_FIELD)
          .setOperation("=")
          .setVal(permissionName);
      Criterion criterion = new Criterion(nameCrit);
      CQLWrapper cqlFilter = new CQLWrapper(criterion);
      return connection.get(TABLE_NAME_PERMS, Permission.class, criterion, false)
          .compose(res -> {
            List<Permission> permList = res.getResults();
            if (permList.size() != 1) {
              return Future.failedFuture("Expected one result for " + PERMISSION_NAME_FIELD
                  + ": '" + permissionName + "', got " + permList.size()
                  + " results");
            }
            Permission permission = permList.get(0);
            List valueList;
            if (field == PermissionField.CHILD_OF) {
              valueList = permission.getChildOf();
            } else {
              valueList = permission.getGrantedTo();
            }
            logger.info("Performing {} operation on {} of permission {} with value {}",
                operation, field, permissionName, fieldValue);
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
            if (!modified) {
              return Future.succeededFuture();
            }
            return connection.update(TABLE_NAME_PERMS, permission, cqlFilter, true)
                .mapEmpty();
          });
    } catch (Exception e) {
      return Future.failedFuture(e);
    }
  }

  private static Future<Void> modifyPermissionArrayFieldList(Conn connection, List<FieldUpdateValues> fuvList) {

    Future<Void> future = Future.succeededFuture();
    for (FieldUpdateValues fuv : fuvList) {
      future = future.compose(x ->
          modifyPermissionArrayField(connection,
              fuv.getFieldValue(), fuv.getPermissionName(), fuv.getField(),
              fuv.getOperation()));
    }
    return future;
  }

  private Future<Void> removePermissionFromUserList(Conn connection, String permissionName, List<String> userIdList) {

    if (userIdList.isEmpty()) {
      return Future.succeededFuture();
    }
    List<String> userIdListCopy = new ArrayList<>(userIdList);
    String userId = userIdListCopy.get(0);
    userIdListCopy.remove(0);
    return removePermissionFromUser(connection, permissionName, userId)
        .compose(res -> removePermissionFromUserList(connection, permissionName, userIdListCopy));
  }

  private Future<Void> removePermissionFromUser(Conn connection, String permissionName, String userId) {
    return connection.getById(TABLE_NAME_PERMSUSERS, userId, PermissionUser.class).compose(user -> {
      if (!user.getPermissions().contains(permissionName)) {
        return Future.succeededFuture(); //User already lacks the permissions
      }
      user.getPermissions().remove(permissionName);
      return connection.update(TABLE_NAME_PERMSUSERS, user, userId).mapEmpty();
    });
  }

  private Future<Void> removeSubpermissionFromPermissionList(Conn connection, String subpermissionName, List<String> permissionNameList) {

    if (permissionNameList.isEmpty()) {
      return Future.succeededFuture();
    }
    List<String> permissionNameListCopy = new ArrayList<>(permissionNameList);
    String permissionName = permissionNameListCopy.get(0);
    permissionNameListCopy.remove(0);
    return removeSubpermissionFromPermission(connection, subpermissionName, permissionName)
        .compose(res -> removeSubpermissionFromPermissionList(connection, subpermissionName,
            permissionNameListCopy));
  }

  private Future<Void> removeSubpermissionFromPermission(Conn connection, String subpermissionName, String permissionName) {

    Criteria nameCrit = new Criteria()
        .addField(PERMISSION_NAME_FIELD)
        .setOperation("=")
        .setVal(permissionName);
    Criterion criterion = new Criterion(nameCrit);
    CQLWrapper cqlFilter = new CQLWrapper(criterion);
    return connection.get(TABLE_NAME_PERMS, Permission.class,
        criterion, false)
        .compose(result -> {
          if (result.getResults().isEmpty()) {
            return Future.succeededFuture();
          }
          Permission permission = result.getResults().get(0);
          permission.getSubPermissions().remove(subpermissionName);
          return connection.update(TABLE_NAME_PERMS, permission, cqlFilter,
              true).mapEmpty();
        });
  }

  private Future<Permission> retrievePermissionByName(String permissionName, Context vertxContext, String tenantId) {

    Criteria nameCrit = new Criteria()
        .addField(PERMISSION_NAME_FIELD)
        .setOperation("=")
        .setVal(permissionName);

    return PostgresClient.getInstance(vertxContext.owner(), tenantId)
        .get(TABLE_NAME_PERMS, Permission.class, new Criterion(nameCrit),false)
        .map(result -> result.getResults().isEmpty() ? null : result.getResults().get(0));
  }

  private Future<Boolean> checkPermlistForDummy(List<Object> permList,
      Context vertxContext, String tenantId) {

    Future<Boolean> future = Future.succeededFuture(false);
    for (Object o : permList) {
      String permissionName = (String) o;
      future = future.compose(x -> {
        if (x) {
          return Future.succeededFuture(true);
        }
        return retrievePermissionByName(permissionName, vertxContext, tenantId)
              .map(result -> result != null && Boolean.TRUE.equals(result.getDummy()));
      });
    }
    return future;
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
    List<Object> subPerms = new ArrayList<>(entity.getSubPermissions());
    perm.setSubPermissions(subPerms);
    perm.setMutable(entity.getMutable());
    perm.setVisible(entity.getVisible());
    perm.setTags(entity.getTags());
    perm.setMetadata(entity.getMetadata());
    return perm;
  }

  static List<List<String>> splitStringList(Iterable<String> stringList, int chunkSize) {
    List<List<String>> listOfLists = new ArrayList<>();
    int count = 0;
    List<String> currentChunk = new ArrayList<>(chunkSize);
    for (String string : stringList) {
      count++;
      currentChunk.add(string);
      if (count == chunkSize) {
        listOfLists.add(currentChunk);
        currentChunk = new ArrayList<>(chunkSize);
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

  @Override
  public void postPermsPurgeDeprecated(Map<String, String> okapiHeaders, Handler<AsyncResult<Response>> asyncResultHandler,
      Context vertxContext) {
    try {
      String tenantId = TenantTool.tenantId(okapiHeaders);
      PostgresClient pgClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
      PermissionUtils.purgeDeprecatedPermissions(pgClient, tenantId)
        .onSuccess(res -> {
          asyncResultHandler
            .handle(Future.succeededFuture(PostPermsPurgeDeprecatedResponse.respond200WithApplicationJson(res)));
        })
        .onFailure(ex -> {
          asyncResultHandler
            .handle(Future.succeededFuture(PostPermsPurgeDeprecatedResponse.respond500WithTextPlain(ex.getMessage())));
        });
    } catch (Exception e) {
      logger.error(e.getMessage(), e);
      asyncResultHandler
        .handle(Future.succeededFuture(PostPermsPurgeDeprecatedResponse.respond500WithTextPlain(e.getMessage())));
    }
  }

}
