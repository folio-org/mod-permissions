package org.folio.rest.impl;

import io.vertx.core.AsyncResult;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import java.util.List;
import java.util.Map;
import javax.ws.rs.Path;
import javax.ws.rs.core.Response;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserdataCollection;
import org.folio.rest.jaxrs.resource.UsersResource;
import org.folio.rest.jaxrs.resource.UsersResource.GetUsersResponse;
import org.folio.rest.jaxrs.resource.UsersResource.PostUsersResponse;
import org.folio.rest.jaxrs.resource.UsersResource.PutUsersByUserIdResponse;
import org.folio.rest.jaxrs.resource.UsersResource.DeleteUsersByUserIdResponse;
import org.folio.rest.persist.MongoCRUD;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.utils.OutStream;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.persist.Criteria.Limit;
import org.folio.rest.persist.Criteria.Offset;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.TenantTool;



/**
 *
 * @author kurt
 */
@Path("users")
public class UsersAPI implements UsersResource {

  private final Messages messages = Messages.getInstance();
  private final String USER_COLLECTION = "user";
  private static final String USER_ID_FIELD = "'id'";
  private static final String USER_NAME_FIELD = "'username'";
  private static final String TABLE_NAME_USER = "user";
  private static final String OKAPI_HEADER_TENANT = "x-okapi-tenant";
  private final Logger logger = LoggerFactory.getLogger(UsersAPI.class);
  
  @Validate
  @Override
  public void getUsers(String query, String orderBy, 
          Order order, int offset, int limit, String lang,
          Map <String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, 
          Context vertxContext) throws Exception {
    logger.debug("Getting users");
    try {
      vertxContext.runOnContext(v -> {
        try {
          String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
          Criterion criterion = Criterion.json2Criterion(query);
          criterion.setLimit(new Limit(limit)).setOffset(new Offset(offset));
          logger.debug("Headers present are: " + okapiHeaders.keySet().toString());
          logger.debug("Using criterion: " + criterion.toString());
          logger.debug("tenantId = " + tenantId);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(tenantId + "." + TABLE_NAME_USER,
                  User.class, criterion, true, reply -> {
            try {
              if(reply.succeeded()) {
                UserdataCollection userCollection = new UserdataCollection();
                List<User> users = (List<User>)reply.result()[0];
                userCollection.setUsers(users);
                userCollection.setTotalRecords(users.size());
                asyncResultHandler.handle(Future.succeededFuture(
                        GetUsersResponse.withJsonOK(userCollection)));
              } else {
                asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                        GetUsersResponse.withPlainInternalServerError(
                                reply.cause().getMessage())));
              }
            } catch(Exception e) {
              asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                        GetUsersResponse.withPlainInternalServerError(
                                reply.cause().getMessage())));
            }            
          });
        } catch(Exception e) {
          asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                        GetUsersResponse.withPlainInternalServerError(
                                messages.getMessage(lang,
                                        MessageConsts.InternalServerError))));
        }
      });
    } catch(Exception e) {
      logger.debug(e.getMessage());
      asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(
                        GetUsersResponse.withPlainInternalServerError(
                                messages.getMessage(lang,
                                        MessageConsts.InternalServerError))));
    }
    /*
    try {
      vertxContext.runOnContext( v -> {
        try {
          MongoCRUD.getInstance(vertxContext.owner()).get(
                  MongoCRUD.buildJson(
                    User.class.getName(), USER_COLLECTION, query, orderBy, order,
                    offset, limit), 
                  reply -> {
            UserdataCollection userCollection = new UserdataCollection();
            List<User> users = (List<User>)reply.result();
            userCollection.setUsers(users);
            userCollection.setTotalRecords(users.size());
            asyncResultHandler.handle(Future.succeededFuture(
                    GetUsersResponse.withJsonOK(userCollection)));
          });
        } catch(Exception e) {
          asyncResultHandler.handle(Future.succeededFuture(
                  GetUsersResponse.withPlainInternalServerError(messages.getMessage(
                          lang, MessageConsts.InternalServerError))));
        }
      });
    } catch(Exception e) {
      asyncResultHandler.handle(Future.succeededFuture(
              GetUsersResponse.withPlainInternalServerError(messages.getMessage(
                      lang, MessageConsts.InternalServerError))));
    }
  */
  }

  @Validate
  @Override
  public void postUsers(String lang, User entity,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler, 
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext( v -> {
        try {
          String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
          Criteria idCrit = new Criteria();
          idCrit.addField(USER_ID_FIELD);
          idCrit.setOperation("=");
          idCrit.setValue(entity.getId());
          Criteria nameCrit = new Criteria();
          nameCrit.addField(USER_NAME_FIELD);
          nameCrit.setOperation("=");
          nameCrit.setValue(entity.getUsername());
          Criterion crit = new Criterion();
          crit.addCriterion(idCrit, "OR", nameCrit);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).get(tenantId + "." + TABLE_NAME_USER, 
                  User.class, crit, true, getReply -> { 
              logger.debug("Attempting to get existing users of same id and/or username");
              if(getReply.failed()) {
                logger.debug("Attempt to get users failed: " + getReply.cause().getMessage());
                asyncResultHandler.handle(Future.succeededFuture(
                              PostUsersResponse.withPlainInternalServerError(
                                      getReply.cause().getMessage())));
              } else {
                List<User> userList = (List<User>)getReply.result()[0];
                if(userList.size() > 0) {
                  logger.debug("User with this id already exists");
                  asyncResultHandler.handle(Future.succeededFuture(
                          PostUsersResponse.withPlainBadRequest(
                                  messages.getMessage(
                                          lang, MessageConsts.UnableToProcessRequest))));
                  //uh oh
                } else {   
                  PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
                  postgresClient.startTx(beginTx -> {
                    logger.debug("Attempting to save new record");
                    try {
                      postgresClient.save(beginTx, tenantId + "." + TABLE_NAME_USER, entity, reply -> {
                        try {
                          if(reply.succeeded()) {
                            logger.debug("Save successful");
                            final User user = entity;
                            user.setId(entity.getId());
                            OutStream stream = new OutStream();
                            stream.setData(user);
                            postgresClient.endTx(beginTx, done -> {
                              asyncResultHandler.handle(Future.succeededFuture(PostUsersResponse.withJsonCreated(reply.result(), stream)));
                            });
                          } else {
                            asyncResultHandler.handle(Future.succeededFuture(
                                    PostUsersResponse.withPlainBadRequest(
                                            messages.getMessage(
                                                    lang, MessageConsts.UnableToProcessRequest))));

                          }
                        } catch(Exception e) {
                          asyncResultHandler.handle(Future.succeededFuture(
                              PostUsersResponse.withPlainInternalServerError(
                                      e.getMessage())));
                        }
                      });
                    } catch(Exception e) {
                      asyncResultHandler.handle(Future.succeededFuture(
                              PostUsersResponse.withPlainInternalServerError(
                                      getReply.cause().getMessage())));
                    }
                  });
                }
             }
            });         
        } catch(Exception e) {
          asyncResultHandler.handle(Future.succeededFuture(
                        PostUsersResponse.withPlainInternalServerError(
                        messages.getMessage(lang, MessageConsts.InternalServerError))));
        }
      });
    } catch(Exception e) {
      asyncResultHandler.handle(Future.succeededFuture(
              PostUsersResponse.withPlainInternalServerError(
              messages.getMessage(lang, MessageConsts.InternalServerError))));            
    }
  }

  @Validate
  @Override
  public void getUsersByUserId(String userId, String lang,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
     try {
       vertxContext.runOnContext(v -> {
         try {
           String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
           Criteria idCrit = new Criteria();
           idCrit.addField(USER_ID_FIELD);
           idCrit.setOperation("=");
           idCrit.setValue(userId);
           PostgresClient.getInstance(vertxContext.owner(), tenantId).get(tenantId + "." + TABLE_NAME_USER, User.class, new Criterion(idCrit), true, getReply -> {
             if(getReply.failed()) {
               asyncResultHandler.handle(Future.succeededFuture(GetUsersByUserIdResponse.withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
             } else {
               List<User> userList = (List<User>)getReply.result()[0];
               if(userList.size() < 1) {
                 asyncResultHandler.handle(Future.succeededFuture(
                        GetUsersByUserIdResponse.withPlainNotFound("User" + 
                                messages.getMessage(lang, 
                                        MessageConsts.ObjectDoesNotExist))));
               } else if(userList.size() > 1) {
                 logger.debug("Multiple users found with the same id");
                 asyncResultHandler.handle(Future.succeededFuture(
                      GetUsersByUserIdResponse.withPlainInternalServerError(
                              messages.getMessage(lang,
                                      MessageConsts.InternalServerError))));
               } else {
                 asyncResultHandler.handle(Future.succeededFuture(
                        GetUsersByUserIdResponse.withJsonOK(userList.get(0))));
               }
             }
           });
         } catch(Exception e) {
           logger.debug("Error occurred: " + e.getMessage());
           asyncResultHandler.handle(Future.succeededFuture(
                  GetUsersResponse.withPlainInternalServerError(messages.getMessage(
                          lang, MessageConsts.InternalServerError))));           
         }
       });
       /*
      vertxContext.runOnContext( v -> {
        try {
          JsonObject query = new JsonObject().put(USER_ID_FIELD, userId);
          MongoCRUD.getInstance(vertxContext.owner()).get(
                  MongoCRUD.buildJson(
                    User.class.getName(), USER_COLLECTION, query.encode()), 
                  reply -> {
            try {
              List<User> userList = (List<User>)reply.result();
              if(userList.isEmpty()) {
                asyncResultHandler.handle(Future.succeededFuture(
                        GetUsersByUserIdResponse.withPlainNotFound("User" + 
                                messages.getMessage(lang, 
                                        MessageConsts.ObjectDoesNotExist))));
              } else {
                asyncResultHandler.handle(Future.succeededFuture(
                        GetUsersByUserIdResponse.withJsonOK(userList.get(0))));
              }
            } catch(Exception e) {
              asyncResultHandler.handle(Future.succeededFuture(
                      GetUsersByUserIdResponse.withPlainInternalServerError(
                              messages.getMessage(lang,
                                      MessageConsts.InternalServerError))));
            }
          });
        } catch(Exception e) {
          asyncResultHandler.handle(Future.succeededFuture(
                  GetUsersResponse.withPlainInternalServerError(messages.getMessage(
                          lang, MessageConsts.InternalServerError))));
        }
      });
      */
    } catch(Exception e) {
      asyncResultHandler.handle(Future.succeededFuture(
              GetUsersResponse.withPlainInternalServerError(messages.getMessage(
                      lang, MessageConsts.InternalServerError))));
    }
  }

  @Validate
  @Override
  public void deleteUsersByUserId(String userId, String lang,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext(v-> {
        try {
          String tenantId = TenantTool.calculateTenantId(okapiHeaders.get(OKAPI_HEADER_TENANT));
          Criteria idCrit = new Criteria();
          idCrit.addField(USER_ID_FIELD);
          idCrit.setOperation("=");
          idCrit.setValue(userId);
          PostgresClient.getInstance(vertxContext.owner(), tenantId).delete(tenantId + "." + TABLE_NAME_USER, new Criterion(idCrit), deleteReply -> {
            if(deleteReply.failed()) {
             
            } else {
              if(deleteReply.result().getRemovedCount() == 0) {
                
              } else if(deleteReply.result().getRemovedCount > 1) {
                
              } else {
                
              }             
            }
          });
        } catch(Exception e) {
          
        }
      });
     } catch(Exception e) {
     }
  
      
      /*
      JsonObject query = new JsonObject().put(USER_ID_FIELD, userId);
      //The delete handler doesn't require a special buildJson query
      vertxContext.runOnContext(v-> {
        MongoCRUD.getInstance(vertxContext.owner()).delete(USER_COLLECTION, query,
                reply -> {
            if(reply.succeeded()) {
              if(reply.result().getRemovedCount() == 1) {
                asyncResultHandler.handle(Future.succeededFuture(
                        DeleteUsersByUserIdResponse.withNoContent()));
              } else {
                String errorMessage = messages.getMessage(lang,
                        MessageConsts.DeletedCountError, 1,
                        reply.result().getRemovedCount());
                asyncResultHandler.handle(Future.succeededFuture(
                        DeleteUsersByUserIdResponse.withPlainNotFound(errorMessage)));
              }
            } else {
              asyncResultHandler.handle(
                      Future.succeededFuture(
                              DeleteUsersByUserIdResponse.withPlainInternalServerError(
                                      messages.getMessage(lang,
                                              MessageConsts.InternalServerError))));
            }                                    
          }
        );
      });
      */
    } catch(Exception e) {
      asyncResultHandler.handle(
            Future.succeededFuture(
                    DeleteUsersByUserIdResponse.withPlainInternalServerError(
                            messages.getMessage(lang,
                                    MessageConsts.InternalServerError))));
    }
  }

  @Validate
  @Override
  public void putUsersByUserId(String userId, 
          String lang, User entity,
          Map<String, String> okapiHeaders,
          Handler<AsyncResult<Response>> asyncResultHandler,
          Context vertxContext) throws Exception {
          
    try {
      JsonObject query = new JsonObject().put(USER_ID_FIELD, userId);
      vertxContext.runOnContext(v -> {
        MongoCRUD.getInstance(vertxContext.owner()).update(USER_COLLECTION, entity,
                query, true, reply -> { 
          if(reply.result().getDocModified() == 0) {
            asyncResultHandler.handle(Future.succeededFuture(
                    PutUsersByUserIdResponse.withPlainNotFound(userId)));
          } else {
            try {
              asyncResultHandler.handle(Future.succeededFuture(
                      PutUsersByUserIdResponse.withNoContent()));
            } catch(Exception e) {
              asyncResultHandler.handle(Future.succeededFuture(
                      PutUsersByUserIdResponse.withPlainInternalServerError(
                              messages.getMessage(lang,
                                      MessageConsts.InternalServerError))));
            }
          }
        });
      });
      
    } catch (Exception e) {
      asyncResultHandler.handle(Future.succeededFuture(
              PutUsersByUserIdResponse.withPlainInternalServerError(
                      messages.getMessage(lang, MessageConsts.InternalServerError))));
    }
  } 
}

