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



/**
 *
 * @author kurt
 */
@Path("users")
public class UsersAPI implements UsersResource {

  private final Messages messages = Messages.getInstance();
  private final String USER_COLLECTION = "user";
  private static final String USER_ID_FIELD = "id";
  private static final String USER_NAME_FIELD = "username";
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
          JsonObject id_query = new JsonObject().put(USER_ID_FIELD, entity.getId());
          JsonObject username_query = new JsonObject().put(USER_NAME_FIELD, entity.getUsername());
          JsonObject query = new JsonObject().put("$or",
                  new JsonArray().add(id_query).add(username_query));
          JsonObject mcQuery = MongoCRUD.buildJson(User.class.getName(), 
                  USER_COLLECTION, query);
          MongoCRUD.getInstance(vertxContext.owner()).get(mcQuery, reply -> {
            List<User> userList = (List<User>)reply.result();
            if(userList.size() > 0) {
              asyncResultHandler.handle(Future.succeededFuture(
                      PostUsersResponse.withPlainBadRequest(
                              messages.getMessage(
                                      lang, MessageConsts.UnableToProcessRequest))));
            } else {
              try {
                MongoCRUD.getInstance(vertxContext.owner()).save(USER_COLLECTION, entity,
                        reply2 -> {
                  try {
                    User user = new User();
                    user = entity;
                    //user.setId(reply.result());
                    user.setId(entity.getId());
                    OutStream stream = new OutStream();
                    stream.setData(user);
                    asyncResultHandler.handle(Future.succeededFuture(
                            PostUsersResponse.withJsonCreated(reply2.result(),
                                    stream)));
                  } catch(Exception e) {
                    asyncResultHandler.handle(Future.succeededFuture(
                            PostUsersResponse.withPlainInternalServerError(
                            messages.getMessage(lang,
                                    MessageConsts.InternalServerError))));                    
                  }
                });
              } catch(Exception e) {
                asyncResultHandler.handle(Future.succeededFuture(
                        PostUsersResponse.withPlainInternalServerError(
                        messages.getMessage(lang, MessageConsts.InternalServerError))));            
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

