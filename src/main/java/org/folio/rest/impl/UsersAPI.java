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
import java.util.List;
import javax.ws.rs.Path;
import javax.ws.rs.core.Response;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserdataCollection;
import org.folio.rest.jaxrs.resource.UsersResource;
import org.folio.rest.jaxrs.resource.UsersResource.GetUsersResponse;
import org.folio.rest.jaxrs.resource.UsersResource.PostUsersResponse;
import org.folio.rest.persist.MongoCRUD;
import org.folio.rest.tools.messages.Messages;
import org.folio.rest.tools.messages.MessageConsts;
import org.folio.rest.tools.utils.OutStream;



/**
 *
 * @author kurt
 */
@Path("users")
public class UsersAPI implements UsersResource {

  private final Messages messages = Messages.getInstance();
  private final String USER_COLLECTION = "user";
  
  @Validate
  @Override
  public void getUsers(String authorization, String query, String orderBy, 
          Order order, int offset, int limit, String lang, 
          Handler<AsyncResult<Response>> asyncResultHandler, 
          Context vertxContext) throws Exception {
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
            asyncResultHandler.handle(Future.succeededFuture(GetUsersResponse.withJsonOK(userCollection)));
          });
        } catch(Exception e) {
          asyncResultHandler.handle(Future.succeededFuture(
                  GetUsersResponse.withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
        }
      });
    } catch(Exception e) {
      asyncResultHandler.handle(Future.succeededFuture(
              GetUsersResponse.withPlainInternalServerError(messages.getMessage(lang, MessageConsts.InternalServerError))));
    }
  }

  @Validate
  @Override
  public void postUsers(String authorization, String lang, User entity, 
          Handler<AsyncResult<Response>> asyncResultHandler, 
          Context vertxContext) throws Exception {
    try {
      vertxContext.runOnContext( v -> {
        try {
          MongoCRUD.getInstance(vertxContext.owner()).save(USER_COLLECTION, entity,
                  reply -> {
                    try {
                      User user = new User();
                      user = entity;
                      user.setId(reply.result());
                      OutStream stream = new OutStream();
                      stream.setData(user);
                      asyncResultHandler.handle(Future.succeededFuture(
                              PostUsersResponse.withJsonCreated(reply.result(), stream)));
                    } catch(Exception e) {
                      asyncResultHandler.handle(Future.succeededFuture(PostUsersResponse.withPlainInternalServerError(
                              messages.getMessage(lang, MessageConsts.InternalServerError))));                    
                    }
                  });
        } catch(Exception e) {
          asyncResultHandler.handle(Future.succeededFuture(PostUsersResponse.withPlainInternalServerError(
                  messages.getMessage(lang, MessageConsts.InternalServerError))));            
        }
      });
      throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    } catch(Exception e) {
      asyncResultHandler.handle(Future.succeededFuture(PostUsersResponse.withPlainInternalServerError(
              messages.getMessage(lang, MessageConsts.InternalServerError))));            
    }
  }

  @Override
  public void getUsersByUserId(String userId, String authorization, String lang, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void deleteUsersByUserId(String userId, String authorization, String lang, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public void putUsersByUserId(String userId, String authorization, String lang, User entity, Handler<AsyncResult<Response>> asyncResultHandler, Context vertxContext) throws Exception {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }
  
  
}
