/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.indexdata.permissions_module;

import com.indexdata.permissions_module.impl.MongoPermissionsStore;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;

/**
 *
 * @author kurt
 */
public class MainVerticle extends AbstractVerticle {
  private MongoClient mongoClient;
  private String authApiKey;
  private PermissionsStore store;
  
  public void start(Future<Void> future) {
    authApiKey = System.getProperty("auth.api.key", "VERY_WEAK_KEY");
    
    String mongoURL = System.getProperty("mongo.url", "mongodb://localhost:27017");
    mongoClient = MongoClient.createShared(vertx, new JsonObject().put("connection_string", mongoURL));
    store = new MongoPermissionsStore(mongoClient);
    
    Router router = Router.router(vertx);
    router.post("/users").handler(BodyHandler.create());
    router.post("/permissions").handler(BodyHandler.create());
    router.get("/users/:username").handler(this::handleUser);
    router.get("/users/:username/permissions").handler(this::handleUserPermission);
    router.post("/users/:username/permissions").handler(this::handleUserPermission);
    router.delete("/users/:username/permissions/:permission_name").handler(this::handleUserPermission);
    router.post("/users").handler(this::handleUser);
    router.delete("/users/:username").handler(this::handleUser);
    router.get("/permissions/:permission_name").handler(this::handlePermission);
    router.post("/permissions").handler(this::handlePermission);
    
  }
  
  private void handleUser(RoutingContext context) {
    if(context.request().method() == HttpMethod.POST) {
      String username;
      try {
        username = context.getBodyAsString();
      } catch(Exception e) {
        context.response()
                .setStatusCode(400)
                .end("Please provide a username");
        return;
      }
      store.addUser(username).setHandler(res -> {
        if(res.succeeded()) {
          context.response()
                  .setStatusCode(201)
                  .end("User added");
        } else {
          context.response()
                  .setStatusCode(500)
                  .end("Unable to add user");
        }
      });
    } else if(context.request().method() == HttpMethod.DELETE) {
      String username = context.request().getParam("username");
      store.removeUser(username).setHandler(res -> {
        if(res.succeeded()) {
          context.response()
                  .setStatusCode(200)
                  .end("User removed");
        } else {
          context.response()
                  .setStatusCode(500)
                  .end("Unable to remove user");
        }
      });
    }
    else {
      context.response()
              .setStatusCode(400)
              .end("Unsupported method");
    }
  }
  
  private void handlePermission(RoutingContext context) {
    
  }
  
  private void handleUserPermission(RoutingContext context) {
  
  }
}
