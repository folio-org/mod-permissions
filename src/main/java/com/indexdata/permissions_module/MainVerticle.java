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
import io.vertx.core.http.HttpServer;
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
  private static final String API_KEY_HEADER = "auth_api_key";
  
  public void start(Future<Void> future) {
    authApiKey = System.getProperty("auth.api.key", "VERY_WEAK_KEY");
    
    String mongoURL = System.getProperty("mongo.url", "mongodb://localhost:27017/test");
    mongoClient = MongoClient.createShared(vertx, new JsonObject().put("connection_string", mongoURL));
    store = new MongoPermissionsStore(mongoClient);
    HttpServer server = vertx.createHttpServer();
    final int port = Integer.parseInt(System.getProperty("port", "8081"));
    
    Router router = Router.router(vertx);
    router.post("/perms/users").handler(BodyHandler.create());
    router.post("/perms").handler(BodyHandler.create());
    router.get("/perms/users/:username").handler(this::handleUser);
    router.get("/perms/users/:username/permissions").handler(this::handleUserPermission);
    router.post("/perms/users/:username/permissions").handler(this::handleUserPermission);
    router.delete("/perms/users/:username/permissions/:permissionname").handler(this::handleUserPermission);
    router.post("/perms/users").handler(this::handleUser);
    router.delete("/perms/users/:username").handler(this::handleUser);
    router.get("/perms/permissions/:permissionname").handler(this::handlePermission); //Get sub permissions
    router.post("/perms/permissions").handler(this::handlePermission); //Add a new permission
    router.post("/perms/permissions/:permissionname").handler(this::handlePermission); //Add a new sub permission
    router.delete("/perms/permissions/:permissionname").handler(this::handlePermission); //Remove a permission
    router.delete("/perms/permissions/:permissionname/:subpermissionname"); //Remove a sub-permission
    router.get("/perms/privileged/users/:username/permissions").handler(this::handleUserPermissionPrivileged);
    
    server.requestHandler(router::accept).listen(port, result -> {
        if(result.succeeded()) {
          future.complete();
        } else {
          future.fail(result.cause());
        }
    });  
    
  }
  
  private void handleUserPermissionPrivileged(RoutingContext context) {
    String apiKeyHeader = context.request().headers().get(API_KEY_HEADER);
    if(apiKeyHeader != null && apiKeyHeader.equals(authApiKey)) {
      handleUserPermission(context);
      return;
    } else {
      context.response()
              .setStatusCode(401)
              .end("Unauthorized");
    }
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
    String postData = null;
    if(context.request().method() == HttpMethod.POST) {
      postData = context.getBodyAsString();
    }
    if(context.request().method() == HttpMethod.POST) {
      String permissionName = context.request().getParam("permissionname");
       if(permissionName == null) {
         //Adding new permission
         store.addPermission(postData).setHandler(res -> {
           if(!res.succeeded()) {
             context.response()
                     .setStatusCode(500)
                     .end("Unable to add permission");
             return;
           }
           context.response()
                   .setStatusCode(201)
                   .end("Permission added");
        });
       } else {
         //Adding new sub-permission
         store.addSubPermission(permissionName, postData).setHandler(res -> {
           if(!res.succeeded()) {
             context.response()
                     .setStatusCode(500)
                     .end("Unable to add permission");
             return;
           }
           context.response()
                   .setStatusCode(201)
                   .end("Sub-Permission added");
         });         
       }
    } else if(context.request().method() == HttpMethod.GET) {
      String permissionName = context.request().getParam("permissionname");
      if(permissionName == null) {
        context.response()
                .setStatusCode(400)
                .end("You must specify a permission name");
        return;
      }
      store.getSubPermissions(permissionName).setHandler(res -> {
        if(!res.succeeded()) {
          context.response()
                  .setStatusCode(500)
                  .end("Unable to retrieve subpermissions");
          return;
        }
        context.response()
                .setStatusCode(200)
                .putHeader("Content-Type", "application/json")
                .end(res.result().encode());
      });
    } else if(context.request().method() == HttpMethod.DELETE) {
      String permissionName = context.request().getParam("permission_name");
      String subPermissionName = context.request().getParam("subpermissionname");
      if(permissionName == null && subPermissionName == null) {
        context.response()
                .setStatusCode(400)
                .end("Unsupported path");
        return;
      } else if (subPermissionName == null) {
        //remove permission
        store.removePermission(permissionName).setHandler(res -> {
          if(!res.succeeded()) {
            context.response()
                    .setStatusCode(500)
                    .end("Unable to delete permission");
            return;
          }
          context.response()
                  .setStatusCode(200)
                  .end("Permission deleted");
        });
      } else {
        //remove sub permission
        store.removeSubPermission(permissionName, subPermissionName).setHandler(res -> {
          if(!res.succeeded()) {
            context.response()
                    .setStatusCode(500)
                    .end("Unable to delete subpermission");
            return;
          }
          context.response()
                  .setStatusCode(200)
                  .end("Sub permission deleted");
        });
      }
    } else {
      context.response()
              .setStatusCode(400)
              .end("Unsupported method");
    }
    
  }
  
  private void handleUserPermission(RoutingContext context) {
    String username = context.request().getParam("username");
    String permissionName = context.request().getParam("permissionname");
    String postData = null;
    if(context.request().method() == HttpMethod.POST) {
      postData = context.getBodyAsString();
    }
    if(username == null) {
      context.response()
              .setStatusCode(400)
              .end("Invalid username specification");
      return;
    }
    if(context.request().method() == HttpMethod.POST) {
      store.addPermissionToUser(username, postData).setHandler(res -> {
        if(!res.succeeded()) {
          context.response()
                  .setStatusCode(500)
                  .end("Unable to add permission to user");
          return;
        }
        context.response()
                .setStatusCode(201)
                .end("Added permission to user");
      });
    } else if(context.request().method() == HttpMethod.GET) {
      store.getPermissionsForUser(username).setHandler(res -> {
        if(!res.succeeded()) {
          context.response()
                  .setStatusCode(500)
                  .end("Unable to retrieve user permissions");
        } else {
          context.response()
                  .setStatusCode(200)
                  .putHeader("Content-Type", "application/json")
                  .end(res.result().encode());
        }
      });
    } else if(context.request().method() == HttpMethod.DELETE) {
      if(permissionName == null) {
        context.response()
                .setStatusCode(400)
                .end("Invalid permission name specification");
        return;
      }
      store.removePermissionFromUser(username, permissionName).setHandler(res -> {
        if(!res.succeeded()) {
          context.response()
                  .setStatusCode(500)
                  .end("Unable to delete permission from user");
          return;
        }
        context.response()
                .setStatusCode(200)
                .end("Permission removed from user");
      });
    } else {
      context.response()
              .setStatusCode(400)
              .end("Unsupported method");
    }
  }
}
