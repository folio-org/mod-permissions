/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.folio.auth.permissions_module;

import org.folio.auth.permissions_module.impl.MongoPermissionsStore;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.http.HttpServer;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;
import java.util.ArrayList;

/**
 *
 * @author kurt
 */
public class MainVerticle extends AbstractVerticle {
  private MongoClient mongoClient;
  private String authApiKey;
  private PermissionsStore store;
  private static final String API_KEY_HEADER = "auth_api_key";
  private static final String TENANT_HEADER = "X-Okapi-Tenant";
  
  public void start(Future<Void> future) {
    authApiKey = System.getProperty("auth.api.key", "VERY_WEAK_KEY");
    
    String mongoURL = System.getProperty("mongo.url", "mongodb://localhost:27017/test");
    mongoClient = MongoClient.createShared(vertx, new JsonObject().put("connection_string", mongoURL));
    store = new MongoPermissionsStore(mongoClient);
    HttpServer server = vertx.createHttpServer();
    final int port = Integer.parseInt(System.getProperty("port", "8081"));
    
    Router router = Router.router(vertx);
    router.post("/perms/users/*").handler(BodyHandler.create());
    router.post("/perms/*").handler(BodyHandler.create());
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
    router.delete("/perms/permissions/:permissionname/:subpermissionname").handler(this::handlePermission); //Remove a sub-permission
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
    String tenant = context.request().headers().get(TENANT_HEADER);
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
      store.addUser(username, tenant).setHandler(res -> {
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
      store.removeUser(username, tenant).setHandler(res -> {
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
    String tenant = context.request().headers().get(TENANT_HEADER);
    String postData = null;
    if(context.request().method() == HttpMethod.POST) {
      postData = context.getBodyAsString();
    }
    if(context.request().method() == HttpMethod.POST) {
      String permissionName = context.request().getParam("permissionname");
      if (permissionName == null) {
        //Adding new permission
        JsonObject perm;
        String permName;
        JsonArray permSubs;
        try {
          perm = new JsonObject(postData);
        } catch (Exception e) {
          context.response()
                  .setStatusCode(400)
                  .end("Unable to parse " + postData + " into valid JSON");
          return;
        }
        permName = perm.getString("permission_name");
        if(permName == null) {
          context.response()
                  .setStatusCode(400)
                  .end("permission_name field is not present");
        }
        permSubs = perm.getJsonArray("sub_permissions");
        store.addPermission(permName, tenant).setHandler(res -> {
          if (res.failed()) {
            System.out.println("Unable to add permission: " + res.cause().getMessage());
            context.response()
                    .setStatusCode(500)
                    .end("Unable to add permission");
          } else {
            if(permSubs == null) {
              context.response()
                  .setStatusCode(201)
                  .end("Permission added");
            } else {
              ArrayList<Future> futureList = new ArrayList<>();
              for(Object o : permSubs) {
                String sub = (String)o;
                futureList.add(store.addSubPermission(permName, sub, tenant));
              }
              CompositeFuture compFut = CompositeFuture.all(futureList);
              compFut.setHandler(res2 -> {
                if(res2.failed()) {
                  System.out.println("Error adding subpermissions: " + res2.cause().getMessage());
                  context.response()
                          .setStatusCode(500)
                          .end("Error adding sub permission");
                } else {
                  context.response()
                    .setStatusCode(201)
                    .end("Permission added");
                }
              });
            }
          }
        });
      } else {
        //Adding new sub-permission
        store.addSubPermission(permissionName, postData, tenant).setHandler(res -> {
          if (!res.succeeded()) {
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
      store.getSubPermissions(permissionName, tenant).setHandler(res -> {
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
      String permissionName = context.request().getParam("permissionname");
      String subPermissionName = context.request().getParam("subpermissionname");
      if(permissionName == null && subPermissionName == null) {
        context.response()
                .setStatusCode(400)
                .end("Unsupported path");
        return;
      } else if (subPermissionName == null) {
        //remove permission
        store.removePermission(permissionName, tenant).setHandler(res -> {
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
        store.removeSubPermission(permissionName, subPermissionName, tenant).setHandler(res -> {
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
    String tenant = context.request().headers().get(TENANT_HEADER);
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
    /*Add a permission to a user */
    /* format:
    { "permission_name" : "some.new.permission" }
    */
    if(context.request().method() == HttpMethod.POST) {
      JsonObject perm;
      try {
        perm = new JsonObject(postData);
      } catch(Exception e) {
        context.response()
                .setStatusCode(400)
                .end("Unable to parse " + postData + " into valid JSON");
        return;
      }
      String addPermissionName = perm.getString("permission_name");
      store.addPermissionToUser(username, addPermissionName, tenant).setHandler(res -> {
        if(res.failed()) {
          context.response()
                  .setStatusCode(500)
                  .end("Unable to add permission to user");
        } else {
          context.response()
                .setStatusCode(201)
                .end("Added permission to user");
        }
      });
    } else if(context.request().method() == HttpMethod.GET) {
      store.getPermissionsForUser(username, tenant).setHandler(res -> {
        if(res.failed()) {
          context.response()
                  .setStatusCode(500)
                  .end("Unable to retrieve user permissions " + res.cause().getMessage());
        } else {
          context.response()
                  .setStatusCode(200)
                  .putHeader("Content-Type", "application/json")
                  .end(res.result().encode());
        }
        return;
      });
    } else if(context.request().method() == HttpMethod.DELETE) {
      if(permissionName == null) {
        context.response()
                .setStatusCode(400)
                .end("Invalid permission name specification");
        return;
      }
      store.removePermissionFromUser(username, permissionName, tenant).setHandler(res -> {
        if(res.failed()) {
          context.response()
                  .setStatusCode(500)
                  .end("Unable to delete permission from user");
        } else {
          context.response()
                .setStatusCode(200)
                .end("Permission removed from user");
        }
      });
    } else {
      context.response()
              .setStatusCode(400)
              .end("Unsupported method");
    }
  }
}
