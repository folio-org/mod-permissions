/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.indexdata.permissions_module.impl;

import com.indexdata.permissions_module.PermissionsStore;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.mongo.UpdateOptions;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;


/**
 *
 * @author kurt
 */
public class MongoPermissionsStore implements PermissionsStore {
  private MongoClient mongoClient;
  private UpdateOptions updateOptions = new UpdateOptions().setMulti(true);
  
  public MongoPermissionsStore(MongoClient mongoClient) {
    this.mongoClient = mongoClient;
  }

  @Override
  public Future<Boolean> addPermission(String permission) {
    Future<Boolean> future = Future.future();
    JsonObject query = new JsonObject().put("permission_name", permission);
    mongoClient.find("permissions", query, res -> {
      if(res.succeeded()) {
        future.complete(false);
      } else {
        JsonObject insert = new JsonObject()
                .put("permission_name", permission)
                .put("sub_permissions", new JsonArray());
        mongoClient.insert("permissions", insert, res2 -> {
          if(res2.succeeded()) {
            future.complete(true);
          } else {
            future.fail(res2.result());
          }
        });
      }
    });
    return future;
  }

  @Override
  public Future<Boolean> addSubPermission(String permission, String sub) {
    //TODO: Check for circular permissions
    Future<Boolean> future = Future.future();
    JsonObject query = new JsonObject().put("permission_name", permission);
    mongoClient.find("permissions", query, res -> {
      if(!res.succeeded()) {
        future.complete(false); //Can't add a sub to a non-existent perm
      } else {
        JsonArray permissionList = res.result().get(0).getJsonArray("sub_permissions");
        permissionList.add(sub);
        JsonObject update = new JsonObject()
                .put("$set", new JsonObject()
                  .put("sub_permissions", permissionList));
        mongoClient.update("permissions", query, update, res2 -> {
          if(res2.succeeded()) {
            future.complete(true);
          } else {
            future.fail("Unable to update records");
          }
        });
      }
    });
    return future;
  }
  
  @Override
  public Future<JsonArray> getSubPermissions(String permission) {
    JsonObject query = new JsonObject().put("permission_name", permission);
    JsonArray permList = new JsonArray();
    Future<JsonArray> future = Future.future();
    mongoClient.find("permissions", query, res -> {
      if(!res.succeeded()) {
        future.fail("Query unsuccessful");
      } else {
        JsonObject permObject = res.result().get(0);
        future.complete(permObject.getJsonArray("sub_permissions"));
      }
    });
    return future;        
  }

  @Override
  public Future<JsonArray> getExpandedPermissions(String permission) {
    JsonObject query = new JsonObject().put("permission_name", permission);
    JsonArray permList = new JsonArray();
    Future<JsonArray> future = Future.future();
    mongoClient.find("permissions", query, res -> {
      if(res.succeeded() && res.result().size() > 0) {
        /*
        If there are no subpermissions, go ahead and complete the future with the
        given value of the JsonArray
        
        If there are subpermissions, create a list of new futures, by calling
        walkPerms for each sub permission, then create a composite future from
        these new futures, with a handler that completes the original
        future when they return
        */
        JsonObject permObj = res.result().get(0);
        permList.add(permission);
        JsonArray subPerms = permObj.getJsonArray("sub_permissions");
        if(!subPerms.isEmpty()) {
          LinkedList<Future> futureList = new LinkedList<>();
          for(Object o : subPerms) {
            String sub = (String)o;
            Future<JsonArray> newFuture = getExpandedPermissions(sub);
            futureList.add(newFuture);
          }
          CompositeFuture compositeFuture = CompositeFuture.all(futureList);
          compositeFuture.setHandler(res2 -> {
            if(res2.succeeded()) {
              //Get output of contained futures and complete the future here
              for(Future f : futureList) {
                JsonArray arr = (JsonArray)f.result();
                for(Object o : arr) {
                  permList.add(o);
                }
              }
              future.complete(permList);
            } else {
              future.fail("Unable to populate permissions");
            }
          });
        } else {
          future.complete(permList);
        }
      }
    });
    return future;
  }


  @Override
  public Future<Boolean> removePermission(String permission) {
    Future<Boolean> future = Future.future();
    JsonObject query = new JsonObject().put("permission_name", permission);
    mongoClient.find("permissions", query, res-> {
      if(!res.succeeded()) {
        future.complete(false);
      } else {
        //Find all permissions that list this permission as a sub
        /*
        JsonObject subQuery = new JsonObject().put("sub_permissions", new JsonObject()
          .put("$in", new JsonArray().add(permission)));
         */
        JsonObject subUpdate = new JsonObject()
                .put("$pullAll", new JsonObject()
                  .put("sub_permissions", new JsonArray().add(permission)));
        mongoClient.updateWithOptions("permissions", new JsonObject(), subUpdate, updateOptions, res2-> {
          if(!res2.succeeded()) {
            future.fail("Unable to remove sub permissions");
          } else {
            //Now delete the actual permission, since the sub permissions are clean
            mongoClient.remove("permissions", query, res3 -> {
              if(!res3.succeeded()) {
                future.fail("Unable to delete permission");
              } else {
                future.complete(true);
              }
            });
          }
        });
      }
    });
    return future;
  }

  @Override
  public Future<Boolean> removeSubPermission(String permission, String sub) {
    Future<Boolean> future = Future.future();
    JsonObject query = new JsonObject().put("permission_name", permission);
    JsonObject update = new JsonObject().put("$pull", new JsonObject()
      .put("sub_permissions", new JsonObject()
        .put("$in", new JsonArray().add(sub))));
    mongoClient.update("permissions", query, update, res -> {
      if(!res.succeeded()) {
        future.fail("Unable to remove sub permissiion");
      } else {
        future.complete(true);
      }      
    });
    return future;
  }

  @Override
  public Future<Boolean> addUser(String user) {
    JsonObject query = new JsonObject().put("user_name", user);
    Future<Boolean> future = Future.future();
    mongoClient.find("users", query, res-> {
      if(res.result().size() > 0) {
        future.complete(false);
      } else {
        JsonObject insert = new JsonObject()
                .put("user_name", user)
                .put("user_permissions", new JsonArray());
        mongoClient.insert("users", insert, res2 -> {
          if(res2.succeeded()) {
            future.complete(true);
          } else {
            future.fail("Unable to insert new user");
          }
        });
      }
    });
    return future;
  }

  @Override
  public Future<Boolean> removeUser(String user) {
    JsonObject query = new JsonObject().put("user_name", user);
    Future<Boolean> future = Future.future();
    mongoClient.find("users", query, res-> {
      if(res.result().size() > 0) {
        future.complete(false);
      } else {
        mongoClient.remove("users", query, res2-> {
          if(!res.succeeded()) {
            future.fail("Unable to remove user");
          } else {
            future.complete(true);
          }
        });
      }
    });
    return future;
  }

  @Override
  public Future<Boolean> addPermissionToUser(String user, String permission) {
    JsonObject query = new JsonObject().put("user_name", user);
    Future<Boolean> future = Future.future();
    mongoClient.find("users", query, res-> {
      if(res.result().size() > 0) {
        future.complete(false);
      } else {
        JsonObject push = new JsonObject()
                .put("$push", new JsonObject()
                  .put("user_permissions", permission));
        mongoClient.update("users", query, push, res2 -> {
          if(!res2.succeeded()) {
            future.fail("Unable to add permission");
          } else {
            future.complete(true);
          }
        });
      }
    });
    return future;
  }

  @Override
  public Future<Boolean> removePermissionFromUser(String user, String permission) {
    JsonObject query = new JsonObject().put("user_name", user);
    Future<Boolean> future = Future.future();
    mongoClient.find("users", query, res-> {
      if(res.result().size() > 0) {
        future.complete(false);
      } else {
        JsonObject update = new JsonObject().put("$pullAll", new JsonObject()
          .put("user_permissions", new JsonArray().add(permission)));
        mongoClient.update("users", query, update, res2 -> {
          if(!res2.succeeded()) {
            future.fail("Unable to remove permission");
          } else {
            future.complete(true);
          }
        });
      }       
    });
    return future;
  }

  @Override
  public Future<JsonArray> getPermissionsForUser(String user) {
    JsonObject query = new JsonObject().put("user_name", user);
    Future<JsonArray> future = Future.future();
    mongoClient.find("users", query, res-> {
      if(res.result().size() < 1) {
        future.fail("No such user");
      } else {
        JsonObject userObject = res.result().get(0);
        future.complete(userObject.getJsonArray("user_permissions"));
      }
    });
    return future;
  }

  
}
