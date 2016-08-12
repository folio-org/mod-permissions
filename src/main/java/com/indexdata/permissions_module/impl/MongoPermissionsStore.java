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
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;


/**
 *
 * @author kurt
 */
public class MongoPermissionsStore implements PermissionsStore {
  private MongoClient mongoClient;
  
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
        JsonObject subUpdate = new JsonObject().put("$pull", new JsonObject()
          .put("sub_permissions", new JsonObject()
            .put("$in", new JsonArray().add(permission))));
        mongoClient.update("permissions", new JsonObject(), subUpdate, res2-> {
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
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public Future<Boolean> removeUser(String user) {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public Future<Boolean> addPermissionToUser(String user, String permission) {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public Future<Boolean> removePermissionFromUser(String user, String permission) {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public Future<JsonArray> getPermissionsForUser(String user) {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public Future<JsonArray> getSubPermissions(String permission) {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public Future<JsonArray> getExpandedPermissions(String permission) {
    Future<JsonArray> future = Future.future();
    Future<Void> futureChain = Future.future();
        
    
    LinkedList<String> permissionNames = new LinkedList<>();
    permissionNames.add(permission);
    JsonObject query = new JsonObject().put("permission_name", permission);
    mongoClient.find("permissions", query, res -> {
      if(!res.succeeded()) {
        future.fail("Unable to complete query");
      } else {
        LinkedList<JsonObject> permissionObjectList = new LinkedList<>();
        JsonObject permissionObject = res.result().get(0);
        permissionObjectList.add(permissionObject);
        while(!permissionObjectList.isEmpty()) {
          JsonObject curPermObj = permissionObjectList.pop();
          JsonArray subPermissions = curPermObj.getJsonArray("sub_permissions");
          for(Object o : subPermissions) {
            String sub = (String)o;
          }
          
        }
      }
    });
    return future;
  }
  
  private Future<JsonArray> walkPerms(String permission) {
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
            Future<JsonArray> newFuture = walkPerms(sub);
            futureList.add(newFuture);
          }
          CompositeFuture compositeFuture = CompositeFuture.all(futureList);
          compositeFuture.compose(res2 -> {
            //Get output of contained futures and complete the future here
          }, future);
        } else {
          future.complete(new JsonArray().add(permission));
        }
      }
    });
    return future;
  }

}
