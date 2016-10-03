/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.folio.auth.permissions_module.impl;

import io.vertx.core.AsyncResult;
import org.folio.auth.permissions_module.PermissionsStore;
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
  public Future<Boolean> addPermission(String permission, String tenant) {
    Future<Boolean> future = Future.future();
    JsonObject query = new JsonObject().put("permission_name", permission)
            .put("tenant", tenant);
    mongoClient.find("permissions", query, res -> {
      if(res.succeeded() && res.result().size() > 0) { //permission already exists
        future.fail("Permission already exists");
      } else {
        JsonObject insert = new JsonObject()
                .put("permission_name", permission)
                .put("tenant", tenant)
                .put("sub_permissions", new JsonArray());
        mongoClient.insert("permissions", insert, res2 -> {
          if(res2.succeeded()) {
            future.complete(true);
          } else {
            future.fail("Unable to insert permission: " + res2.cause().getMessage());
          }
        });
      }
    });
    return future;
  }

  @Override
  public Future<Boolean> addSubPermission(String permission, String sub, String tenant) {
    //TODO: Check for circular permissions
    Future<Boolean> future = Future.future();
    JsonObject query = new JsonObject().put("permission_name", permission)
            .put("tenant", tenant);
    mongoClient.find("permissions", query, res -> {
      if(res.failed() || res.result().size() < 1) {
        future.fail("Unable to find permission to add subs to"); //Can't add a sub to a non-existent perm
      } else {
        //JsonArray permissionList = res.result().get(0).getJsonArray("sub_permissions");
        //permissionList.add(sub);
        JsonObject update = new JsonObject()
                .put("$addToSet", new JsonObject()
                  .put("sub_permissions", sub));
        mongoClient.updateCollection("permissions", query, update, res2 -> {
          if(res2.succeeded()) {
            future.complete(true);
          } else {
            future.fail("Unable to update records: " + res2.cause().getMessage());
          }
        });
      }
    });
    return future;
  }
  
  @Override
  public Future<JsonArray> getSubPermissions(String permission, String tenant) {
    JsonObject query = new JsonObject()
            .put("permission_name", permission)
            .put("tenant", tenant);
    JsonArray permList = new JsonArray();
    Future<JsonArray> future = Future.future();
    mongoClient.find("permissions", query, res -> {
      if(res.failed() || res.result().size() < 1) {
        future.fail("Query unsuccessful");
      } else {
        JsonObject permObject = res.result().get(0);
        future.complete(permObject.getJsonArray("sub_permissions"));
      }
    });
    return future;        
  }

  @Override
  public Future<JsonArray> getExpandedPermissions(String permission, String tenant) {
    //System.out.println("Permissions> Expanding permission '"+ permission + "' on tenant '"+
    //        tenant + "'");
    JsonObject query = new JsonObject()
            .put("permission_name", permission)
            .put("tenant", tenant);
    JsonArray permList = new JsonArray();
    Future<JsonArray> future = Future.future();
    mongoClient.find("permissions", query, res -> {
      if(res.succeeded() && res.result().size() > 0) {
        //System.out.println("Permissions> Successfully queried mongo for '"+ permission + "' on tenant '"+
        //    tenant + "'");
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
        LinkedList<Future> futureList = new LinkedList<>();
        if(subPerms != null && !subPerms.isEmpty()) {
          System.out.println("Permissions> " + subPerms.size() + " subs to process for '" + permission + "'");
          for(Object o : subPerms) {
            String sub = (String)o;
            Future<JsonArray> newFuture = getExpandedPermissions(sub, tenant);
            futureList.add(newFuture);
          }
          System.out.println("Permissions> Creating CompositeFuture to expand " 
                  + permission + " into " + futureList.size() + " subs: " +
                  subPerms.encode());
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
              future.fail("Unable to populate permissions: " + res2.cause().getMessage());
            }
          });
        } else {
          //System.out.println("Permissions> No sub-permissions found for '" + permission + "'");
          future.complete(permList);
        }
      } else {
        future.fail("No permission '" + permission + "' found for tenant '" + tenant + "'");
      }
    });
    return future;
  }


  @Override
  public Future<Boolean> removePermission(String permission, String tenant) {
    Future<Boolean> future = Future.future();
    JsonObject query = new JsonObject()
            .put("permission_name", permission)
            .put("tenant", tenant);
    mongoClient.find("permissions", query, res-> {
      if(res.failed() || res.result().size() < 1) {
        future.fail("Unable to find permission to delete");
      } else {
        //Find all permissions that list this permission as a sub
        /*
        JsonObject subQuery = new JsonObject().put("sub_permissions", new JsonObject()
          .put("$in", new JsonArray().add(permission)));
         */
        JsonObject subUpdate = new JsonObject()
                .put("$pullAll", new JsonObject()
                  .put("sub_permissions", new JsonArray().add(permission)));
        mongoClient.updateCollectionWithOptions("permissions", new JsonObject(), subUpdate, updateOptions, res2-> {
          if(res2.failed()) {
            future.fail("Unable to remove sub permissions: " + res2.cause().getMessage());
          } else {
            //Now delete the actual permission, since the sub permissions are clean
            mongoClient.removeDocument("permissions", query, res3 -> {
              if(res3.failed()) {
                future.fail("Unable to delete permission: " + res3.cause().getMessage());
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
  public Future<Boolean> removeSubPermission(String permission, String sub, String tenant) {
    Future<Boolean> future = Future.future();
    JsonObject query = new JsonObject()
            .put("permission_name", permission)
            .put("tenant", tenant);
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
  public Future<Boolean> addUser(String user, String tenant) {
    JsonObject query = new JsonObject()
            .put("user_name", user)
            .put("tenant", tenant);
    Future<Boolean> future = Future.future();
    mongoClient.find("users", query, res-> {
      if(res.result().size() > 0) {
        future.complete(false);
      } else {
        JsonObject insert = new JsonObject()
                .put("user_name", user)
                .put("tenant", tenant)
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
  public Future<Boolean> removeUser(String user, String tenant) {
    JsonObject query = new JsonObject()
            .put("user_name", user)
            .put("tenant", tenant);
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
  public Future<Boolean> addPermissionToUser(String user, String permission, String tenant) {
    System.out.println("Permissions> Adding permission '" + permission + "' to user '" + user + "'");
    JsonObject query = new JsonObject()
            .put("user_name", user)
            .put("tenant", tenant);
    Future<Boolean> future = Future.future();
    mongoClient.find("users", query, res-> {
      if(res.failed()) {
        future.fail(res.cause().getMessage());
      } else {
        System.out.println("Found user successfully");
        JsonArray currentPermissions = res.result().get(0).getJsonArray("user_permissions");
        if(res.result().size() < 0) {
          future.complete(false);
        } else {
          this.getPermissionsForUser(user, tenant, false).setHandler(res2 -> {
          if(res2.failed()) {
            future.fail("Error querying current permissions: " + res2.cause().getMessage());
          } else {
            JsonArray currentPerms = res2.result();
            if(currentPerms.contains(permission)) {
              future.fail("Permission already exists in user");
            } else {
              JsonObject push = new JsonObject()
                      .put("$push", new JsonObject()
                              .put("user_permissions", permission));
              System.out.println("Using user query '" + query.encode() + "' and update query '" + push.encode() + "'");
              mongoClient.updateCollection("users", query, push, res3 -> {
                if (res3.failed()) {
                  future.fail("Unable to add permission:" + res3.cause().getMessage());
                } else {
                  System.out.println("Permissions> Permission '" + permission + "' added to user '" + user + "'");
                  future.complete(true);
                }
              });
            }
          }
        });
          /*
          JsonArray newPermissionsSet = new JsonArray();
          if(currentPermissions != null) {
            for(Object o : currentPermissions) {
              String s = (String)o;
              newPermissionsSet.add(s);
            }
          }
          newPermissionsSet.add(permission);
          JsonObject update = new JsonObject()
                  .put("$set", new JsonObject())
                    .put("user_permissions", newPermissionsSet);
          System.out.println("Using user query '" + query.encode() + "' and update query '"+update.encode()+"'");
          mongoClient.updateCollection("users", query, update, res2 -> {
            if(res2.failed()) {
              future.fail("Unable to add permission:" + res2.cause().getMessage());
            } else {
              future.complete(true);
            }
          });
          */
        }
      }
    });
    return future;
  }

  @Override
  public Future<Boolean> removePermissionFromUser(String user, String permission, String tenant) {
    JsonObject query = new JsonObject()
            .put("user_name", user)
            .put("tenant", tenant);
    Future<Boolean> future = Future.future();
    mongoClient.find("users", query, res-> {
      if(res.failed() || res.result().size() < 1) {
        future.complete(false);
      } else {
        getPermissionsForUser(user, tenant, false).setHandler( res2 -> {
          if(res2.failed()) {
            future.fail("Unable to retrieve initial permissions: " + res2.cause().getMessage());
          } else {
            JsonArray permissions = res2.result();
            System.out.println("PERMISSIONS: " + permissions.encode());
            if(!permissions.contains(permission)) {
              future.complete(true);
            } else {
              permissions.remove(permission);
              JsonObject update = new JsonObject().put("$set", new JsonObject()
                .put("user_permissions", permissions));
              mongoClient.updateCollection("users", query, update, res3 -> {
                if(res3.failed()) {
                  future.fail("Unable to remove permission:" + res3.cause().getMessage());
                } else {
                  System.out.println("Permissions> Permission '" + permission + "' removed");
                  future.complete(true);
                }
              });
            }
          }
        });
      }       
    });
    return future;
  }

  //TODO: Consider if we should add a flag to determine whether or not to
  //expand the permissionslist returned for the user
  @Override
  public Future<JsonArray> getPermissionsForUser(String user, String tenant) {
    return getPermissionsForUser(user, tenant, true);
  }
  
  public Future<JsonArray> getPermissionsForUser(String user, String tenant, Boolean expand) {
    JsonObject query = new JsonObject()
            .put("user_name", user)
            .put("tenant", tenant);
    Future<JsonArray> future = Future.future();
    mongoClient.find("users", query, (AsyncResult<List<JsonObject>> res)-> {
      if(res.result().size() < 1) {
        future.fail("No such user");
      } else {
        JsonObject userObject = res.result().get(0);
        System.out.println("Permissions> Permissions for user " + user + ": " + userObject.encode());
        JsonArray permissions = userObject.getJsonArray("user_permissions");
        if(expand) {
          ArrayList<Future> futureList = new ArrayList<>();
          for(Object o : permissions) {
            String permissionName = (String)o;
            Future<JsonArray> expandPermissionFuture = 
                    this.getExpandedPermissions(permissionName, tenant);
            futureList.add(expandPermissionFuture);
          }
          System.out.println("Permissions> Assembling CompositeFuture of " + futureList.size() + " permissions to expand");
          CompositeFuture compositeFuture = CompositeFuture.all(futureList);
          compositeFuture.setHandler(res2 -> {
            if(res2.failed()) {
              future.fail(res2.cause());
            } else {
              JsonArray allPermissions = new JsonArray();
              for(Future f : futureList) {
                JsonArray arr = (JsonArray)f.result();
                for(Object o : arr) {
                  String perm = (String)o;
                  if(!allPermissions.contains(perm)) {
                    allPermissions.add(perm);
                  }
                }
              }
              System.out.println("Permissions> Returning list of " + allPermissions.size() + " permissions");
              future.complete(allPermissions);
            }
          });
        } else {
          future.complete(permissions);
        }
      }
    });
    return future;
  }

  @Override
  public Future<JsonObject> getPermission(String permission, String tenant) {
    Future<JsonObject> future = Future.future();
    JsonObject query = new JsonObject()
            .put("permission_name", permission)
            .put("tenant", tenant);
    mongoClient.find("permissions", query, res -> {
      if(res.failed()) {
        future.fail(res.cause().getMessage());
      } else if(res.result().size() < 1) {
          future.fail("No results for that name"); 
      } else {
       future.complete(res.result().get(0)); 
      }
    });
    return future;//To change body of generated methods, choose Tools | Templates.
  }
  
  @Override
  public Future<JsonObject> getUser(String username, String tenant) {
    Future<JsonObject> future = Future.future();
    JsonObject query = new JsonObject()
            .put("user_name", username)
            .put("tenant", tenant);
    mongoClient.find("users", query, res -> {
      if(res.failed()) {
        future.fail(res.cause().getMessage());
      } else if(res.result().size() < 1) {
        future.fail("No results for that name");
      } else {
        future.complete(res.result().get(0));
      }
    });
    return future;
  }
}
