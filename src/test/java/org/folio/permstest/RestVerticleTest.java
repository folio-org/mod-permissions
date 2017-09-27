package org.folio.permstest;

import java.net.HttpURLConnection;
import java.net.URLEncoder;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;

import io.vertx.core.Future;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientRequest;
import io.vertx.core.http.HttpClientResponse;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.json.JsonArray;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import java.util.ArrayList;
import java.util.List;


@RunWith(VertxUnitRunner.class)
public class RestVerticleTest {

  private static final String       SUPPORTED_CONTENT_TYPE_JSON_DEF = "application/json";
  private static final String       SUPPORTED_CONTENT_TYPE_TEXT_DEF = "text/plain";
  private static final String userId1 = "35d05a6a-d61e-4e81-9708-fc44daadbec5";
  private static final String userId2 = "176bc0cc-b785-4cf9-9e8a-5fafe8178332";

  private static String postPermUsersRequest = "{\"userId\": \"93cb7ed4-313e-4f06-bd4b-d44b1308c3f3\",\"permissions\": ["+
    "{\"permissionName\": \"a\", \"displayName\": \"b\"  } ], \"id\" : \"" + userId2 + "\"}";

  private static String postPermRequest = "{\"permissionName\":\"a\",\"displayName\":\"b\"}";


  private static Vertx vertx;
  static int port;

  @Rule
  public Timeout rule = Timeout.seconds(180);  // 3 minutes for loading embedded postgres

  @BeforeClass
  public static void setup(TestContext context) {
    Async async = context.async();
    port = NetworkUtils.nextFreePort();
    TenantClient tenantClient = new TenantClient("localhost", port, "diku", "diku");
    vertx = Vertx.vertx();
    DeploymentOptions options = new DeploymentOptions().setConfig(new JsonObject().put("http.port", port));
    try {
      PostgresClient.setIsEmbedded(true);
      PostgresClient.getInstance(vertx).startEmbeddedPostgres();
    } catch(Exception e) {
      e.printStackTrace();
      context.fail(e);
      return;
    }
    vertx.deployVerticle(RestVerticle.class.getName(), options, res -> {
      try {
        tenantClient.post(null, res2 -> {
           async.complete();
        });
      } catch(Exception e) {
        e.printStackTrace();
      }

    });
  }

  @AfterClass
  public static void teardown(TestContext context) {
    Async async = context.async();
    vertx.close(context.asyncAssertSuccess( res-> {
      PostgresClient.stopEmbeddedPostgres();
      async.complete();
    }));
  }

/*
	Call our various tests for the permissions module, but do so in a sequential fashion,
	chaning each future's completion to the next in line
*/
 @Test
 public void testPermsSeq(TestContext context) {
   Async async = context.async();
   Future<Void> mainFuture;
   Future<Void> beginFuture = Future.future();
   sendPermissionSet(context).setHandler(beginFuture.completer());
   mainFuture = beginFuture.compose(v -> {
     Future<JsonObject> f = Future.future();
     postPermUser(context).setHandler(f.completer());
     return f;
   }).compose(permsUserObject -> {
     Future<Void> f = Future.future();
     testUserPerms(context, permsUserObject.getString("id")).setHandler(f.completer());
     return f;
   }).compose(v -> {
		 Future<Void> f = Future.future();
		 testUserPermsQuery(context).setHandler(f.completer());
		 return f;
	 }).compose(v -> {
     Future<Void> f = Future.future();
     testTenantPermissionVisible(context).setHandler(f.completer());
     return f;
   }).compose( v-> {
     Future<Void> f = Future.future();
     testPostBadPermission(context).setHandler(f.completer());
     return f;
   })
   /*
   	 .compose(v -> {
   	 Future<Void> f = Future.future();
   	 testNonAsciiUser(context).setHandler(f.completer());
   	 return f;
	 })
	 */;

   mainFuture.setHandler(res -> {
     if(res.succeeded()) {
       async.complete();
     } else {
       context.fail(res.cause());
     }
   });
 }

 @Test
 public void testGroup(TestContext context){
   String url = "http://localhost:"+port+"/perms/users";
   
   String permUrl = "http://localhost:"+port+"/perms/permissions";
   String userUrl = "http://localhost:"+port+"/perms/users";
   try {
    /**add a perm for a user */
     CompletableFuture<Response> addPUCF = new CompletableFuture();
     String addPUURL = url;
     send(addPUURL, context, HttpMethod.POST, postPermUsersRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPNoBodyResponseHandler(addPUCF));
     Response addPUResponse = addPUCF.get(5, TimeUnit.SECONDS);
     context.assertEquals(addPUResponse.code, HttpURLConnection.HTTP_CREATED);
     System.out.println("Status - " + addPUResponse.code + " at " +
         System.currentTimeMillis() + " for " + addPUURL);

     /**add a perm for a user again 422 */
     CompletableFuture<Response> addPUCF2 = new CompletableFuture();
     String addPUURL2 = url;
     send(addPUURL2, context, HttpMethod.POST, postPermUsersRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 422,  new HTTPResponseHandler(addPUCF2));
     Response addPUResponse2 = addPUCF2.get(5, TimeUnit.SECONDS);
     context.assertEquals(addPUResponse2.code, 422);
     System.out.println(addPUResponse2.body +
       "\nStatus - " + addPUResponse2.code + " at " + System.currentTimeMillis() + " for "
         + addPUURL2);

     String addPermURL = url + "/" + userId2 + "/permissions";
     /**add a perm  for a user */
     CompletableFuture<Response> addPUCF3 = new CompletableFuture();
     send(addPermURL, context, HttpMethod.POST, postPermRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addPUCF3));
     Response addPUResponse3 = addPUCF3.get(5, TimeUnit.SECONDS);
     context.assertEquals(addPUResponse3.code, HttpURLConnection.HTTP_OK);
     System.out.println(addPUResponse3.body +
       "\nStatus - " + addPUResponse3.code + " at " + System.currentTimeMillis() + " for "
         + addPermURL);

     /**add a perm  for a user again 422 */
     CompletableFuture<Response> addPUCF4 = new CompletableFuture();
     send(addPermURL, context, HttpMethod.POST, postPermRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 422,  new HTTPResponseHandler(addPUCF4));
     Response addPUResponse4 = addPUCF4.get(5, TimeUnit.SECONDS);
     context.assertEquals(addPUResponse4.code, 422);
     System.out.println(addPUResponse4.body +
       "\nStatus - " + addPUResponse4.code + " at " + System.currentTimeMillis() + " for "
         + addPermURL);

     /**add a perm */
     String addPermRL2 = "http://localhost:"+port+"/perms/permissions";
     CompletableFuture<Response> addPerms = new CompletableFuture();
     send(addPermRL2, context, HttpMethod.POST, postPermRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addPerms));
     Response addPermsResponse = addPerms.get(5, TimeUnit.SECONDS);
     context.assertEquals(addPermsResponse.code, HttpURLConnection.HTTP_CREATED);
     System.out.println(addPermsResponse.body +
       "\nStatus - " + addPermsResponse.code + " at " + System.currentTimeMillis() + " for "
         + addPermRL2);

     /**add a perm again 422 */
     CompletableFuture<Response> addPerms2 = new CompletableFuture();
     send(addPermRL2, context, HttpMethod.POST, postPermRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 422,  new HTTPResponseHandler(addPerms2));
     Response addPermsResponse2 = addPerms2.get(5, TimeUnit.SECONDS);
     context.assertEquals(addPermsResponse2.code, 422);
     System.out.println(addPermsResponse2.body +
       "\nStatus - " + addPermsResponse2.code + " at " + System.currentTimeMillis() + " for "
         + addPermRL2);
     
     /* Add a new permission */
     String newPermId = null;     
     JsonObject addNewPermRequestObject = new JsonObject()
             .put("permissionName", "foo.all")
             .put("displayName", "foo all")
             .put("description", "All foo permissions");
     {
       CompletableFuture<Response> addNewPermCF = new CompletableFuture();
       send(permUrl, context, HttpMethod.POST, addNewPermRequestObject.encode(),
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,
               new HTTPResponseHandler(addNewPermCF));
       Response addNewPermResponse = addNewPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(addNewPermResponse.code, 201);
       newPermId = addNewPermResponse.body.getString("id");
     }
     
     /* Attempt to add the same permission */
     {
       CompletableFuture<Response> reAddNewPermCF = new CompletableFuture();
       send(permUrl, context, HttpMethod.POST, addNewPermRequestObject.encode(),
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 422,
               new HTTPResponseHandler(reAddNewPermCF));
       Response reAddNewPermResponse = reAddNewPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(reAddNewPermResponse.code, 422);
     }
     
     /* Add a second permission */
     {
       JsonObject addSecondPermRequestObject = new JsonObject()
               .put("permissionName", "foo.whizz")
               .put("displayName", "foo whizz")
               .put("description", "Whizz a foo");
       CompletableFuture<Response> addSecondPermCF = new CompletableFuture();
       send(permUrl, context, HttpMethod.POST, addSecondPermRequestObject.encode(),
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,
               new HTTPResponseHandler(addSecondPermCF));
       Response addSecondPermResponse = addSecondPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(addSecondPermResponse.code, 201);
     }
     
     /* Modify the first permission to make the second a subpermission */
     {
       JsonObject modifyNewPermRequestObject = new JsonObject()
               .put("permissionName", "foo.all")
               .put("displayName", "foo all")
               .put("description", "All foo permissions")
               .put("id", newPermId)
               .put("subPermissions", new JsonArray().add("foo.whizz"));
       CompletableFuture<Response> modifyNewPermCF = new CompletableFuture();
       send(permUrl + "/" + newPermId, context, HttpMethod.PUT, modifyNewPermRequestObject.encode(),
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
               new HTTPResponseHandler(modifyNewPermCF));
       Response modifyNewPermResponse = modifyNewPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(modifyNewPermResponse.code, 200);
     }
 
     /* Get the first permission, check for subpermission */
     {
       CompletableFuture<Response> getNewPermCF = new CompletableFuture();
       send(permUrl + "/" + newPermId, context, HttpMethod.GET, null,
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
               new HTTPResponseHandler(getNewPermCF));
       Response getNewPermResponse = getNewPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getNewPermResponse.code, 200);
       context.assertNotNull(getNewPermResponse.body.getJsonArray("subPermissions"));
       context.assertTrue(getNewPermResponse.body.getJsonArray("subPermissions").contains("foo.whizz"));
     }
     /* Add a new user */
     String newUserId;
     JsonObject addNewUserObject = new JsonObject()
               .put("userId", "5a94d5bd-f76b-4af6-bfe9-497e80094114")
               .put("permissions", new JsonArray());
     {
       CompletableFuture<Response> addNewUserCF = new CompletableFuture();
       send(userUrl, context, HttpMethod.POST, addNewUserObject.encode(),
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,
               new HTTPResponseHandler(addNewUserCF));
       Response addNewUserResponse = addNewUserCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(addNewUserResponse.code, 201);
       newUserId = addNewUserResponse.body.getString("id");
       context.assertNotNull(newUserId);
     }
     
     /* Attempt to add the same user */
     {
       CompletableFuture<Response> addSameUserCF = new CompletableFuture();
       send(userUrl, context, HttpMethod.POST, addNewUserObject.encode(),
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 422,
               new HTTPResponseHandler(addSameUserCF));
       Response addSameUserResponse = addSameUserCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(addSameUserResponse.code, 422);
     }
     /* Add the permission to the user */
     
     JsonObject addPermToUserObject = new JsonObject()
             .put("permissionName", "foo.all");
     {
       CompletableFuture<Response> addPermToUserCF = new CompletableFuture();
       send(userUrl + "/" + newUserId + "/permissions", context, HttpMethod.POST,
               addPermToUserObject.encode(), SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
               new HTTPResponseHandler(addPermToUserCF));
       Response addPermToUserResponse = addPermToUserCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(addPermToUserResponse.code, 200);
     }
     
     /* Get a list of permissions that the user has */
     {
       CompletableFuture<Response> getUserPermsCF = new CompletableFuture();
       send(userUrl + "/" + newUserId + "/permissions", context, HttpMethod.GET,
               null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, 
               new HTTPResponseHandler(getUserPermsCF));
       Response getUserPermsResponse = getUserPermsCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getUserPermsResponse.code, 200);
       context.assertNotNull(getUserPermsResponse.body.getJsonArray("permissionNames"));
       context.assertTrue(getUserPermsResponse.body.getJsonArray("permissionNames").contains("foo.all"));
     }
     /* Get a list of permissions the user has with full subpermissions */
     
     /* Get a list of permissions the user has with expanded subpermissions */
     
     /* Delete the permission from the user */
     
     /* Get the user's permissions, make sure the permission is not present */
     
     /* Delete the user */
     
     /* Attempt to retrieve the user */
     
     /* Delete the permission */
     
     /* Attempt to retrieve the permission */
     
     

  } catch (Exception e) {
    e.printStackTrace();
    context.fail(e.getMessage());
  }
 }

 private void send(String url, TestContext context, HttpMethod method, String content,
     String contentType, int errorCode, Handler<HttpClientResponse> handler) {
   HttpClient client = vertx.createHttpClient();
   HttpClientRequest request;
   if(content == null){
     content = "";
   }
   Buffer buffer = Buffer.buffer(content);

   if (method == HttpMethod.POST) {
     request = client.postAbs(url);
   }
   else if (method == HttpMethod.DELETE) {
     request = client.deleteAbs(url);
   }
   else if (method == HttpMethod.GET) {
     request = client.getAbs(url);
   }
   else {
     request = client.putAbs(url);
   }
   request.exceptionHandler(error -> {
     context.fail(error.getMessage());
   })
   .handler(handler);
   request.putHeader("Authorization", "diku");
   request.putHeader("x-okapi-tenant", "diku");
   request.putHeader("Accept", "application/json,text/plain");
   request.putHeader("Content-type", contentType);
   request.end(buffer);
 }

 class HTTPResponseHandler implements Handler<HttpClientResponse> {

   CompletableFuture<Response> event;
   public HTTPResponseHandler(CompletableFuture<Response> cf){
     event = cf;
   }
   @Override
   public void handle(HttpClientResponse hcr) {
     hcr.bodyHandler( bh -> {
       Response r = new Response();
       r.code = hcr.statusCode();
       r.body = bh.toJsonObject();
       event.complete(r);
     });
   }
 }

 class HTTPNoBodyResponseHandler implements Handler<HttpClientResponse> {

   CompletableFuture<Response> event;
   public HTTPNoBodyResponseHandler(CompletableFuture<Response> cf){
     event = cf;
   }
   @Override
   public void handle(HttpClientResponse hcr) {
     Response r = new Response();
     r.code = hcr.statusCode();
     event.complete(r);
   }
 }

 class Response {
   int code;
   JsonObject body;
 }

 private boolean isSizeMatch(Response r, int size){
   if(r.body.getInteger("totalRecords") == size){
     return true;
   }
   return false;
 }

 private Future<Void> sendPermissionSet(TestContext context) {
   Future future = Future.future();
   JsonObject permissionSet = new JsonObject()
    .put("moduleId","dummy")
    .put("perms", new JsonArray()
      .add(new JsonObject()
        .put("permissionName", "dummy.read")
        .put("displayName", "Dummy Read")
        .put("description", "Read Dummy Entries")
        .put("visible", true)
      )
      .add(new JsonObject()
        .put("permissionName", "dummy.write")
        .put("displayName", "Dummy Write")
        .put("description", "Write Dummy Entries")
      )
      .add(new JsonObject()
        .put("permissionName", "dummy.all")
        .put("displayName", "Dummy All")
        .put("description", "All Dummy Permissions")
        .put("subPermissions", new JsonArray()
          .add("dummy.read")
          .add("dummy.write")
        )
      )
    );
   HttpClient client = vertx.createHttpClient();
   client.post(port, "localhost", "/_/tenantpermissions", res -> {
     if(res.statusCode() == 201) {
        future.complete();
     } else {
      res.bodyHandler(buf -> {
          future.fail("Post Permission set failed. Got return code " + res.statusCode() + " : " + buf.toString());
      });
    }
   })
    .putHeader("X-Okapi-Tenant", "diku")
    .putHeader("Content-type", "application/json")
    .putHeader("Accept", "application/json,text/plain")
    .end(permissionSet.encode());
    return future;
  }

  private Future<JsonObject> postPermUser(TestContext context) {
    Future future = Future.future();
    HttpClient client = vertx.createHttpClient();
    JsonObject newUser = new JsonObject()
      .put("userId", userId1)
      .put("permissions", new JsonArray().add("dummy.all"));
    client.post(port, "localhost", "/perms/users", res -> {
    	res.bodyHandler(buf -> {
    		if(res.statusCode() == 201) {
    			JsonObject permUser = new JsonObject(buf.toString());
    			future.complete(permUser);	
    		} else {
    			future.fail("Post permission user failed. Got return code " + res.statusCode() + " : " + buf.toString());
    		}
			});
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("Content-type", "application/json")
      .putHeader("Accept", "application/json,text/plain")
      .end(newUser.encode());
    return future;
  }

  private Future<Void> testUserPerms(TestContext context, String permsUserId) {
    Future future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/perms/users/" + permsUserId +
    		"/permissions?expanded=true", res -> {
      if(res.statusCode() != 200) {
        res.bodyHandler(buf -> {
          future.fail("Get permissions failed. Got return code " + res.statusCode() + " : " + buf.toString());
        });
      } else {
        res.bodyHandler(buf -> {
          JsonObject result = new JsonObject(buf.toString());
          JsonArray nameList = result.getJsonArray("permissionNames");
          if(nameList.contains("dummy.read") && nameList.contains("dummy.write")) {
            future.complete();
          } else {
            future.fail("Namelist does not contain 'dummy.read' and 'dummy.write' " + "( " + buf.toString() + " )");
          }
        });
      }
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("Content-type", "application/json")
      .putHeader("Accept", "application/json,text/plain")
      .putHeader("X-Okapi-Permissions", "[ \"perms.users.get\" ]")
      .end();
    return future;
  }
	
	private Future<Void> testUserPermsQuery(TestContext context) {
    Future future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/perms/users?query=permissions=dummy*", res -> {
      if(res.statusCode() != 200) {
        res.bodyHandler(buf -> {
          future.fail("Query failed. Got return code " + res.statusCode() + " : " + buf.toString());
        });
      } else {
        res.bodyHandler(buf -> {
          JsonObject result = new JsonObject(buf.toString());
          JsonArray userList = result.getJsonArray("permissionUsers");
					JsonObject userObject = null;
					for(Object ob : userList) {
						JsonObject userCandidateObject = (JsonObject)ob;
						if(userCandidateObject.getString("userId").equals(userId1)) {
							userObject = userCandidateObject;
							break;
						} 
					}
					if(userObject == null) {
						future.fail("Permissions record for userId matching " +
								userId1 + " not found in permissionUsers listing");
						return;
					} else {
						future.complete();
					}
        });
      }
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("Content-type", "application/json")
      .putHeader("Accept", "application/json,text/plain")
      .putHeader("X-Okapi-Permissions", "[ \"perms.users.get\" ]")
      .end();
    return future;
  }

  private Future<Void> testTenantPermissionVisible(TestContext context) {
    Future future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/perms/permissions?query=permissionName=dummy*", res -> {
        if(res.statusCode() != 200) {
          res.bodyHandler(buf -> {
            future.fail("Unable to get permissions. Got return code " + res.statusCode() + " : " + buf.toString());
          });
        } else {
          res.bodyHandler(buf -> {
            JsonObject result = new JsonObject(buf.toString());
            JsonArray permList = result.getJsonArray("permissions");
            boolean dummyReadFound = false;
            boolean dummyWriteFound = false;
            for( Object ob : permList ) {
              JsonObject permJson = (JsonObject)ob;
              if(permJson.getString("permissionName").equals("dummy.read")) {
                dummyReadFound = true;
                boolean visible = permJson.getBoolean("visible");
                if(visible != true) {
                  future.fail("visible field of dummy.read should be true. Value is " + visible);
                  return;
                }
              } else if(permJson.getString("permissionName").equals("dummy.write")) {
                dummyWriteFound = true;
                boolean visible = permJson.getBoolean("visible");
                if(visible != false) {
                  future.fail("visible field of dummy.write should be true. Value is " + visible);
                  return;
                }

              }
            }
            if(dummyReadFound && dummyWriteFound) {
              future.complete();
            } else {
              future.fail("Unable to locate all added permissions");
            }
          });
        }
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("Content-type", "application/json")
      .putHeader("Accept", "application/json,text/plain")
      .putHeader("X-Okapi-Permissions", "[ \"perms.users.get\" ]")
      .end();
    return future;
  }


  private Future<Void> testPostBadPermission(TestContext context) {
    Future future = Future.future();
    JsonObject badPermission = new JsonObject()
      .put("permissionName", "setOne")
      .put("subPermissions", new JsonArray()
        .add(new JsonObject()
          .put("permissionName", "dummy.read")
         )
        .add(new JsonObject()
          .put("permissionName", "dummy.write")
        )
      );
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/perms/permissions", res -> {
      if(res.statusCode() != 400) {
        res.bodyHandler(buf -> {
          future.fail("Error, expected code 422, got return code '" +
            res.statusCode() + "' : " + buf.toString());
        });
      } else {
        future.complete();
      }
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("Content-Type", "application/json")
      .putHeader("Accept", "application/json,text/plain")
      .end(badPermission.encode());
    return future;
  }

  private Future<Void> testNonAsciiUser(TestContext context) {
    Future future = Future.future();
    HttpClient client = vertx.createHttpClient();
    JsonObject newUser = new JsonObject()
      .put("username", "sschönberger")
      .put("permissions", new JsonArray());
    client.post(port, "localhost", "/perms/users", res -> {
      if(res.statusCode() == 201) {
            //Try to retrieve the new user
        HttpClient getClient = vertx.createHttpClient();
        try {
          client.get(port, "localhost", "/perms/users/" + URLEncoder.encode("sschönberger"), getRes -> {
            if (getRes.statusCode() == 200) {
              future.complete();
            } else {
              getRes.bodyHandler(body -> {
                future.fail("Expected status code 200, got " + getRes.statusCode()
                        + " : " + body.toString());
              });
            }
          })
                  .putHeader("X-Okapi-Tenant", "diku")
                  .putHeader("Content-type", "application/json")
                  .putHeader("Accept", "application/json,text/plain")
                  .putHeader("X-Okapi-Permissions", "[ \"perms.users.get\" ]")
                  .end();
        } catch (Exception e) {
          future.fail(e);
        }
      } else {
        res.bodyHandler(buf -> {
          future.fail("Post permission user failed. Got return code " + res.statusCode() + " : " + buf.toString());
        });
      }
    })
      .putHeader("X-Okapi-Tenant", "diku")
      .putHeader("Content-type", "application/json")
      .putHeader("Accept", "application/json,text/plain")
      .end(newUser.encode());
    return future;
  }

}
