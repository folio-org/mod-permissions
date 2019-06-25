package org.folio.permstest;

import java.net.HttpURLConnection;
import java.net.URLEncoder;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.impl.PermsCache;
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
import io.vertx.core.http.CaseInsensitiveHeaders;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientRequest;
import io.vertx.core.http.HttpClientResponse;
import io.vertx.core.http.HttpMethod;
import static io.vertx.core.http.HttpMethod.GET;
import static io.vertx.core.http.HttpMethod.POST;
import static io.vertx.core.http.HttpMethod.PUT;
import io.vertx.core.json.JsonObject;
import io.vertx.core.json.JsonArray;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;
import org.folio.permstest.TestUtil.WrappedResponse;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;

@RunWith(VertxUnitRunner.class)
public class RestVerticleTest {

  private static final String       SUPPORTED_CONTENT_TYPE_JSON_DEF = "application/json";
  private static final String       SUPPORTED_CONTENT_TYPE_TEXT_DEF = "text/plain";
  private static final String userId1 = "35d05a6a-d61e-4e81-9708-fc44daadbec5";
  private static final String userId2 = "176bc0cc-b785-4cf9-9e8a-5fafe8178332";
  private static final String userId3 = "f36400e5-ec5e-4e6c-abac-25fc42e1ec47";

  /* 
  private static String postPermUsersRequest = "{\"userId\": \"93cb7ed4-313e-4f06-bd4b-d44b1308c3f3\",\"permissions\": ["+
    "{\"permissionName\": \"a\", \"displayName\": \"b\"  } ], \"id\" : \"" + userId2 + "\"}";
  */
  
  private static final String userUserId = "93cb7ed4-313e-4f06-bd4b-d44b1308c3f3";
  private static String postPermUsersRequest = "{\"userId\": \""+ userUserId +"\",\"permissions\": " +
    "[], \"id\" : \"" + userId2 + "\"}";
  
  private static String postBadPermUsersRequest = "{\"userId\": \"93cb7ed4-313e-4f06-bd4b-d44b1308c3f3\",\"permissions\": " +
    "[\"bunny.fufu\"], \"id\" : \"" + userId2 + "\"}";

  private static String postPermRequest = "{\"permissionName\":\"a\",\"displayName\":\"b\"}";
  
  private static String postPermUserPermRequest = "{\"permissionName\":\"a\"}";


  private static Vertx vertx;
  static int port;

  @Rule
  public Timeout rule = Timeout.seconds(180);  // 3 minutes for loading embedded postgres

  @BeforeClass
  public static void setup(TestContext context) {
    Async async = context.async();
    port = NetworkUtils.nextFreePort();
    TenantClient tenantClient = new TenantClient("http://localhost:" + port, "diku", "diku");
    vertx = Vertx.vertx();
    DeploymentOptions options = new DeploymentOptions().setConfig(new JsonObject()
            .put("http.port", port).put(PermsCache.CACHE_HEADER, false)).setWorker(true);
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
        TenantAttributes ta = new TenantAttributes();
        ta.setModuleTo("mod-permissions-1.0.0");
        List<Parameter> parameters = new LinkedList<>();
        parameters.add(new Parameter().withKey("loadSample").withValue("true"));
        ta.setParameters(parameters);
        tenantClient.postTenant(ta, res2 -> {
          async.complete();
        });
      } catch (Exception e) {
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
    Future<WrappedResponse> startFuture;
    startFuture = sendPermissionSet(context, false).compose(w -> {
      return sendPermissionSet(context, true);
    }).compose(w -> {
      return testUpdateParentPermission(context);
    }).compose(w -> {
      return testUpdateChildPermission(context);
    }).compose(w -> {
      return postPermUser(context, userId1);
    }).compose(w -> {
      return testUserPerms(context, w.getJson().getString("id"));
    }).compose(w -> {
      return postPermUser(context, userId3);
    }).compose(w -> {
      return putPermUserBad(context, w.getJson().getString("id"));
    }).compose(w -> {
      return testUserPermsQuery(context);
    }).compose(w -> {
      return testTenantPermissionVisible(context);
    }).compose(w -> {
      return testPostBadPermission(context);
    }).compose(w -> {
      return testPostNullPermissionName(context);
    }).compose(w -> {
      return testPermissionExists(context, "dummy.all");
    }).compose(w -> {
      return sendBadPermissionSet(context);
    }).compose(w -> {
      return testBadPermissionSet(context);
    }).compose(w-> {
      return sendOtherBadPermissionSet(context);
    }).compose(w -> {
      return sendAlienPermissionSet(context);
    }).compose(w -> {
      return testAlienPermissionSet(context);
    }).compose(w -> {
      return sendOtherPermissionSet(context);
    }).compose(w -> {
      return testPermUserMetadata(context);
    }).compose(w -> {
      return testPermMetadata(context);
    });

    startFuture.setHandler(res -> {
      if(res.failed()) {
        context.fail(res.cause());
      } else {
        async.complete();
      }
    });  
   }

 @Test
 public void testGroup(TestContext context){
   String url = "http://localhost:"+port+"/perms/users";
   
   String permUrl = "http://localhost:"+port+"/perms/permissions";
   String userUrl = "http://localhost:"+port+"/perms/users";
   try {
     /**add a perm */
     String addPermURL2 = "http://localhost:"+port+"/perms/permissions";
     CompletableFuture<Response> addPerms = new CompletableFuture();
     send(addPermURL2, context, HttpMethod.POST, postPermRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addPerms));
     Response addPermsResponse = addPerms.get(5, TimeUnit.SECONDS);
     context.assertEquals(addPermsResponse.code, HttpURLConnection.HTTP_CREATED);
     System.out.println(addPermsResponse.body +
       "\nStatus - " + addPermsResponse.code + " at " + System.currentTimeMillis() + " for "
         + addPermURL2);

     /**add a perm again 422 */
     CompletableFuture<Response> addPerms2 = new CompletableFuture();
     send(addPermURL2, context, HttpMethod.POST, postPermRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 422,  new HTTPResponseHandler(addPerms2));
     Response addPermsResponse2 = addPerms2.get(5, TimeUnit.SECONDS);
     context.assertEquals(addPermsResponse2.code, 422);
     System.out.println(addPermsResponse2.body +
       "\nStatus - " + addPermsResponse2.code + " at " + System.currentTimeMillis() + " for "
         + addPermURL2);
     
    /* add a perm user with a non-existent perm */
    {
      CompletableFuture<Response> addBadPUCF = new CompletableFuture();
      String addPUURL = url;
      send(addPUURL, context, HttpMethod.POST, postBadPermUsersRequest,
        SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addBadPUCF));
      Response addBadPUResponse = addBadPUCF.get(5, TimeUnit.SECONDS);
      System.out.println("Status - " + addBadPUResponse.code + " with body " +
              addBadPUResponse.body + " at " +
          System.currentTimeMillis() + " for " + addPUURL);
      context.assertEquals(addBadPUResponse.code, 422);    
    }
    
    /**add a perm user */
     CompletableFuture<Response> addPUCF = new CompletableFuture();
     String addPUURL = url;
     send(addPUURL, context, HttpMethod.POST, postPermUsersRequest,
       SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,  new HTTPResponseHandler(addPUCF));
     Response addPUResponse = addPUCF.get(5, TimeUnit.SECONDS);
     System.out.println("Status - " + addPUResponse.code + " with body " +
             addPUResponse.body + " at " +
         System.currentTimeMillis() + " for " + addPUURL);
     context.assertEquals(addPUResponse.code, HttpURLConnection.HTTP_CREATED);

     /**add a perm user again 422 */
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
     send(addPermURL, context, HttpMethod.POST, postPermUserPermRequest,
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
     
     
     /* Try to add a new permission with a non-existent sub */  
     JsonObject addNewBadPermRequestObject = new JsonObject()
             .put("permissionName", "foo.all")
             .put("displayName", "foo all")
             .put("description", "All foo permissions")
             .put("subPermissions", new JsonArray().add("foo.whizz"));
     {
       CompletableFuture<Response> addNewPermCF = new CompletableFuture();
       send(permUrl, context, HttpMethod.POST, addNewBadPermRequestObject.encode(),
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,
               new HTTPResponseHandler(addNewPermCF));
       Response addNewPermResponse = addNewPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(addNewPermResponse.code, 422);
     }
     
     /* Add a new permission */
     String newPermId = null;    
     String newPermId2 = null;
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
     
     /* Attempt to modify the permission with a non-existent subpermission */
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
       context.assertEquals(modifyNewPermResponse.code, 422);
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
       newPermId2 = addSecondPermResponse.body.getString("id");
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
     
     /* Get the second permission, check for childOf */
     {
       CompletableFuture<Response> getSecondPermCF = new CompletableFuture();
       send(permUrl + "/" + newPermId2, context, HttpMethod.GET, null,
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
               new HTTPResponseHandler(getSecondPermCF));
       Response getSecondPermResponse = getSecondPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getSecondPermResponse.code, 200);
       context.assertTrue(getSecondPermResponse.body.getString("permissionName").equals("foo.whizz"));
       context.assertNotNull(getSecondPermResponse.body.getJsonArray("childOf"));
       context.assertFalse(getSecondPermResponse.body.getJsonArray("childOf").isEmpty());
       context.assertTrue(getSecondPermResponse.body.getJsonArray("childOf").contains("foo.all"));
     }
     
     /*Retrieve all the permissions */
     {
       CompletableFuture<Response> getPermsCF = new CompletableFuture();
       send(permUrl, context, HttpMethod.GET, null,
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
               new HTTPResponseHandler(getPermsCF));
       Response getPermsResponse = getPermsCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getPermsResponse.code, 200);
       context.assertNotNull(getPermsResponse.body.getJsonArray("permissions"));
       context.assertFalse(getPermsResponse.body.getJsonArray("permissions").isEmpty());
       context.assertTrue(getPermsResponse.body.getInteger("totalRecords") > 1);
     }
     
     /* Add a new user */
     String newUserId;
     String newUserUserId = "626a7a5c-4b66-4d2f-981f-1df9757e2aa9";
     JsonObject addNewUserObject = new JsonObject()
               //.put("userId", "5a94d5bd-f76b-4af6-bfe9-497e80094114")
               .put("userId", newUserUserId)
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
     
     /*Get the permission, check for grantedTo    */
     
     {
       CompletableFuture<Response> getPermCF = new CompletableFuture();
       send(permUrl + "/" + newPermId, context, HttpMethod.GET, null,
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
               new HTTPResponseHandler(getPermCF));
       Response getPermResponse = getPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getPermResponse.code, 200);
       context.assertNotNull(getPermResponse.body.getJsonArray("grantedTo"));
       context.assertTrue(getPermResponse.body.getJsonArray("grantedTo")
               .contains(newUserId));
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
     {
       CompletableFuture<Response> getFullUserPermsCF = new CompletableFuture();
       send(userUrl + "/" + newUserId + "/permissions?full=true", context, HttpMethod.GET,
               null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, 
               new HTTPResponseHandler(getFullUserPermsCF));
       Response getFullUserPermsResponse = getFullUserPermsCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getFullUserPermsResponse.code, 200);
       context.assertNotNull(getFullUserPermsResponse.body.getJsonArray("permissionNames"));
       context.assertNotNull(getFullUserPermsResponse.body.getJsonArray("permissionNames").getJsonObject(0));
       context.assertNotNull(getFullUserPermsResponse.body.getJsonArray("permissionNames")
               .getJsonObject(0).getJsonArray("subPermissions").contains("foo.whizz"));
     }
     /* Get a list of permissions the user has with expanded subpermissions */
     {
       CompletableFuture<Response> getExpandedUserPermsCF = new CompletableFuture();
       send(userUrl + "/" + newUserId + "/permissions?expanded=true", context, HttpMethod.GET,
               null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, 
               new HTTPResponseHandler(getExpandedUserPermsCF));
       Response getExpandedUserPermsResponse = getExpandedUserPermsCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getExpandedUserPermsResponse.code, 200);
       context.assertNotNull(getExpandedUserPermsResponse.body.getJsonArray("permissionNames"));
       context.assertTrue(getExpandedUserPermsResponse.body.getJsonArray("permissionNames").contains("foo.all"));
       context.assertTrue(getExpandedUserPermsResponse.body.getJsonArray("permissionNames").contains("foo.whizz"));
     }
     
     /* Get a list of full and expanded permissions the user has */
      {
       CompletableFuture<Response> getFullExpandedUserPermsCF = new CompletableFuture();
       send(userUrl + "/" + newUserId + "/permissions?expanded=true&full=true", context, HttpMethod.GET,
               null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, 
               new HTTPResponseHandler(getFullExpandedUserPermsCF));
       Response getFullExpandedUserPermsResponse = getFullExpandedUserPermsCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getFullExpandedUserPermsResponse.code, 200);
       context.assertNotNull(getFullExpandedUserPermsResponse.body.getJsonArray("permissionNames"));
       try {
         boolean allFound = false;
         boolean whizzFound = false;
         JsonArray perms = getFullExpandedUserPermsResponse.body.getJsonArray("permissionNames");
         for(Object ob : perms) {
           JsonObject perm = (JsonObject)ob;
           if(perm.getString("permissionName").equals("foo.all")) {
             JsonArray subs = perm.getJsonArray("subPermissions");
             if(subs.contains("foo.whizz")) {
               allFound = true;
             }
           } else if(perm.getString("permissionName").equals("foo.whizz")) {
             whizzFound = true;
           } else {
             continue;
           }
         }
         if(!allFound) {
           context.fail("Did not locate permission for 'foo.all'");
         } else if(!whizzFound) {
           context.fail("Did not locate permission for 'foo.whizz'");
         }
       } catch(Exception e) {
         context.fail(e);
       }
     }
      
     /* Delete the child permission */
     {
      CompletableFuture<Response> deletePermCF = new CompletableFuture();
      send(permUrl + "/" + newPermId2, context, HttpMethod.DELETE, null,
              SUPPORTED_CONTENT_TYPE_JSON_DEF, 204,
              new HTTPResponseHandler(deletePermCF));
      Response deletePermResponse = deletePermCF.get(5, TimeUnit.SECONDS);
      context.assertEquals(deletePermResponse.code, 204);
     }
     
     /* Get the original permission, check that child is not in subpermissions */
     {
       CompletableFuture<Response> getPermCF = new CompletableFuture();
       send(permUrl + "/" + newPermId, context, HttpMethod.GET, null,
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
               new HTTPResponseHandler(getPermCF));
       Response getPermResponse = getPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getPermResponse.code, 200);
       context.assertFalse(getPermResponse.body.getJsonArray("subPermissions")
               .contains("foo.whizz"));
     }
       
     /* Delete the permission from the user */
     {
       CompletableFuture<Response> deleteUserPermsCF = new CompletableFuture();
       send(userUrl + "/" + newUserId + "/permissions/foo.all", context, HttpMethod.DELETE,
               null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, 
               new HTTPResponseHandler(deleteUserPermsCF));
       Response deleteUserPermsResponse = deleteUserPermsCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(deleteUserPermsResponse.code, 204);
     }
     
     /* Get the original permission, check that our user is no longer in grantedTo */
     {
       CompletableFuture<Response> getPermCF = new CompletableFuture();
       send(permUrl + "/" + newPermId, context, HttpMethod.GET, null,
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
               new HTTPResponseHandler(getPermCF));
       Response getPermResponse = getPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getPermResponse.code, 200);
       context.assertFalse(getPermResponse.body.getJsonArray("grantedTo")
               .contains(newUserId));
     }
     
     /* Get the user's permissions, make sure the permission is not present */
     {
       CompletableFuture<Response> getUserPermsCF = new CompletableFuture();
       send(userUrl + "/" + newUserId + "/permissions", context, HttpMethod.GET,
               null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, 
               new HTTPResponseHandler(getUserPermsCF));
       Response getUserPermsResponse = getUserPermsCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getUserPermsResponse.code, 200);
       context.assertNotNull(getUserPermsResponse.body.getJsonArray("permissionNames"));
       context.assertFalse(getUserPermsResponse.body.getJsonArray("permissionNames").contains("foo.all"));
     }
     
     /* Add another new permission */
     String anotherNewPermId = null;    
     JsonObject addAnotherNewPermRequestObject = new JsonObject()
             .put("permissionName", "moo.all")
             .put("displayName", "moo all")
             .put("description", "All moo permissions");
     {
       CompletableFuture<Response> addNewPermCF = new CompletableFuture();
       send(permUrl, context, HttpMethod.POST, addAnotherNewPermRequestObject.encode(),
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 201,
               new HTTPResponseHandler(addNewPermCF));
       Response addNewPermResponse = addNewPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(addNewPermResponse.code, 201);
       anotherNewPermId = addNewPermResponse.body.getString("id");
     }
     
     /* Add the new permission to the user via the user's userId */
     JsonObject addAnotherPermToUserObject = new JsonObject()
             .put("permissionName", "moo.all");
     {
       CompletableFuture<Response> addPermToUserCF = new CompletableFuture();
       send(userUrl + "/" + newUserUserId + "/permissions?indexField=userId", context, HttpMethod.POST,
               addAnotherPermToUserObject.encode(), SUPPORTED_CONTENT_TYPE_JSON_DEF, 200,
               new HTTPResponseHandler(addPermToUserCF));
       Response addPermToUserResponse = addPermToUserCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(addPermToUserResponse.code, 200);
     }
     
     /* check for presence of permission */
     {
       CompletableFuture<Response> getUserPermsCF = new CompletableFuture();
       send(userUrl + "/" + newUserId + "/permissions", context, HttpMethod.GET,
               null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, 
               new HTTPResponseHandler(getUserPermsCF));
       Response getUserPermsResponse = getUserPermsCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getUserPermsResponse.code, 200);
       context.assertNotNull(getUserPermsResponse.body.getJsonArray("permissionNames"));
       context.assertTrue(getUserPermsResponse.body.getJsonArray("permissionNames")
               .contains("moo.all"));
     }
     
     /* Delete the new permission from the user, via userId */
     {
       CompletableFuture<Response> deleteUserPermsCF = new CompletableFuture();
       send(userUrl + "/" + newUserUserId + "/permissions/moo.all?indexField=userId", context, HttpMethod.DELETE,
               null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, 
               new HTTPResponseHandler(deleteUserPermsCF));
       Response deleteUserPermsResponse = deleteUserPermsCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(deleteUserPermsResponse.code, 204);
     }
     
     /* check for non-presence of permission */
     {
       CompletableFuture<Response> getUserPermsCF = new CompletableFuture();
       send(userUrl + "/" + newUserId + "/permissions", context, HttpMethod.GET,
               null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 200, 
               new HTTPResponseHandler(getUserPermsCF));
       Response getUserPermsResponse = getUserPermsCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getUserPermsResponse.code, 200);
       context.assertNotNull(getUserPermsResponse.body.getJsonArray("permissionNames"));
       context.assertFalse(getUserPermsResponse.body.getJsonArray("permissionNames")
               .contains("moo.all"));
     }
     
     /* Delete the user */
    
     {
       CompletableFuture<Response> deleteUserCF = new CompletableFuture();
       send(userUrl + "/" + newUserId, context, HttpMethod.DELETE,
               null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 204, 
               new HTTPResponseHandler(deleteUserCF));
       Response deleteUserResponse = deleteUserCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(deleteUserResponse.code, 204);
     }
     
     /* Attempt to retrieve the user */
     {
       CompletableFuture<Response> getUserCF = new CompletableFuture();
       send(userUrl + "/" + newUserId, context, HttpMethod.GET,
               null, SUPPORTED_CONTENT_TYPE_JSON_DEF, 404, 
               new HTTPResponseHandler(getUserCF));
       Response getUserResponse = getUserCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getUserResponse.code, 404);
     }
     
     /* Delete the permission */     
     {
       CompletableFuture<Response> deletePermCF = new CompletableFuture();
       send(permUrl + "/" + newPermId, context, HttpMethod.DELETE, null,
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 204,
               new HTTPResponseHandler(deletePermCF));
       Response deletePermResponse = deletePermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(deletePermResponse.code, 204);
     }
     
     /* Attempt to retrieve the permission */
     {
       CompletableFuture<Response> getPermCF = new CompletableFuture();
       send(permUrl + "/" + newPermId, context, HttpMethod.GET, null,
               SUPPORTED_CONTENT_TYPE_JSON_DEF, 404,
               new HTTPResponseHandler(getPermCF));
       Response getPermResponse = getPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(getPermResponse.code, 404);
     }
     
     
     /*Delete a permission that's not there */
     {
       CompletableFuture<Response> deleteBadPermCF = new CompletableFuture();
       send(permUrl + "/ed145a0a-c4ff-46b3-8c44-62d89f32afea", context, 
               HttpMethod.DELETE, null,SUPPORTED_CONTENT_TYPE_JSON_DEF, 404,
               new HTTPResponseHandler(deleteBadPermCF) );
       Response deleteBadPermResponse = deleteBadPermCF.get(5, TimeUnit.SECONDS);
       context.assertEquals(deleteBadPermResponse.code, 404);
     }
     

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
   System.out.println("Sending " + method.toString() + " request to " +
           url + " with content '" + content + "'");
 }

 class HTTPResponseHandler implements Handler<HttpClientResponse> {

   CompletableFuture<Response> event;
   public HTTPResponseHandler(CompletableFuture<Response> cf){
     event = cf;
   }
   @Override
   public void handle(HttpClientResponse hcr) {
     hcr.bodyHandler( bh -> {
       try {
        Response r = new Response();
        r.code = hcr.statusCode();
        try {
          r.body = bh.toJsonObject();
        } catch(Exception e) {
          System.out.println("Warning: '" + bh.toString() + "' cannot be parsed as JSON");
          r.body = new JsonObject(); //Or should it be null?
        }
        System.out.println("Got code '" + hcr.statusCode() + "' and body '" +
                bh.toString() + "'");
        event.complete(r);
       } catch(Exception e) {
         event.completeExceptionally(e);
       }
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

 private Future<WrappedResponse> sendPermissionSet(TestContext context, boolean more) {
   Future<WrappedResponse> future = Future.future();
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
   
   if (more) {
     permissionSet.getJsonArray("perms")
       .add(new JsonObject()
         .put("permissionName", "dummy.delete")
         .put("displayName", "Dummy Delete")
         .put("description", "Delete Dummy Entries"))
       .getJsonObject(2).getJsonArray("subPermissions").add("dummy.delete");
   };
   
   CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
   headers.add("accept", "application/json,text/plain");
   TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
           HttpMethod.POST, headers, permissionSet.encode(), 201).setHandler(res -> {
     if(res.failed()) {
       future.fail(res.cause());
     } else {
       future.complete(res.result());
     }
   });
   
    return future;
  }
 
 // test update for parent permission
 private Future<WrappedResponse> testUpdateParentPermission(TestContext context) {
   Future<WrappedResponse> future = Future.future(); 
   testPermissionExists(context, "dummy.all", true).setHandler( testRes -> {
     if(testRes.failed()) {
       future.fail(testRes.cause());
     } else {
       WrappedResponse wr = testRes.result();
       JsonObject json = new JsonObject(wr.getBody());
       JsonArray subPermissions = json.getJsonArray("permissions").getJsonObject(0)
               .getJsonArray("subPermissions");
       if (subPermissions.size() != 3 || !subPermissions.contains("dummy.delete")) {
         future.fail("dummy.all should contain three " + subPermissions.toString() + 
             " subPermissions including dummy.delete");
       } else {
         future.complete(wr);
       }
     }
   });
   return future;
 }
 
 // test update for child permission
 private Future<WrappedResponse> testUpdateChildPermission(TestContext context) {
   Future<WrappedResponse> future = Future.future(); 
   testPermissionExists(context, "dummy.delete", true).setHandler( testRes -> {
     if(testRes.failed()) {
       future.fail(testRes.cause());
     } else {
       WrappedResponse wr = testRes.result();
       JsonObject json = new JsonObject(wr.getBody());
       JsonArray childOf = json.getJsonArray("permissions").getJsonObject(0)
               .getJsonArray("childOf");
       if (childOf.size() != 1 || !childOf.contains("dummy.all")) {
         future.fail("dummy.delete should be child of dummy.all");
       } else {
         future.complete(wr);
       }
     }
   });
   return future;
 }
 
 private Future<WrappedResponse> sendOtherPermissionSet(TestContext context) {
   Future<WrappedResponse> future = Future.future();
   JsonObject permissionSet = new JsonObject()
    .put("moduleId","silly")
    .put("perms", new JsonArray()
      .add(new JsonObject()
        .put("permissionName", "silly.all")
        .put("displayName", "Dummy All")
        .put("description", "All Dummy Permissions")
        .put("subPermissions", new JsonArray()
          .add("silly.some")
        )
      )
      .add(new JsonObject()
        .put("permissionName", "silly.some")
        .put("displayName", "Silly Some")
        .put("description", "Some Silly Permissions")
        .put("subPermissions", new JsonArray()
          .add("silly.write")
          .add("silly.read")
        )
      )
      .add(new JsonObject()
        .put("permissionName", "silly.read")
        .put("displayName", "Dummy Read")
        .put("description", "Read Dummy Entries")
        .put("visible", true)
      )
      .add(new JsonObject()
        .put("permissionName", "silly.write")
        .put("displayName", "Dummy Write")
        .put("description", "Write Dummy Entries")
      )
    );
   
   CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
   headers.add("accept", "application/json,text/plain");
   TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
           HttpMethod.POST, headers, permissionSet.encode(), 201).setHandler(res -> {
     if(res.failed()) {
       future.fail(res.cause());
     } else {
       future.complete(res.result());
     }
   });
   
    return future;
  }
 
  private Future<WrappedResponse> sendBadPermissionSet(TestContext context) {
   Future<WrappedResponse> future = Future.future();
   JsonObject permissionSet = new JsonObject()
    .put("moduleId","bad")
    .put("perms", new JsonArray()
      .add(new JsonObject()
        .put("permissionName", "bad.read")
        .put("displayName", "Bad Read")
        .put("description", "Read Bad Entries")
        .put("visible", true)
      )
      .add(new JsonObject()
        .put("permissionName", "bad.write")
        .put("displayName", "Bad Write")
        .put("description", "Write Bad Entries")
      )
      .add(new JsonObject()
        .put("permissionName", "bad.all")
        .put("displayName", "Bad All")
        .put("description", "All Bad Permissions")
        .put("subPermissions", new JsonArray()
          .add("bad.read")
          .add("bad.write")
          .add("bad.delete")
        )
      )
    );
   
   CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
   headers.add("accept", "application/json,text/plain");
   TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
           HttpMethod.POST, headers, permissionSet.encode(), 201).setHandler(res -> {
     if(res.failed()) {
       future.fail(new Exception(res.cause()));
     } else {
       future.complete(res.result());
     }
   });
   
    return future;
  }
  
  //need test to find bad.delete and verify that it is a dummy perm
  
  private Future<WrappedResponse> testBadPermissionSet(TestContext context) {
    Future<WrappedResponse> future = Future.future(); 
    testPermissionExists(context, "bad.delete", true).setHandler( testRes -> {
      if(testRes.failed()) {
        future.fail(testRes.cause());
      } else {
        WrappedResponse wr = testRes.result();
        JsonObject json = new JsonObject(wr.getBody());
        boolean dummy = json.getJsonArray("permissions").getJsonObject(0)
                .getBoolean("dummy");
        if(!dummy) {
          future.fail("bad.delete is not flagged as a dummy perm");
        } else {
          future.complete(wr);
        }
      }
    });
    return future;
  }
  
  private Future<WrappedResponse> sendOtherBadPermissionSet(TestContext context) {
   Future<WrappedResponse> future = Future.future();
   JsonObject permissionSet = new JsonObject()
    .put("moduleId","otherbad")
    .put("perms", new JsonArray()
      .add(new JsonObject()
        .put("permissionName", "otherbad.read")
        .put("displayName", "Bad Read")
        .put("description", "Read Bad Entries")
        .put("visible", true)
      )
      .add(new JsonObject()
        .put("permissionName", "otherbad.write")
        .put("displayName", "Bad Write")
        .put("description", "Write Bad Entries")
      )
      .add(new JsonObject()
        .put("permissionName", "otherbad.some")
        .put("displayName", "Bad Some")
        .put("description", "Some bad perms")      
        .put("subPermissions", new JsonArray()
          .add("alien.woo")
        )
      )
      .add(new JsonObject()
        .put("permissionName", "otherbad.all")
        .put("displayName", "Bad All")
        .put("description", "All Bad Permissions")
        .put("subPermissions", new JsonArray()
          .add("otherbad.read")
          .add("otherbad.write")
          .add("otherbad.delete")
        )
      )
    );
   
   CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
   headers.add("accept", "application/json,text/plain");
   TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
           HttpMethod.POST, headers, permissionSet.encode(), 201).setHandler(res -> {
     if(res.failed()) {
       future.fail(new Exception(res.cause()));
     } else {
       future.complete(res.result());
     }
   });
   
    return future;
  }
  
  private Future<WrappedResponse> sendAlienPermissionSet(TestContext context) {
   Future<WrappedResponse> future = Future.future();
   JsonObject permissionSet = new JsonObject()
    .put("moduleId","alien")
    .put("perms", new JsonArray()
      .add(new JsonObject()
        .put("permissionName", "alien.woo")
        .put("displayName", "Alien Woo")
        .put("description", "Woo an Alien (wtf?)")
        .put("visible", true)
      )
      .add(new JsonObject()
        .put("permissionName", "alien.all")
        .put("displayName", "Alien All")
        .put("description", "All Alien Permissions")
        .put("subPermissions", new JsonArray()
          .add("alien.woo")
        )
      )
    );
   
   CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
   headers.add("accept", "application/json,text/plain");
   TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
           HttpMethod.POST, headers, permissionSet.encode(), 201).setHandler(res -> {
     if(res.failed()) {
       future.fail(new Exception(res.cause()));
     } else {
       future.complete(res.result());
     }
   });
   
    return future;
  }
  
  //load a permission set that includes alien.woo
  //test that alien.woo is a real permission  
  private Future<WrappedResponse> testAlienPermissionSet(TestContext context) {
    Future<WrappedResponse> future = Future.future(); 
    testPermissionExists(context, "alien.woo").setHandler( testRes -> {
      if(testRes.failed()) {
        future.fail(testRes.cause());
      } else {
        WrappedResponse wr = testRes.result();
        JsonObject json = new JsonObject(wr.getBody());
        boolean dummy = json.getJsonArray("permissions").getJsonObject(0)
                .getBoolean("dummy");
        if(dummy) {
          future.fail("alien.woo is flagged as a dummy perm");
        } else {
          future.complete(wr);
        }
      }
    });
    return future;
  }
   
  
  private Future<WrappedResponse> postPermUser(TestContext context, String userId) {
    JsonObject newUser = new JsonObject()
      .put("userId", userId)
      .put("permissions", new JsonArray().add("dummy.all"));
    Future<WrappedResponse> future = Future.future();
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/users",
            POST, null, newUser.encode(), 201).setHandler(res -> {
      if(res.failed()) { future.fail(res.cause()); } else {
        future.complete(res.result());
      }
    });

    return future;
  }
  
  private Future<WrappedResponse> putPermUserBad(TestContext context,
          String permsUserId) {
    JsonObject modifiedUser = new JsonObject()
            .put("id", permsUserId)
            .put("userId", userId1)
            .put("permissions", new JsonArray().add("spurious.all"));
    Future<WrappedResponse> future = Future.future();
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/users/123",
            PUT, null, modifiedUser.encode(), 404).setHandler(res -> {
      if(res.failed()) {
        future.fail(res.cause());
      } else {
        future.complete(res.result());
      }
    });
    return future;
  }

  private Future<WrappedResponse> testUserPerms(TestContext context, String permsUserId) {
    CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
    headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
    Future<WrappedResponse> future = Future.future();
    TestUtil.doRequest(vertx, "http://localhost:"+port+"/perms/users/" + permsUserId +
            "/permissions?expanded=true", GET, headers, null, 200).setHandler(res -> {
      try {
        if(res.failed()) {
          future.fail(res.cause());
        } else {
          JsonArray nameList = res.result().getJson().getJsonArray("permissionNames");
          if(nameList == null) {
            future.fail("Could not find 'permissionNames' in " + res.result().getBody());
          } else {
            if(nameList.contains("dummy.read") && nameList.contains("dummy.write")) {
              future.complete(res.result());
            } else {
              future.fail("Namelist does not contain 'dummy.read' and 'dummy.write' " + "( " + res.result().getBody() + " )");
            }
          }
        }
      } catch(Exception e) {
        future.fail(e);
      }
    });
  
    return future;
  }

  private Future<WrappedResponse> testUserPermsQuery(TestContext context) {
    Future<WrappedResponse> future = Future.future();
    CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
    String url;
    try {
      headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
      url = "http://localhost:" + port + "/perms/users?query=" +
              URLEncoder.encode("permissions=dummy*", "UTF-8");
    } catch(Exception e) {
      future.fail(e);
      return future;
    }
    TestUtil.doRequest(vertx, url, GET, headers, null, 200).setHandler(res -> {
      if(res.failed()) {
        future.fail(res.cause());
      } else {
        try {
          JsonArray userList = res.result().getJson().getJsonArray("permissionUsers");
          JsonObject userObject = null;
          for (Object ob : userList) {
            JsonObject userCandidateObject = (JsonObject) ob;
            if (userCandidateObject.getString("userId").equals(userId1)) {
              userObject = userCandidateObject;
              break;
            }
          }
          if (userObject == null) {
            future.fail("Permissions record for userId matching "
                    + userId1 + " not found in permissionUsers listing");
            return;
          } else {
            future.complete(res.result());
          }
        } catch(Exception e) {
          future.fail(e);
        }
      }
    });

    return future;
  }

  private Future<WrappedResponse> testTenantPermissionVisible(TestContext context) {
    Future<WrappedResponse> future = Future.future();
    CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
    String url;
    try {
      headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
      url = "http://localhost:"+port+ "/perms/permissions?query=" + URLEncoder.encode("permissionName=dummy*", "UTF-8");
    } catch(Exception e) {
      future.fail(e);
      return future;
    }
    TestUtil.doRequest(vertx, url, GET, headers, null, 200).setHandler(res -> {
      if(res.failed()) { future.fail(res.cause()); } else {
        try {
          JsonArray permList = res.result().getJson().getJsonArray("permissions");
          boolean dummyReadFound = false;
          boolean dummyWriteFound = false;
          for (Object ob : permList) {
            JsonObject permJson = (JsonObject) ob;
            if (permJson.getString("permissionName").equals("dummy.read")) {
              dummyReadFound = true;
              boolean visible = permJson.getBoolean("visible");
              if (visible != true) {
                future.fail("visible field of dummy.read should be true. Value is " + visible);
                return;
              }
            } else if (permJson.getString("permissionName").equals("dummy.write")) {
              dummyWriteFound = true;
              boolean visible = permJson.getBoolean("visible");
              if (visible != false) {
                future.fail("visible field of dummy.write should be true. Value is " + visible);
                return;
              }
            }
          }
          if (dummyReadFound && dummyWriteFound) {
            future.complete(res.result());
          } else {
            future.fail(new Exception("Resultset does not contain 'dummy.read' and 'dummy.write'"));
          }
        } catch(Exception e) {
          future.fail(e);
        }
      }
    });
    
    return future;
  }


  private Future<WrappedResponse> testPostBadPermission(TestContext context) {
    Future<WrappedResponse> future = Future.future();
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
    TestUtil.doRequest(vertx, "http://localhost:"+port+"/perms/permissions", POST,
            null, badPermission.encode(), 400).setHandler(res -> {
      if(res.failed()) { future.fail(res.cause()); } else {
        future.complete(res.result());
      }
    });

    return future;
  }

  private Future<WrappedResponse> testPostNullPermissionName(TestContext context) {
    Future<WrappedResponse> future = Future.future();
    JsonObject nullPermission = new JsonObject()
      .put("displayName", "nullPermName");
    TestUtil.doRequest(vertx, "http://localhost:"+port+"/perms/permissions", POST,
            null, nullPermission.encode(), 201).setHandler(res -> {
      if(res.failed()) { future.fail(res.cause()); } else {
        future.complete(res.result());
      }
    });

    return future;
  }

  private Future<WrappedResponse> testNonAsciiUser(TestContext context) {
    Future<WrappedResponse> future = Future.future();
    String url = "http://localhost:" + port + "/perms/users";
    String url2;
    CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
     JsonObject newUser = new JsonObject()
      .put("username", "sschönberger")
      .put("permissions", new JsonArray());
    try {
      headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
      url2 = "http://localhost:"+port+"/perms/users" + URLEncoder.encode("sschönberger", "UTF-8");
    } catch(Exception e) {
      future.fail(e);
      return future;
    }
    TestUtil.doRequest(vertx, url, POST, null, newUser.encode(), 201).setHandler(res -> {
      if(res.failed()) { future.fail(res.cause()); } else {
        TestUtil.doRequest(vertx, url2, GET, headers, null, 200 ).setHandler(res2 -> {
          if(res2.failed()) { future.fail(res2.cause()); } else {
            future.complete(res2.result());
          }
        });
      }
    });   
    return future;
  }
  
  private Future<WrappedResponse> testPermissionExists(TestContext context,
          String permissionName, boolean includeDummies) {
    Future<WrappedResponse> future = Future.future();
    String dummyFlag;
    if(includeDummies) {
      dummyFlag = "includeDummy=true&";
    } else {
      dummyFlag = "";
    }
    String url = "http://localhost:" + port + "/perms/permissions?" + dummyFlag + 
            "query=permissionName=="+permissionName;
    TestUtil.doRequest(vertx, url, GET, null, null, 200).setHandler(res -> {
      if(res.failed()) {
        future.fail(res.cause());
      } else {
        WrappedResponse wr = res.result();
        JsonObject json = new JsonObject(wr.getBody());
        if(json.getInteger("totalRecords") < 1) {
          future.fail("permission " + permissionName + " not found");
        } else {
          future.complete(wr);
        }
      }
    });
    return future;
  }
  
  private Future<WrappedResponse> testPermissionExists(TestContext context,
          String permissionName) {
    return testPermissionExists(context, permissionName, false);
  }
  
  private Future<WrappedResponse> testPermUserMetadata(TestContext context) {
    Future<WrappedResponse> future = Future.future();
    String url = "http://localhost:" + port + "/perms/users";
    JsonObject newUser = new JsonObject()
        .put("userId", UUID.randomUUID().toString())
        .put("permissions", new JsonArray());
    CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
    String fakeUserId = UUID.randomUUID().toString();
    headers.add("X-Okapi-Token", makeFakeJWT("mcdonald", fakeUserId, "diku"));
    headers.add("X-Okapi-User-Id", fakeUserId);
    TestUtil.doRequest(vertx, url, POST, headers, newUser.encode(), 201).setHandler(
        res -> {
      if(res.failed()) { future.fail(res.cause()); } else {
        try {
          String newUserId = res.result().getJson().getString("id");
          String url2 = String.format("http://localhost:%s/perms/users/%s", port,
              newUserId);
          TestUtil.doRequest(vertx, url2, GET, null, null, 200).setHandler(res2 -> {
            try {
              JsonObject metadata = res2.result().getJson().getJsonObject("metadata");
              if(metadata == null) {
                future.fail("No metadata found in result: " + res2.result().getJson().encode());
              } else {
                future.complete(res2.result());
              }
            } catch(Exception e) {
              future.fail(e);
            }
          });
        } catch(Exception e) {
          future.fail(e);
        }              
      }
    });
    return future;
  }
  
    private Future<WrappedResponse> testPermMetadata(TestContext context) {
    Future<WrappedResponse> future = Future.future();
    String url = "http://localhost:" + port + "/perms/permissions";
    JsonObject newPerm = new JsonObject()
        .put("permissionName", "testmeta.test")
        .put("description", "a permission to test metadata create")
        .put("displayName", "testmeta test");
    CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
    String fakeUserId = UUID.randomUUID().toString();
    headers.add("X-Okapi-Token", makeFakeJWT("mcdonald", fakeUserId, "diku"));
    headers.add("X-Okapi-User-Id", fakeUserId);
    TestUtil.doRequest(vertx, url, POST, headers, newPerm.encode(), 201).setHandler(
        res -> {
      if(res.failed()) { future.fail(res.cause()); } else {
        try {
          String newPermId = res.result().getJson().getString("id");
          String url2 = String.format("http://localhost:%s/perms/permissions/%s", port,
              newPermId);
          TestUtil.doRequest(vertx, url2, GET, null, null, 200).setHandler(res2 -> {
            try {
              JsonObject metadata = res2.result().getJson().getJsonObject("metadata");
              if(metadata == null) {
                future.fail("No metadata found in result: "  + res2.result().getJson().encode());
              } else {
                future.complete(res2.result());
              }
            } catch(Exception e) {
              future.fail(e);
            }
          });
        } catch(Exception e) {
          future.fail(e);
        }              
      }
    });
    return future;
  }
  
  private static String makeFakeJWT(String username, String id, String tenant) {
   JsonObject header = new JsonObject()
           .put("alg", "HS512");
   JsonObject payload = new JsonObject()
           .put("sub", username)
           .put("user_id", id)
           .put("tenant", tenant);
   String ret = String.format("%s.%s.%s",
           Base64.getEncoder().encodeToString(header.encode()
                   .getBytes(StandardCharsets.UTF_8)),
           Base64.getEncoder().encodeToString(payload.encode()
                   .getBytes(StandardCharsets.UTF_8)),
           Base64.getEncoder().encodeToString((header.encode() + payload.encode())
                   .getBytes(StandardCharsets.UTF_8)));
   System.out.println("Generated fake JWT: " + ret);
   return ret;

 }
}

