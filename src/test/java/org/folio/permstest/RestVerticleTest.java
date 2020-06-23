package org.folio.permstest;

import io.vertx.core.Future;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Handler;
import io.vertx.core.MultiMap;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientRequest;
import io.vertx.core.http.HttpClientResponse;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.json.JsonArray;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import java.net.URLEncoder;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.folio.permstest.TestUtil.WrappedResponse;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
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

import static org.folio.permstest.TestUtil.*;

@RunWith(VertxUnitRunner.class)
public class RestVerticleTest {
  private static final Logger logger = LoggerFactory.getLogger(RestVerticleTest.class);

  private static final String userId1 = "35d05a6a-d61e-4e81-9708-fc44daadbec5";
  private static final String userId2 = "176bc0cc-b785-4cf9-9e8a-5fafe8178332";
  private static final String userId3 = "f36400e5-ec5e-4e6c-abac-25fc42e1ec47";

  private static final String userUserId = "93cb7ed4-313e-4f06-bd4b-d44b1308c3f3";
  private static Vertx vertx;
  private static HttpClient client;
  static int port;

  @Rule
  public Timeout rule = Timeout.seconds(180);  // 3 minutes for loading embedded postgres

  @BeforeClass
  public static void setup(TestContext context) {
    Async async = context.async();
    port = NetworkUtils.nextFreePort();
    TenantClient tenantClient = new TenantClient("http://localhost:" + port, "diku",  null);
    vertx = Vertx.vertx();
    client = vertx.createHttpClient();
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
    client.close();
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
    }).compose(w -> {
      return sendNestedSubPerms(context);
    }).compose(w -> {
      return testNestedSubPermExpansion(context);
    }).compose(w -> {
      return sendNestedSubPermsWithException(context);
    }).compose(w -> {
      return testNestedSubPermExpansionWithExceptions(context);
    });

    startFuture.onComplete(res -> {
      if(res.failed()) {
        context.fail(res.cause());
      } else {
        async.complete();
      }
    });
  }

  @Test
  public void testGetPermsUsersByIdBadIndexField(TestContext context) {

    Response response = send(HttpMethod.GET, "/perms/users/123?indexField=bad", null, context);
    context.assertEquals(500, response.code);
  }

  @Test
  public void testGetPermsUsersByIdBadUUID(TestContext context) {
    Response response = send(HttpMethod.GET, "/perms/users/12%2334?indexField=id", null, context);
    context.assertEquals(404, response.code);
    context.assertEquals("No user with id: 12#34", response.body.getString("text"));
  }

  @Test
  public void testGetPermsUsersByIdBadTenant(TestContext context) {
    Response response = send("badTenant", HttpMethod.GET, "/perms/users/1234", null, context);
    context.assertEquals(400, response.code);
  }

  @Test
  public void testDeletePermsUsersByIdBadTenant(TestContext context) {
    Response response = send("badTenant", HttpMethod.DELETE, "/perms/users/1234", null, context);
    context.assertEquals(400, response.code);
  }

  @Test
  public void testPostPermsUsersPermissionsInvalidUUID(TestContext context) {
    String request = "{\"permissionName\":\"a\"}";

    Response response = send(HttpMethod.POST, "/perms/users/123/permissions",
        request, context);
    context.assertEquals(400, response.code);
  }

  @Test
  public void testPostPermsUsersInvalidUUID(TestContext context) {

    String permsUsers = "{\"userId\": \"1234\",\"permissions\": " +
        "[], \"id\" : \"1234\"}";

    Response response = send(HttpMethod.POST, "/perms/users",
        permsUsers, context);
    context.assertEquals(400, response.code);
  }

  @Test
  public void testPostPermsUsersNoUserId(TestContext context) {
    String permsUsers = "{\"permissions\": [], \"id\" : \"1234\"}";
    Response response = send(HttpMethod.POST, "/perms/users", permsUsers, context);
    context.assertEquals(400, response.code);
  }

  @Test
  public void testPostPermsUsersBadTenant(TestContext context) {
    String permsUsers = "{\"userId\": \""+ userUserId +"\",\"permissions\": " +
        "[], \"id\" : \"1234\"}";
    Response response = send("badTenant", HttpMethod.POST, "/perms/users",
        permsUsers, context);
    context.assertEquals(400, response.code);
  }

  @Test
  public void testPutPermsUsersInvalidUUID(TestContext context) {
    String postPermUsersRequest = "{\"userId\": \""+ userUserId +"\",\"permissions\": " +
        "[], \"id\" : \"" + userId2 + "\"}";
    Response response = send(HttpMethod.PUT, "/perms/users/123", postPermUsersRequest, context);
    context.assertEquals(404, response.code);
    context.assertEquals("No permissions user found with id 123", response.body.getString("text"));
  }

  @Test
  public void testDeletePermsUsersByIdInvalidUUID(TestContext context) {
    Response response = send(HttpMethod.DELETE, "/perms/users/123", null, context);
    context.assertEquals(404, response.code);
    context.assertEquals("No permissions user found with id 123", response.body.getString("text"));
  }

  @Test
  public void testPutPermsUsersByIdDummyPerm(TestContext context) {
    String postPermUsersRequest = "{\"userId\": \""+ userUserId +"\",\"permissions\": " +
        "[], \"id\" : \"" + userId2 + "\"}";
    Response response = send(HttpMethod.POST, "/perms/users", postPermUsersRequest, context);
    context.assertEquals(response.code, 201);

    // adummy.perm not defined so it becomes dummy
    JsonObject permissionSet = new JsonObject()
        .put("moduleId","amodule")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", "adummy.all")
                .put("displayName", "Dummy All")
                .put("description", "Some Permissions")
                .put("subPermissions", new JsonArray()
                    .add("adummy.perm")
                )
            )
        );
    response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(201, response.code);

    String permsUsers = "{\"userId\": \""+userId2+"\",\"permissions\": [\"adummy.perm\"], \"id\" : \"1234\"}";
    response = send(HttpMethod.PUT, "/perms/users/" + userId2, permsUsers, context);
    context.assertEquals(400, response.code);
    context.assertEquals("Cannot add permissions flagged as 'dummy' to users", response.body.getString("text"));

    // adummy.perm becomes non-dummy
    permissionSet = new JsonObject()
        .put("moduleId","bmodule")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", "adummy.perm")
                .put("displayName", "Dummy perm")
                .put("description", "permission")
            )
        );
    response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(201, response.code);

    permsUsers = "{\"userId\": \""+userId2+"\",\"permissions\": [\"adummy.perm\"], \"id\" : \"1234\"}";
    response = send(HttpMethod.PUT, "/perms/users/" + userId2, permsUsers, context);
    context.assertEquals(200, response.code);

    response = send(HttpMethod.DELETE, "/perms/users/" + userId2, postPermUsersRequest, context);
    context.assertEquals(response.code, 204);
  }

  @Test
  public void testPutPermsUsersByIdNonExistingPerm(TestContext context) {
    String postPermUsersRequest = "{\"userId\": \""+ userUserId +"\",\"permissions\": " +
        "[], \"id\" : \"" + userId2 + "\"}";
    Response response = send(HttpMethod.POST, "/perms/users", postPermUsersRequest, context);
    context.assertEquals(response.code, 201);

    String permsUsers = "{\"userId\": \""+userId2+"\",\"permissions\": [\"non.existing\"], \"id\" : \"1234\"}";
    response = send(HttpMethod.PUT, "/perms/users/" + userId2, permsUsers, context);
    context.assertEquals(422, response.code);

    response = send(HttpMethod.DELETE, "/perms/users/" + userId2, postPermUsersRequest, context);
    context.assertEquals(response.code, 204);
  }

  @Test
  public void testPutPermsUsersByIdBadTenant(TestContext context) {
    String permsUsers = "{\"userId\": \"1234\",\"permissions\": [\"foo\"], \"id\" : \"1234\"}";
    Response response = send("badTenant", HttpMethod.PUT, "/perms/users/123", permsUsers, context);
    context.assertEquals(400, response.code);

    String permsUsers2 = "{\"userId\": \"1234\",\"permissions\": [], \"id\" : \"1234\"}";
    response = send("badTenant", HttpMethod.PUT, "/perms/users/123", permsUsers2, context);
    context.assertEquals(400, response.code);
  }

  @Test
  public void testPermsUsersGetCqlError(TestContext context) {
    Response response = send(HttpMethod.GET, "/perms/users?query=a+and", null, context);
    context.assertEquals(400, response.code);
  }

  @Test
  public void testPermsUsersGetBadStart(TestContext context) {
    Response response = send(HttpMethod.GET, "/perms/users?query=permissions%3Ddummy*&start=0",
        null, context);
    context.assertEquals(400, response.code);
  }

  @Test
  public void testPermsPermissionsGetCqlError(TestContext context) {

    Response response = send(HttpMethod.GET, "/perms/permissions?query=a+and",
        null, context);
    context.assertEquals(400, response.code);
  }

  @Test
  public void testPostPermsPermissionsBadTenant(TestContext context) {
    String permRequest = "{\"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send("badTenant", HttpMethod.POST, "/perms/permissions",
        permRequest, context);
    context.assertEquals(response.code, 400);
  }

  @Test
  public void testPostPermsUsersPermissionsByIdBadTenant(TestContext context) {
    String permRequest = "{\"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send("badTenant", HttpMethod.POST, "/perms/users/123/permissions",
        permRequest, context);
    context.assertEquals(response.code, 400);
  }

  @Test
  public void testPostPermsUsersPermissionsByIdBadIndexField(TestContext context) {
    String permRequest = "{\"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send(HttpMethod.POST, "/perms/users/123/permissions?indexField=foo",
        permRequest, context);
    context.assertEquals(response.code, 500);
    context.assertEquals("Invalid value 'foo' for indexField", response.body.getString("text"));
  }

  @Test
  public void testPostPermsUsersPermissionsByIdUnknownUser(TestContext context) {
    String permRequest = "{\"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send(HttpMethod.POST, "/perms/users/123/permissions",
        permRequest, context);
    context.assertEquals(response.code, 400);
    context.assertEquals("User with id 123 does not exist", response.body.getString("text"));
  }

  @Test
  public void testPostPermsUsersPermissionsByIdUnknownPermission(TestContext context) {
    String userId = UUID.randomUUID().toString();
    String userUserId = UUID.randomUUID().toString();
    String postPermUsersRequest = "{\"userId\": \""+ userUserId +"\",\"permissions\": " +
        "[ ], \"id\" : \"" + userId + "\"}";
    Response response = send(HttpMethod.POST, "/perms/users", postPermUsersRequest, context);
    context.assertEquals(response.code, 201);

    String permRequest = "{\"permissionName\":\"abname\",\"displayName\":\"abdisplay\"}";
    response = send(HttpMethod.POST, "/perms/users/" + userId + "/permissions",
        permRequest, context);
    context.assertEquals(response.code, 400);
    context.assertEquals("Permission by name 'abname' does not exist", response.body.getString("text"));

    // adummy.perm not defined so it becomes dummy
    JsonObject permissionSet = new JsonObject()
        .put("moduleId","amodule")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", "adummy.all")
                .put("displayName", "Dummy All")
                .put("description", "Some Permissions")
                .put("subPermissions", new JsonArray()
                    .add("abname")
                )
            )
        );
    response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(201, response.code);

    response = send(HttpMethod.POST, "/perms/users/" + userId + "/permissions",
        permRequest, context);
    context.assertEquals(response.code, 400);
    context.assertEquals("'abname' is flagged as a dummy permission and cannot be assigned to a user",
        response.body.getString("text"));

    response = send(HttpMethod.DELETE, "/perms/users/" + userId, null, context);
    context.assertEquals(response.code, 204);
  }


  @Test
  public void testGetPermsUsersPermissionsByIdBadTenant(TestContext context) {
    Response response = send("badTenant", HttpMethod.GET, "/perms/users/123/permissions",
        null, context);
    context.assertEquals(response.code, 400);
  }

  @Test
  public void testGetPermsUsersPermissionsByIdBadUUID(TestContext context) {
    Response response = send(HttpMethod.GET, "/perms/users/12%2334/permissions",
        null, context);
    context.assertEquals(response.code, 404);
    context.assertEquals("No user found by id 12#34", response.body.getString("text"));
  }

  @Test
  public void testPutPermsPermissionsByIdBadIdValue(TestContext context) {
    String permRequest = "{\"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send(HttpMethod.PUT, "/perms/users/123/permissions",
        permRequest, context);
    context.assertEquals(response.code, 400);
  }

  @Test
  public void testPutPermsPermissionsByIdNotFound(TestContext context) {
    String permRequest = "{\"id\": \"123\", \"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send(HttpMethod.PUT, "/perms/permissions/123",
        permRequest, context);
  }

  @Test
  public void testPutPermsPermissionsByIdBadTenant(TestContext context) {
    String permRequest = "{\"id\": \"123\", \"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send("badTenant", HttpMethod.PUT, "/perms/permissions/123",
        permRequest, context);
    context.assertEquals(response.code, 400);
  }

  @Test
  public void testDeletePermsPermissionsByIdBadTenant(TestContext context) {
    Response response = send("badTenant", HttpMethod.DELETE, "/perms/permissions/123",
        null, context);
    context.assertEquals(response.code, 400);
  }

  @Test
  public void testDeletePermsUsersPermissionsByIdAndPermissionnameBadIndexField(TestContext context) {
    Response response = send(HttpMethod.DELETE, "/perms/users/123/permissions/name?indexField=foo",
        null, context);
    context.assertEquals(response.code, 500);
    context.assertEquals("Invalid value 'foo' for indexField", response.body.getString("text"));
  }

  @Test
  public void testDeletePermsUsersPermissionsByIdAndPermissionnameBadTenant(TestContext context) {
    Response response = send("badTenant", HttpMethod.DELETE, "/perms/users/123/permissions/name",
        null, context);
    context.assertEquals(response.code, 400);
  }

  @Test
  public void testDeletePermsUsersPermissionsByIdAndPermissionnameInvalidUUID(TestContext context) {
    Response response = send(HttpMethod.DELETE, "/perms/users/123/permissions/name",
        null, context);
    context.assertEquals(response.code, 400);
    context.assertEquals("User with id 123 does not exist", response.body.getString("text"));
  }

  @Test
  public void testDeletePermsUsersPermissionsByIdAndPermissionnameMissingPermissionName(TestContext context) {
    /**add a perm user */
    String postPermUsersRequest = "{\"userId\": \""+ userUserId +"\",\"permissions\": " +
        "[], \"id\" : \"" + userId2 + "\"}";
    Response response = send(HttpMethod.POST, "/perms/users", postPermUsersRequest, context);
    context.assertEquals(response.code, 201);

    response = send(HttpMethod.DELETE, "/perms/users/" + userId2 + "/permissions/name",
        null, context);
    context.assertEquals(response.code, 400);
    context.assertEquals("User with id " + userId2 + " does not contain name", response.body.getString("text"));

    response = send(HttpMethod.DELETE, "/perms/users/" + userId2, postPermUsersRequest, context);
    context.assertEquals(response.code, 204);
  }

  @Test
  public void testPostPermsPermissions(TestContext context) {
    String uuid = UUID.randomUUID().toString();
    String permRequest = "{\"id\": \"" + uuid + "\", \"permissionName\":\"adname\",\"displayName\":\"addisplay\"}";

    Response response = send(HttpMethod.POST, "/perms/permissions",
        permRequest, context);
    context.assertEquals(response.code, 201);
    context.assertEquals(uuid, response.body.getString("id")); // MODPERMS-84

    response = send(HttpMethod.DELETE, "/perms/permissions/" + uuid,null, context);
    context.assertEquals(response.code, 204);
  }

  @Test
  public void testGetPermsPermissionsByIdBadTenant(TestContext context) {
    Response response = send("badTenant", HttpMethod.GET, "/perms/permissions/123",
        null, context);
    context.assertEquals(response.code, 400);
  }

  @Test
  public void testPutPermsUsersById(TestContext context) {
    CompletableFuture<Response> futureResponse = new CompletableFuture();

    String userId = UUID.randomUUID().toString();
    String id = UUID.randomUUID().toString();
    String postPermUsersRequest = "{\"userId\": \""+ userId +"\",\"permissions\": [], \"id\" : \"" + id + "\"}";

    Response response = send(HttpMethod.POST, "/perms/users", postPermUsersRequest, context);
    context.assertEquals(response.code, 201);
    JsonObject user = response.body;
    user.remove("metadata");
    context.assertEquals(id, user.getString("id"));

    response = send(HttpMethod.PUT, "/perms/users/" + id, user.encode(), context);
    context.assertEquals(response.code, 200);

    futureResponse = new CompletableFuture();
    user.getJsonArray("permissions").add("permission.second");
    response = send(HttpMethod.PUT, "/perms/users/" + id, user.encode(), context);
    context.assertEquals(response.code, 422);

    response = send(HttpMethod.DELETE, "/perms/users/" + id, null, context);
    context.assertEquals(response.code, 204);
  }

  @Test
  public void testGroup(TestContext context) {
    String postPermRequest = "{\"permissionName\":\"a\",\"displayName\":\"b\"}";

    CompletableFuture<Response> futureResponse;
    Response response;

    /**add a perm */
    response = send(HttpMethod.POST, "/perms/permissions", postPermRequest, context);
    context.assertEquals(response.code, 201, response.body.getString("text"));
    String permUuid = response.body.getString("id");

    /* get it back */
    response = send(HttpMethod.GET, "/perms/permissions", null, context);
    context.assertEquals(response.code, 200);
    context.assertTrue( response.body.getInteger("totalRecords") > 0);

    response = send(HttpMethod.GET, "/perms/permissions?query=permissionName%3Da", null, context);
    context.assertEquals(response.code, 200);
    context.assertEquals(1, response.body.getInteger("totalRecords"));

    /**add a perm again 422 */
    response = send(HttpMethod.POST, "/perms/permissions", postPermRequest, context);
    context.assertEquals(response.code, 422);

    /* add a perm user with a non-existent perm */
    String postBadPermUsersRequest = "{\"userId\": \"93cb7ed4-313e-4f06-bd4b-d44b1308c3f3\",\"permissions\": " +
        "[\"bunny.fufu\"], \"id\" : \"" + userId2 + "\"}";
    response = send(HttpMethod.POST, "/perms/users", postBadPermUsersRequest, context);
    context.assertEquals(response.code, 422);

    /**add a perm user */
    String postPermUsersRequest = "{\"userId\": \""+ userUserId +"\",\"permissions\": " +
        "[], \"id\" : \"" + userId2 + "\"}";
    response = send(HttpMethod.POST, "/perms/users", postPermUsersRequest, context);
    context.assertEquals(response.code, 201);

    /**add a perm user again 422 */
    response = send(HttpMethod.POST, "/perms/users", postPermUsersRequest, context);
    context.assertEquals(response.code, 422);

    /**add a perm  for a user */
    String postPermUserPermRequest = "{\"permissionName\":\"a\"}";
    response = send(HttpMethod.POST, "/perms/users/" + userId2 + "/permissions", postPermUserPermRequest, context);
    context.assertEquals(response.code, 200);

    /**add a perm  for a user again 422 */
    response = send(HttpMethod.POST, "/perms/users/" + userId2 + "/permissions", postPermUserPermRequest, context);
    context.assertEquals(response.code, 422);

    response = send(HttpMethod.DELETE, "/perms/permissions/" + permUuid, null, context);
    context.assertEquals(response.code, 204);

    /* Try to add a new permission with a non-existent sub */
    JsonObject addNewBadPermRequestObject = new JsonObject()
        .put("permissionName", "foo.all#")
        .put("displayName", "foo all")
        .put("description", "All foo permissions")
        .put("subPermissions", new JsonArray().add("foo.whizz"));
    response = send(HttpMethod.POST, "/perms/permissions", addNewBadPermRequestObject.encode(), context);
    context.assertEquals(response.code, 422);

    /* Add a new permission */
    String newPermId = null;
    String newPermId2 = null;
    JsonObject addNewPermRequestObject = new JsonObject()
        .put("permissionName", "foo.all#")
        .put("displayName", "foo all")
        .put("description", "All foo permissions");
    response = send(HttpMethod.POST, "/perms/permissions", addNewPermRequestObject.encode(), context);
    context.assertEquals(response.code, 201);
    newPermId = response.body.getString("id");

    /* Attempt to add the same permission */
    response = send(HttpMethod.POST, "/perms/permissions", addNewPermRequestObject.encode(), context);
    context.assertEquals(response.code, 422);

    /* Attempt to modify the permission with a non-existent subpermission */
    JsonObject modifyNewPermRequestObject = new JsonObject()
        .put("permissionName", "foo.all#")
        .put("displayName", "foo all")
        .put("description", "All foo permissions")
        .put("id", newPermId)
        .put("subPermissions", new JsonArray().add("foo.whizz"));
    response = send(HttpMethod.PUT, "/perms/permissions/" + newPermId, modifyNewPermRequestObject.encode(), context);
    context.assertEquals(response.code, 422);

    /* Add a second permission */
    JsonObject addSecondPermRequestObject = new JsonObject()
        .put("permissionName", "foo.whizz")
        .put("displayName", "foo whizz")
        .put("description", "Whizz a foo");
    response = send(HttpMethod.POST, "/perms/permissions", addSecondPermRequestObject.encode(), context);
    context.assertEquals(response.code, 201);
    newPermId2 = response.body.getString("id");

    /* Modify the first permission to make the second a subpermission */
    JsonObject modifyNewPermRequestObject1 = new JsonObject()
        .put("permissionName", "foo.all#")
        .put("displayName", "foo all")
        .put("description", "All foo permissions")
        .put("id", newPermId)
        .put("subPermissions", new JsonArray().add("foo.whizz"));
    response = send(HttpMethod.PUT, "/perms/permissions/" + newPermId, modifyNewPermRequestObject1.encode(), context);
    context.assertEquals(response.code, 200);

    /* Get the first permission, check for subpermission */
    response = send(HttpMethod.GET, "/perms/permissions/" + newPermId, null, context);
    context.assertEquals(response.code, 200);
    context.assertNotNull(response.body.getJsonArray("subPermissions"));
    context.assertTrue(response.body.getJsonArray("subPermissions").contains("foo.whizz"));

    /* Get the second permission, check for childOf */
    response = send(HttpMethod.GET, "/perms/permissions/" + newPermId2, null, context);
    context.assertEquals(response.code, 200);
    context.assertTrue(response.body.getString("permissionName").equals("foo.whizz"));
    context.assertNotNull(response.body.getJsonArray("childOf"));
    context.assertFalse(response.body.getJsonArray("childOf").isEmpty());
    context.assertTrue(response.body.getJsonArray("childOf").contains("foo.all#"));

    /*Retrieve all the permissions */
    response = send(HttpMethod.GET, "/perms/permissions", null, context);
    context.assertEquals(response.code, 200);
    context.assertNotNull(response.body.getJsonArray("permissions"));
    context.assertFalse(response.body.getJsonArray("permissions").isEmpty());
    context.assertTrue(response.body.getInteger("totalRecords") > 1);

    /* Add a new user */
    String newUserUserId = "626a7a5c-4b66-4d2f-981f-1df9757e2aa9";
    JsonObject addNewUserObject = new JsonObject()
        //.put("userId", "5a94d5bd-f76b-4af6-bfe9-497e80094114")
        .put("userId", newUserUserId)
        .put("permissions", new JsonArray());
    response = send(HttpMethod.POST, "/perms/users", addNewUserObject.encode(), context);
    context.assertEquals(response.code, 201);
    String newUserId = response.body.getString("id");
    context.assertNotNull(newUserId);

    /* Attempt to add the same user */
    response = send(HttpMethod.POST, "/perms/users", addNewUserObject.encode(), context);
    context.assertEquals(response.code, 422);
    /* Add the permission to the user */

    JsonObject addPermToUserObject = new JsonObject()
        .put("permissionName", "foo.all#");
    response = send(HttpMethod.POST, "/perms/users/" + newUserId + "/permissions", addPermToUserObject.encode(), context);
    context.assertEquals(response.code, 200);

    /*Get the permission, check for grantedTo    */
    response = send(HttpMethod.GET, "/perms/permissions/" + newPermId, null, context);
    context.assertEquals(response.code, 200);
    context.assertNotNull(response.body.getJsonArray("grantedTo"));
    context.assertTrue(response.body.getJsonArray("grantedTo")
        .contains(newUserId));

    /* Get a list of permissions that the user has */
    response = send(HttpMethod.GET, "/perms/users/" + newUserId + "/permissions", null, context);
    context.assertEquals(response.code, 200);
    context.assertNotNull(response.body.getJsonArray("permissionNames"));
    context.assertTrue(response.body.getJsonArray("permissionNames").contains("foo.all#"));

    /* Get a list of permissions the user has with full subpermissions */
    response = send(HttpMethod.GET, "/perms/users/" + newUserId + "/permissions?full=true", null, context);
    context.assertEquals(response.code, 200);
    context.assertNotNull(response.body.getJsonArray("permissionNames"));
    context.assertNotNull(response.body.getJsonArray("permissionNames").getJsonObject(0));
    context.assertNotNull(response.body.getJsonArray("permissionNames")
        .getJsonObject(0).getJsonArray("subPermissions").contains("foo.whizz"));

    /* Get a list of permissions the user has with expanded subpermissions */
    response = send(HttpMethod.GET, "/perms/users/" + newUserId + "/permissions?expanded=true", null, context);
    context.assertEquals(response.code, 200);
    context.assertNotNull(response.body.getJsonArray("permissionNames"));
    context.assertTrue(response.body.getJsonArray("permissionNames").contains("foo.all#"));
    context.assertTrue(response.body.getJsonArray("permissionNames").contains("foo.whizz"));

    /* Get a list of full and expanded permissions the user has */
    response = send(HttpMethod.GET, "/perms/users/" + newUserId + "/permissions?expanded=true&full=true", null, context);
    context.assertEquals(response.code, 200);
    context.assertNotNull(response.body.getJsonArray("permissionNames"));
    try {
      boolean allFound = false;
      boolean whizzFound = false;
      JsonArray perms = response.body.getJsonArray("permissionNames");
      for(Object ob : perms) {
        JsonObject perm = (JsonObject)ob;
        if(perm.getString("permissionName").equals("foo.all#")) {
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
        context.fail("Did not locate permission for 'foo.all#'");
      } else if(!whizzFound) {
        context.fail("Did not locate permission for 'foo.whizz'");
      }
    } catch(Exception e) {
      context.fail(e);
    }

    /* Delete the child permission */
    response = send(HttpMethod.DELETE, "/perms/permissions/" + newPermId2, null, context);
    context.assertEquals(response.code, 204);

    /* Get the original permission, check that child is not in subpermissions */
    response = send(HttpMethod.GET, "/perms/permissions/" + newPermId, null, context);
    context.assertEquals(response.code, 200);
    context.assertFalse(response.body.getJsonArray("subPermissions").contains("foo.whizz"));

    /* Delete the permission from the user */
    response = send(HttpMethod.DELETE, "/perms/users/" + newUserId + "/permissions/foo.all%23", null, context);
    context.assertEquals(response.code, 204);

    /* Get the original permission, check that our user is no longer in grantedTo */
    response = send(HttpMethod.GET, "/perms/permissions/" + newPermId, null, context);
    context.assertEquals(response.code, 200);
    context.assertFalse(response.body.getJsonArray("grantedTo")
        .contains(newUserId));

    /* Get the user's permissions, make sure the permission is not present */
    response = send(HttpMethod.GET, "/perms/users/" + newUserId + "/permissions", null, context);
    context.assertEquals(response.code, 200);
    context.assertNotNull(response.body.getJsonArray("permissionNames"));
    context.assertFalse(response.body.getJsonArray("permissionNames").contains("foo.all#"));


    /* Add another new permission */
    JsonObject addAnotherNewPermRequestObject = new JsonObject()
        .put("permissionName", "moo/all")
        .put("displayName", "moo all")
        .put("description", "All moo permissions");
    response = send(HttpMethod.POST, "/perms/permissions", addAnotherNewPermRequestObject.encode(), context);
    context.assertEquals(response.code, 201);
    String anotherNewPermId = response.body.getString("id");

    /* Add the new permission to the user via the user's userId */
    JsonObject addAnotherPermToUserObject = new JsonObject()
        .put("permissionName", "moo/all");
    response = send(HttpMethod.POST, "/perms/users/" + newUserUserId + "/permissions?indexField=userId",
        addAnotherPermToUserObject.encode(), context);
    context.assertEquals(response.code, 200);

    /* check for presence of permission */
    response = send(HttpMethod.GET, "/perms/users/" + newUserId + "/permissions", null, context);
    context.assertEquals(response.code, 200);
    context.assertNotNull(response.body.getJsonArray("permissionNames"));
    context.assertTrue(response.body.getJsonArray("permissionNames")
        .contains("moo/all"));

    /* Delete the new permission from the user, via userId */
    response = send(HttpMethod.DELETE, "/perms/users/" + newUserUserId + "/permissions/moo%2Fall?indexField=userId",
        null, context);
    context.assertEquals(response.code, 204);

    /* check for non-presence of permission */
    response = send(HttpMethod.GET, "/perms/users/" + newUserId + "/permissions", null, context);
    context.assertEquals(response.code, 200);
    context.assertNotNull(response.body.getJsonArray("permissionNames"));
    context.assertFalse(response.body.getJsonArray("permissionNames")
        .contains("moo.all"));

    /* Delete the user */
    response = send(HttpMethod.DELETE, "/perms/users/" + newUserId, null, context);
    context.assertEquals(response.code, 204);

    /* Attempt to retrieve the user */
    response = send(HttpMethod.GET, "/perms/users/" + newUserId, null, context);
    context.assertEquals(response.code, 404);

    response = send(HttpMethod.DELETE, "/perms/users/" + newUserId, null, context);
    context.assertEquals(response.code, 404);

    /* Delete the permission */
    response = send(HttpMethod.DELETE, "/perms/permissions/" + newPermId, null, context);
    context.assertEquals(response.code, 204);

    /* Attempt to retrieve the permission */
    response = send(HttpMethod.GET, "/perms/permissions/" + newPermId, null, context);
    context.assertEquals(response.code, 404);

    response = send(HttpMethod.DELETE, "/perms/permissions/" + newPermId, null, context);
    context.assertEquals(response.code, 404);

    /*Delete a permission that's not there */
    response = send(HttpMethod.DELETE, "/perms/permissions/ed145a0a-c4ff-46b3-8c44-62d89f32afea", null, context);
    context.assertEquals(response.code, 404);

    response = send(HttpMethod.DELETE, "/perms/users/" + userId2, postPermUsersRequest, context);
    context.assertEquals(response.code, 204);
  }

  private Response send(HttpMethod method, String path, String content, TestContext context) {
    return send("diku", method, path, content, context);
  }

  private Response send(String tenant, HttpMethod method, String path, String content, TestContext context) {
    try {
      CompletableFuture<Response> futureResponse = new CompletableFuture();
      send(tenant, "http://localhost:" + port + path, context, method, content,
          CONTENT_TYPE_JSON, new HTTPResponseHandler(futureResponse));
      return futureResponse.get(5, TimeUnit.SECONDS);
    } catch (Exception e) {
      context.fail(e);
      return null;
    }
  }

  private void send(String tenant, String url, TestContext context, HttpMethod method, String content,
                    String contentType, Handler<HttpClientResponse> handler) {
    HttpClientRequest request = client.requestAbs(method, url);
    request.exceptionHandler(error -> context.fail(error.getMessage())).handler(handler);
    request.putHeader("x-okapi-tenant", tenant);
    request.putHeader("Accept", CONTENT_TYPE_TEXT_JSON);
    request.putHeader("Content-type", contentType);
    if (content == null) {
      request.end();
    } else {
      request.end(content);
    }
    logger.debug("Sending " + method.toString() + " request to " +
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
            if (CONTENT_TYPE_JSON.equals(hcr.getHeader("Content-Type"))) {
              r.body = bh.toJsonObject();
            } else if (CONTENT_TYPE_TEXT.equals(hcr.getHeader("Content-Type"))) {
              r.body = new JsonObject().put("text", bh.toString());
            } else {
              r.body = null;
            }
          } catch (Exception e) {
            logger.warn("Warning: '" + bh.toString() + "' cannot be parsed as JSON");
            r.body = new JsonObject(); //Or should it be null?
          }
          logger.debug("Got code '" + hcr.statusCode() + "' and body '" +
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

    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("accept", CONTENT_TYPE_TEXT_JSON);
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
        HttpMethod.POST, headers, permissionSet.encode(), 201).onComplete(res -> {
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
    testPermissionExists(context, "dummy.all", true).onComplete( testRes -> {
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
    testPermissionExists(context, "dummy.delete", true).onComplete( testRes -> {
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

    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("accept", CONTENT_TYPE_TEXT_JSON);
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
        HttpMethod.POST, headers, permissionSet.encode(), 201).onComplete(res -> {
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

    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("accept", CONTENT_TYPE_TEXT_JSON);
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
        HttpMethod.POST, headers, permissionSet.encode(), 201).onComplete(res -> {
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
    testPermissionExists(context, "bad.delete", true).onComplete( testRes -> {
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

    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("accept", CONTENT_TYPE_TEXT_JSON);
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
        HttpMethod.POST, headers, permissionSet.encode(), 201).onComplete(res -> {
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

    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("accept", "application/json,text/plain");
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
        HttpMethod.POST, headers, permissionSet.encode(), 201).onComplete(res -> {
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
    testPermissionExists(context, "alien.woo").onComplete( testRes -> {
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
        HttpMethod.POST, null, newUser.encode(), 201).onComplete(res -> {
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
        HttpMethod.PUT, null, modifiedUser.encode(), 404).onComplete(res -> {
      if(res.failed()) {
        future.fail(res.cause());
      } else {
        future.complete(res.result());
      }
    });
    return future;
  }

  private Future<WrappedResponse> testUserPerms(TestContext context, String permsUserId) {
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
    Future<WrappedResponse> future = Future.future();
    TestUtil.doRequest(vertx, "http://localhost:"+port+"/perms/users/" + permsUserId +
        "/permissions?expanded=true", HttpMethod.GET, headers, null, 200).onComplete(res -> {
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
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    String url;
    try {
      headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
      url = "http://localhost:" + port + "/perms/users?query=" +
          URLEncoder.encode("permissions=dummy*", "UTF-8");
    } catch(Exception e) {
      future.fail(e);
      return future;
    }
    TestUtil.doRequest(vertx, url, HttpMethod.GET, headers, null, 200).onComplete(res -> {
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
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    String url;
    try {
      headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
      url = "http://localhost:"+port+ "/perms/permissions?query=" + URLEncoder.encode("permissionName=dummy*", "UTF-8");
    } catch(Exception e) {
      future.fail(e);
      return future;
    }
    TestUtil.doRequest(vertx, url, HttpMethod.GET, headers, null, 200).onComplete(res -> {
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
    TestUtil.doRequest(vertx, "http://localhost:"+port+"/perms/permissions", HttpMethod.POST,
        null, badPermission.encode(), 400).onComplete(res -> {
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
    TestUtil.doRequest(vertx, "http://localhost:"+port+"/perms/permissions", HttpMethod.POST,
        null, nullPermission.encode(), 201).onComplete(res -> {
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
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
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
    TestUtil.doRequest(vertx, url, HttpMethod.POST, null, newUser.encode(), 201).onComplete(res -> {
      if(res.failed()) { future.fail(res.cause()); } else {
        TestUtil.doRequest(vertx, url2, HttpMethod.GET, headers, null, 200 ).onComplete(res2 -> {
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
    TestUtil.doRequest(vertx, url, HttpMethod.GET, null, null, 200).onComplete(res -> {
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
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    String fakeUserId = UUID.randomUUID().toString();
    headers.add("X-Okapi-Token", makeFakeJWT("mcdonald", fakeUserId, "diku"));
    headers.add("X-Okapi-User-Id", fakeUserId);
    TestUtil.doRequest(vertx, url, HttpMethod.POST, headers, newUser.encode(), 201).onComplete(
        res -> {
          if(res.failed()) { future.fail(res.cause()); } else {
            try {
              String newUserId = res.result().getJson().getString("id");
              String url2 = String.format("http://localhost:%s/perms/users/%s", port,
                  newUserId);
              TestUtil.doRequest(vertx, url2, HttpMethod.GET, null, null, 200).onComplete(res2 -> {
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
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    String fakeUserId = UUID.randomUUID().toString();
    headers.add("X-Okapi-Token", makeFakeJWT("mcdonald", fakeUserId, "diku"));
    headers.add("X-Okapi-User-Id", fakeUserId);
    TestUtil.doRequest(vertx, url, HttpMethod.POST, headers, newPerm.encode(), 201).onComplete(
        res -> {
          if(res.failed()) { future.fail(res.cause()); } else {
            try {
              String newPermId = res.result().getJson().getString("id");
              String url2 = String.format("http://localhost:%s/perms/permissions/%s", port,
                  newPermId);
              TestUtil.doRequest(vertx, url2, HttpMethod.GET, null, null, 200).onComplete(res2 -> {
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

  private Future<WrappedResponse> sendNestedSubPerms(TestContext context) {
    Promise<WrappedResponse> promise = Promise.promise();
    JsonObject permissionSet = new JsonObject()
        .put("moduleId","dummy")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", "test.a")
                .put("subPermissions", new JsonArray()
                    .add("test.b")
                )
            )
            .add(new JsonObject()
                .put("permissionName", "test.b")
                .put("subPermissions", new JsonArray()
                    .add("test.c")
                )
            )
            .add(new JsonObject()
                .put("permissionName", "test.c")
                .put("subPermissions", new JsonArray()
                    .add("test.d")
                )
            )
        );
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("accept", "application/json,text/plain");
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
        HttpMethod.POST, headers, permissionSet.encode(), 201).onComplete(res -> {
      if(res.failed()) {
        promise.fail(res.cause());
      } else {
        promise.complete(res.result());
      }
    });
    return promise.future();
  }

  private Future<WrappedResponse> testNestedSubPermExpansion(TestContext context) {
    Set<String> perms = new HashSet<>(Arrays.asList("test.b", "test.c", "test.d"));
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("X-Okapi-Permissions", new JsonArray().add("perms.permissions.get").encode());
    Promise<WrappedResponse> promise = Promise.promise();
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/permissions?expanded=true&query=(permissionName==test.a)",
        HttpMethod.GET, headers, null, 200).onComplete(res -> {
      try {
        if (res.failed()) {
          promise.fail(res.cause());
        } else {
          JsonArray subPermList = res.result().getJson().getJsonArray("permissions").getJsonObject(0).getJsonArray("subPermissions");
          Set<String> set = new HashSet<>();
          subPermList.forEach(subPerm -> set.add(subPerm.toString()));
          if (set.containsAll(perms) && set.size() == perms.size()) {
            promise.complete(res.result());
          } else {
            promise.fail("SubPermList does not match " + perms + " ( "
                + res.result().getBody() + " )");
          }
        }
      } catch (Exception e) {
        promise.fail(e);
      }
    });

    return promise.future();
  }

  private Future<WrappedResponse> sendNestedSubPermsWithException(TestContext context) {
    Promise<WrappedResponse> promise = Promise.promise();
    JsonObject permissionSet = new JsonObject()
        .put("moduleId","dummy")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", "test.aa")
                .put("subPermissions", new JsonArray()
                    .add(PermsCache.TEST_EXCEPTION_PERMISSION)
                )
            )
        );
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("accept", "application/json,text/plain");
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
        HttpMethod.POST, headers, permissionSet.encode(), 201).onComplete(res -> {
      if(res.failed()) {
        promise.fail(res.cause());
      } else {
        promise.complete(res.result());
      }
    });
    return promise.future();
  }

  private Future<WrappedResponse> testNestedSubPermExpansionWithExceptions(TestContext context) {
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("X-Okapi-Permissions", new JsonArray().add("perms.permissions.get").encode());
    Promise<WrappedResponse> promise = Promise.promise();
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/permissions?expanded=true&query=(permissionName==test.aa)",
        HttpMethod.GET, headers, null, 200).onComplete(res -> {
      if (res.failed()) {
        promise.complete();
      } else {
        promise.fail("Should throw exception due to use of special exception: " + PermsCache.TEST_EXCEPTION_PERMISSION);
      }
    });
    return promise.future();
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
    logger.debug("Generated fake JWT: " + ret);
    return ret;

  }
}

