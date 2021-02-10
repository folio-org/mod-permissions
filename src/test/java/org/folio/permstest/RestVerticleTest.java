package org.folio.permstest;

import static org.folio.permstest.TestUtil.CONTENT_TYPE_JSON;
import static org.folio.permstest.TestUtil.CONTENT_TYPE_TEXT;
import static org.folio.permstest.TestUtil.CONTENT_TYPE_TEXT_JSON;
import static org.hamcrest.CoreMatchers.containsString;


import io.vertx.core.buffer.Buffer;
import io.vertx.ext.web.client.WebClient;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.permstest.TestUtil.WrappedResponse;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.impl.PermsCache;
import org.folio.rest.impl.TenantPermsAPI;
import org.folio.rest.jaxrs.model.OkapiPermission;
import org.folio.rest.jaxrs.model.OkapiPermissionSet;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;
import io.vertx.core.CompositeFuture;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.MultiMap;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpClientResponse;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.client.HttpRequest;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;

@RunWith(VertxUnitRunner.class)
public class RestVerticleTest {
  private static final Logger logger = LogManager.getLogger(RestVerticleTest.class);

  private static final String userId1 = "35d05a6a-d61e-4e81-9708-fc44daadbec5";
  private static final String userId2 = "176bc0cc-b785-4cf9-9e8a-5fafe8178332";
  private static final String userId3 = "f36400e5-ec5e-4e6c-abac-25fc42e1ec47";

  private static final String permUserId1 = UUID.randomUUID().toString();

  private static final String userDefinedPermId = UUID.randomUUID().toString();

  private static final String userUserId = "93cb7ed4-313e-4f06-bd4b-d44b1308c3f3";
  private static Vertx vertx;
  private static WebClient client;
  static int port;

  @Rule
  public Timeout rule = Timeout.seconds(240);  // 4 (?!) minutes for loading embedded postgres

  @BeforeClass
  public static void setup(TestContext context) {
    port = NetworkUtils.nextFreePort();
    TenantClient tenantClient = new TenantClient("http://localhost:" + port, "diku",  null);
    vertx = Vertx.vertx();
    client = WebClient.create(vertx);
    DeploymentOptions options = new DeploymentOptions().setConfig(new JsonObject()
        .put("http.port", port).put(PermsCache.CACHE_HEADER, false)).setWorker(false);

    vertx.deployVerticle(RestVerticle.class.getName(), options)
        .onComplete(
            context.asyncAssertSuccess(res -> {
              TenantAttributes ta = new TenantAttributes();
              ta.setModuleTo("mod-permissions-1.0.0");
              List<Parameter> parameters = new LinkedList<>();
              parameters.add(new Parameter().withKey("loadSample").withValue("true"));
              ta.setParameters(parameters);
              TestUtil.tenantOp(tenantClient, ta)
                  .onComplete(context.asyncAssertSuccess());
            }));
  }

  @AfterClass
  public static void teardown(TestContext context) {
    Async async = context.async();
    client.close();
    PostgresClient.stopEmbeddedPostgres();
    vertx.close(context.asyncAssertSuccess( res-> {
      async.complete();
    }));
  }

  /*
          Call our various tests for the permissions module, but do so in a sequential fashion,
          chaning each future's completion to the next in line
  */
  @Test
  public void testPermsSeq(TestContext context) {
    Future<WrappedResponse> startFuture;
    startFuture = sendInitialPermissionSet(context).compose(w -> {
      return sendPermissionSetWithCollision(context); // fail due to module-defined perm collision
    }).compose(w -> {
      return testPostPermission(context); // add user-defined perm to cause collision later
    }).compose(w -> {
      return removeModuleContext(context, new String[] {"dummy.all", "dummy.write",
          "dummy.read", "dummy.collection.get", "dummy.collection.item.get", "dummy.update",
          "dummy.delete"});
    }).compose(w -> {
      return sendInitialPermissionSet(context); // simulate perm refresh
    }).compose(w -> {
      return testModuleContextWasAdded(context, new String[] {"dummy.all", "dummy.write",
          "dummy.read", "dummy.collection.get", "dummy.collection.item.get", "dummy.update"});
    }).compose(w -> {
      return postPermUser(context, userId1, permUserId1,
          new String[] {"dummy.all", "dummy.collection.get", "dummy.collection.item.get"});
    }).compose(w -> {
      return testUserPerms(context, permUserId1, new String[] {"dummy.all", "dummy.write",
          "dummy.read", "dummy.collection.get", "dummy.collection.item.get", "dummy.update"});
    }).compose(w -> {
      return testPermissionExists(context, "dummy.collection.get");
    }).compose(w -> {
      return testGrantedTo(context, w, permUserId1);
    }).compose(w -> {
      return sendUpdatedPermissionSet(context, true); // fail upgrade due to perm collision
    }).compose(w -> {
      return resolvePermNameConflict(context); // remove the user-defined perm dummy.delete
    }).compose(w -> {
      return sendUpdatedPermissionSet(context, false); // simulate upgrade
    }).compose(w -> {
      return testSoftDeleteOfRemovedPermission(context, "dummy.update");
    }).compose(w -> {
      return testPermissionRename(context);
    }).compose(w -> {
      return testUserPerms(context, permUserId1, new String[] {"dummy.all", "dummy.write",
          "dummy.read", "dummy.collection.read", "dummy.delete"});
    }).compose(w -> {
      return testPermissionExists(context, "dummy.collection.read");
    }).compose(w -> {
      return testGrantedTo(context, w, permUserId1);
    }).compose(w -> {
      return testUpdateParentPermission(context);
    }).compose(w -> {
      return testUpdateChildPermission(context);
    }).compose(w -> {
      return postPermUser(context, userId2, new String[] {"dummy.all"});
    }).compose(w -> {
      return testUserPerms(context, w.getJson().getString("id"),
          new String[] {"dummy.write", "dummy.read"});
    }).compose(w -> {
      return postPermUser(context, userId3, new String[] {"dummy.all"});
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
    }).compose(w -> {
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
    }).compose(w -> {
      return sendInitialPermissionSet(context); // simulate downgrade
    }).compose(w -> {
      return testDowngradeRestoredPermission(context);
    }).compose(w -> {
      return testPermissionExists(context, "dummy.collection.get");
    }).compose(w -> {
      return testGrantedTo(context, w, permUserId1);
    }).compose(w -> {
      return testUserPerms(context, permUserId1, new String[] {"dummy.all", "dummy.write",
          "dummy.read", "dummy.collection.get", "dummy.update"});
    }).compose(w -> {
      return testSoftDeleteOfRemovedPermission(context, "dummy.collection.read");
    }).compose(w -> {
      return testSoftDeleteOfRemovedPermission(context, "dummy.delete");
    });

    startFuture.onComplete(context.asyncAssertSuccess());
  }

  @Test
  public void testGetPermsUsersByIdBadIndexField(TestContext context) {

    Response response = send(HttpMethod.GET, "/perms/users/123?indexField=bad", null, context);
    context.assertEquals(500, response.code);
  }

  @Test
  public void testGetPermsUsersByIdBadUUID(TestContext context) {
    Response response = send(HttpMethod.GET, "/perms/users/12%2334?indexField=id", null, context);
    context.assertEquals(400, response.code);
    Assert.assertThat(response.body.getString("text"),
    containsString("invalid input syntax for type uuid: \\\"12#34\\\""));
  }

  @Test
  public void testGetPermsUsersByIdDoesNotExist(TestContext context) {
    String uuid = UUID.randomUUID().toString();
    Response response = send(HttpMethod.GET, "/perms/users/" + uuid + "?indexField=id", null, context);
    context.assertEquals(404, response.code);
    context.assertEquals("No user with id: " + uuid, response.body.getString("text"));
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
    context.assertEquals(400, response.code);
    Assert.assertThat(response.body.getString("text"),
        containsString("invalid input syntax for type uuid"));
  }

  @Test
  public void testDeletePermsUsersByIdDoesNotExist(TestContext context) {
    String uuid = UUID.randomUUID().toString();
    Response response = send(HttpMethod.DELETE, "/perms/users/" + uuid, null, context);
    context.assertEquals(404, response.code);
    context.assertEquals("No permissions user found with id " + uuid, response.body.getString("text"));
  }

  @Test
  public void testTenantPermissionsNullPermList(TestContext context) {
    Response response = send(HttpMethod.POST, "/_/tenantpermissions", "{\"moduleId\":\"module\",\"perms\":null}", context);
    context.assertEquals(201, response.code);
  }

  // Test that a permission can be used in another module if first one is deleted MODPERMS-130
  // This may happen if a module is deprecated and simply renamed, but keep providing the same interfaces
  // and permissions
  // Or there really are two modules out there offering the same interface and permissions (and only
  // one can be enabled at a time).
  @Test
  public void testPermissionsOnTheMove(TestContext context) {
    String perm = "perm" + UUID.randomUUID().toString();
    JsonObject permissionSet = new JsonObject()
        .put("moduleId","moduleA0")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", perm)
                .put("displayName", "Description A")
                )
            );
    Response response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(201, response.code);

    // remove permissions for it / OKAPI-982
    permissionSet = new JsonObject()
        .put("moduleId","moduleA0")
        .put("perms", new JsonArray());
    response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(201, response.code);

    // use same permission again in other module with new definition
    permissionSet = new JsonObject()
        .put("moduleId","moduleB0")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", perm)
                .put("displayName", "Description B")
            )
        );
    response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(201, response.code);
  }

  @Test
  public void testPermissionsChangingInMultipleModules(TestContext context) {
    String perm = "perm" + UUID.randomUUID().toString();
    JsonObject permissionSet = new JsonObject()
        .put("moduleId","moduleA1-1.0.0")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", perm)
                .put("displayName", "Description 1")
            )
        );
    Response response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(201, response.code);

    // use same permission in other module with same definition
    permissionSet = new JsonObject()
        .put("moduleId","moduleB1-1.0.0")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", perm)
                .put("displayName", "Description 1")
            )
        );
    response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    // fails because we can only have one module offering a permission
    context.assertEquals(400, response.code);
  }

  @Test
  public void testSameModuleCanChangeDefinition(TestContext context) {
    String perm = "perm" + UUID.randomUUID().toString();
    JsonObject permissionSet = new JsonObject()
        .put("moduleId","moduleA2-1.0.0")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", perm)
                .put("displayName", "Description 1")
            )
        );
    Response response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(201, response.code);

    // update the module with an updated permission definition
    permissionSet = new JsonObject()
        .put("moduleId","moduleA2-1.0.1")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", perm)
                .put("displayName", "Description 2")
            )
        );
    response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(201, response.code);
  }

  @Test
  public void testOtherModuleCanNotChangeDefinition(TestContext context) {
    String perm = "perm" + UUID.randomUUID().toString();
    JsonObject permissionSet = new JsonObject()
        .put("moduleId","moduleA3-1.0.0")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", perm)
                .put("displayName", "Description 1")
            )
        );
    Response response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(201, response.code);

    // use same permission in other module with same definition
    permissionSet = new JsonObject()
        .put("moduleId","moduleB3-1.0.0")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", perm)
                .put("displayName", "Description 1")
            )
        );
    response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(400, response.code);
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

    response = send(HttpMethod.GET, "/perms/permissions?query=permissionName%3Dadummy.perm&includeDummy=true", null, context);
    context.assertEquals(200, response.code);
    context.assertEquals(1, response.body.getInteger("totalRecords"));

    response = send(HttpMethod.GET, "/perms/permissions?query=permissionName%3Dadummy.perm&includeDummy=xx", null, context);
    context.assertEquals(200, response.code);
    context.assertEquals(0, response.body.getInteger("totalRecords"));

    response = send(HttpMethod.GET, "/perms/permissions?query=permissionName%3Dadummy.perm", null, context);
    context.assertEquals(200, response.code);
    context.assertEquals(0, response.body.getInteger("totalRecords"));

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
  public void testPostPermsUsersPermissionsByIdBadUUID(TestContext context) {
    String permRequest = "{\"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send(HttpMethod.POST, "/perms/users/123/permissions",
        permRequest, context);
    context.assertEquals(response.code, 400);
    Assert.assertThat(response.body.getString("text"), containsString("invalid input syntax for type uuid"));
  }

  @Test
  public void testPostPermsUsersPermissionsByIdUnknownUser(TestContext context) {
    String uuid = UUID.randomUUID().toString();
    String permRequest = "{\"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send(HttpMethod.POST, "/perms/users/" + uuid + "/permissions",
        permRequest, context);
    context.assertEquals(response.code, 400);
    context.assertEquals("User with id " + uuid + " does not exist", response.body.getString("text"));
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
    context.assertEquals(response.code, 400);
    Assert.assertThat(response.body.getString("text"),
        containsString("invalid input syntax for type uuid: \\\"12#34\\\""));
  }

  @Test
  public void testGetPermsUsersPermissionsByIdDoesNotExist(TestContext context) {
    String uuid = UUID.randomUUID().toString();
    Response response = send(HttpMethod.GET, "/perms/users/" + uuid + "/permissions",
        null, context);
    context.assertEquals(response.code, 404);
    context.assertEquals("No user found by id " + uuid, response.body.getString("text"));
  }

  @Test
  public void testPutPermsPermissionsByIdBadIdValue1(TestContext context) {
    String permRequest = "{\"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send(HttpMethod.PUT, "/perms/permissions/123",
        permRequest, context);
    context.assertEquals(response.code, 400);
    context.assertEquals("Invalid id value", response.body.getString("text"));
  }

  @Test
  public void testPutPermsPermissionsByIdBadIdValue2(TestContext context) {
    String permRequest = "{\"permissionName\":\"aaname\",\"displayName\":\"aadisplay\", \"id\":\"1\"}";
    Response response = send(HttpMethod.PUT, "/perms/permissions/123",
        permRequest, context);
    context.assertEquals(response.code, 400);
    context.assertEquals("Invalid id value", response.body.getString("text"));
  }

  @Test
  public void testPutPermsPermissionsByIdBadUUID(TestContext context) {
    String permRequest = "{\"id\": \"123\", \"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send(HttpMethod.PUT, "/perms/permissions/123",
        permRequest, context);
    context.assertEquals(response.code, 400);
    Assert.assertThat(response.body.getString("text"), containsString("invalid input syntax for type uuid"));
  }

  @Test
  public void testPutPermsPermissionsByIdNotFound(TestContext context) {
    String uuid = UUID.randomUUID().toString();
    String permRequest = "{\"id\": \"" + uuid + "\", \"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send(HttpMethod.PUT, "/perms/permissions/" + uuid,
        permRequest, context);
    context.assertEquals(response.code, 404);
    context.assertEquals("No permission found to match that id", response.body.getString("text"));
  }

  @Test
  public void testPutPermsPermissionsByIdBadTenant(TestContext context) {
    String permRequest = "{\"id\": \"123\", \"permissionName\":\"aaname\",\"displayName\":\"aadisplay\"}";
    Response response = send("badTenant", HttpMethod.PUT, "/perms/permissions/123",
        permRequest, context);
    context.assertEquals(response.code, 400);
  }

  @Test
  public void testPutPermsPermissionsByIdCannotModify(TestContext context) {
    String permName = "perm-" + UUID.randomUUID().toString();
    JsonObject o = new JsonObject().put("permissionName", permName);
    Response response = send(HttpMethod.POST, "/perms/permissions", o.encode(), context);
    context.assertEquals(response.code, 201);
    String id = response.body.getString("id");

    o = new JsonObject().put("id", id).put("permissionName", "otherName");
    response = send(HttpMethod.PUT, "/perms/permissions/" + id, o.encode(), context);
    context.assertEquals(response.code, 400);
    context.assertEquals("permission name property cannot change", response.body.getString("text"));
  }

  @Test
  public void testPutPermsPermissionsByIdDummy(TestContext context) {
    String normalPerm = "normal-" + UUID.randomUUID().toString();
    String dummyPerm = "dummy-" + UUID.randomUUID().toString();

    JsonObject permissionSet = new JsonObject()
        .put("moduleId", "amodule")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", normalPerm)
                .put("subPermissions", new JsonArray()
                    .add(dummyPerm)
                )
            )
        );
    Response response = send(HttpMethod.POST, "/_/tenantpermissions", permissionSet.encode(), context);
    context.assertEquals(201, response.code);

    response = send(HttpMethod.GET, "/perms/permissions?includeDummy=true&query=permissionName=" + dummyPerm,
        null, context);
    context.assertEquals(200, response.code);
    context.assertEquals(1, response.body.getInteger("totalRecords"));
    String dummyId = response.body.getJsonArray("permissions").getJsonObject(0).getString("id");

    response = send(HttpMethod.GET, "/perms/permissions?includeDummy=true&query=permissionName=" + normalPerm,
        null, context);
    context.assertEquals(200, response.code);
    context.assertEquals(1, response.body.getInteger("totalRecords"));
    String normalId = response.body.getJsonArray("permissions").getJsonObject(0).getString("id");

    JsonObject o = new JsonObject().put("id", dummyId).put("permissionName", dummyPerm);
    response = send(HttpMethod.PUT, "/perms/permissions/" + dummyId, o.encode(), context);
    context.assertEquals(response.code, 400);
    context.assertEquals("dummy permissions cannot be modified", response.body.getString("text"));

    response = send(HttpMethod.DELETE, "/perms/permissions/" + normalId, null, context);
    context.assertEquals(response.code, 204);

    response = send(HttpMethod.GET, "/perms/permissions?includeDummy=true&query=permissionName=" + normalPerm,
        null, context);
    context.assertEquals(200, response.code);
    context.assertEquals(0, response.body.getInteger("totalRecords"));

    response = send(HttpMethod.GET, "/perms/permissions?includeDummy=true&query=permissionName=" + dummyPerm,
        null, context);
    context.assertEquals(200, response.code);
    context.assertEquals(1, response.body.getInteger("totalRecords"));

    response = send(HttpMethod.DELETE, "/perms/permissions/" + dummyId, null, context);
    context.assertEquals(response.code, 204);

    response = send(HttpMethod.GET, "/perms/permissions?includeDummy=true&query=permissionName=" + dummyPerm,
        null, context);
    context.assertEquals(200, response.code);
    context.assertEquals(0, response.body.getInteger("totalRecords"));
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
    Assert.assertThat(response.body.getString("text"), containsString("invalid input syntax"));
  }

  @Test
  public void testDeletePermsUsersPermissionsByIdAndPermissionnameDoesNotExist(TestContext context) {
    String uuid = UUID.randomUUID().toString();
    Response response = send(HttpMethod.DELETE, "/perms/users/" + uuid + "/permissions/name",
        null, context);
    context.assertEquals(response.code, 404);
    context.assertEquals("User with id " + uuid +" does not exist", response.body.getString("text"));
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
    context.assertEquals(response.code, 404);
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
  public void testPostTenantPermissionsBadTenant(TestContext context) {
    List<OkapiPermission> perms = new LinkedList<>();
    perms.add(new OkapiPermission().withPermissionName("perm" + UUID.randomUUID().toString()));
    OkapiPermissionSet set = new OkapiPermissionSet().withPerms(perms).withModuleId("amodule");
    Response response = send("badTenant", HttpMethod.POST, "/_/tenantpermissions", Json.encode(set), context);
    context.assertEquals(400, response.code);
  }

  @Test
  public void testPostTenantPermissionsEmpty(TestContext context) {
    OkapiPermissionSet set = new OkapiPermissionSet();
    Response response = send(HttpMethod.POST, "/_/tenantpermissions", Json.encode(set), context);
    context.assertEquals(422, response.code);

    set = new OkapiPermissionSet().withModuleId("module");
    response = send(HttpMethod.POST, "/_/tenantpermissions", Json.encode(set), context);
    context.assertEquals(201, response.code);
  }

  @Test
  public void testPostTenantPermissionsNoPermissionName(TestContext context) {
    List<OkapiPermission> perms = new LinkedList<>();
    perms.add(new OkapiPermission());
    OkapiPermissionSet set = new OkapiPermissionSet().withPerms(perms).withModuleId("module");
    Response response = send(HttpMethod.POST, "/_/tenantpermissions", Json.encode(set), context);
    context.assertEquals(201, response.code);
  }

  @Test
    public void testPostTenantPermissionsMutual1(TestContext context) {
    String permName1 = "perm" + UUID.randomUUID().toString();
    String permName2 = "perm" + UUID.randomUUID().toString();
    List<OkapiPermission> perms = new LinkedList<>();
    perms.add(new OkapiPermission().withPermissionName(permName1).withSubPermissions(Arrays.asList(permName2)));
    perms.add(new OkapiPermission().withPermissionName(permName2).withSubPermissions(Arrays.asList(permName1)));
    OkapiPermissionSet set = new OkapiPermissionSet().withModuleId("module-1.2.3").withPerms(perms);
    Response response = send(HttpMethod.POST, "/_/tenantpermissions", Json.encode(set), context);
    context.assertEquals(400, response.code);
    context.assertTrue(response.body.getString("text").contains("Unable to resolve permission dependencies for"));
  }

  @Test
  public void testPostTenantPermissionsMutual2(TestContext context) {
    String permName1 = "perm" + UUID.randomUUID().toString();
    String permName2 = "perm" + UUID.randomUUID().toString();

    List<OkapiPermission> perms = new LinkedList<>();
    perms.add(new OkapiPermission().withPermissionName(permName2).withSubPermissions(Arrays.asList(permName1)));
    OkapiPermissionSet set = new OkapiPermissionSet().withModuleId("module-2.3.4").withPerms(perms);
    Response response = send(HttpMethod.POST, "/_/tenantpermissions", Json.encode(set), context);
    context.assertEquals(201, response.code);

    response = send(HttpMethod.GET, "/perms/permissions?includeDummy=true&query=permissionName%3D" + permName1, null, context);
    context.assertEquals(200, response.code);
    context.assertEquals(1, response.body.getInteger("totalRecords"));
    String id1 = response.body.getJsonArray("permissions").getJsonObject(0).getString("id");

    response = send(HttpMethod.GET, "/perms/permissions?query=permissionName%3D" + permName2, null, context);
    context.assertEquals(200, response.code);
    context.assertEquals(1, response.body.getInteger("totalRecords"));
    String id2 = response.body.getJsonArray("permissions").getJsonObject(0).getString("id");

    perms.clear();
    perms.add(new OkapiPermission().withPermissionName(permName1).withSubPermissions(Arrays.asList(permName2)));
    set = new OkapiPermissionSet().withModuleId("module-3.4.5").withPerms(perms);
    response = send(HttpMethod.POST, "/_/tenantpermissions", Json.encode(set), context);
    context.assertEquals(201, response.code);

    // Need to get id for permName1 again .. It has changed.. Which appears to be an error
    response = send(HttpMethod.GET, "/perms/permissions?query=permissionName%3D" + permName1, null, context);
    context.assertEquals(200, response.code);
    context.assertEquals(1, response.body.getInteger("totalRecords"));
    id1 = response.body.getJsonArray("permissions").getJsonObject(0).getString("id");

    response = send(HttpMethod.GET, "/perms/permissions/" + id1, null, context);
    context.assertEquals(200, response.code);

    response = send(HttpMethod.DELETE, "/perms/permissions/" + id1, null, context);
    context.assertEquals(204, response.code);

    response = send(HttpMethod.GET, "/perms/permissions/" + id2, null, context);
    context.assertEquals(200, response.code);

    response = send(HttpMethod.DELETE, "/perms/permissions/" + id2, null, context);
    context.assertEquals(204, response.code);
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
      return futureResponse.get(10000, TimeUnit.SECONDS);
    } catch (Exception e) {
      context.fail(e);
      return null;
    }
  }

  private void send(String tenant, String url, TestContext context, HttpMethod method, String content,
                    String contentType, Handler<HttpResponse<Buffer>> handler) {
    HttpRequest<Buffer> request = client.requestAbs(method, url)
      .putHeader("x-okapi-tenant", tenant)
      .putHeader("Accept", CONTENT_TYPE_TEXT_JSON)
      .putHeader("Content-type", contentType);

    if (content == null) {
      request.send(res -> {
        if(res.failed()) {
          context.fail(res.cause());
        }
        handler.handle(res.result());
      });
    } else {
      request.sendBuffer(io.vertx.core.buffer.Buffer.buffer(content), res -> {
        if(res.failed()) {
          context.fail(res.cause());
        }
        handler.handle(res.result());
      });
    }
    logger.debug("Sending " + method.toString() + " request to " +
        url + " with content '" + content + "'");
  }

  class HTTPResponseHandler implements Handler<HttpResponse<Buffer>> {

    CompletableFuture<Response> event;
    public HTTPResponseHandler(CompletableFuture<Response> cf){
      event = cf;
    }
    @Override
    public void handle(HttpResponse hcr) {
        try {
          Response r = new Response();
          r.code = hcr.statusCode();
          try {
            if (CONTENT_TYPE_JSON.equals(hcr.getHeader("Content-Type"))) {
              r.body = hcr.bodyAsJsonObject();
            } else if (CONTENT_TYPE_TEXT.equals(hcr.getHeader("Content-Type"))) {
              r.body = new JsonObject().put("text", hcr.bodyAsString());
            } else {
              r.body = null;
            }
          } catch (Exception e) {
            logger.warn("Warning: '" + hcr.toString() + "' cannot be parsed as JSON");
            r.body = new JsonObject(); //Or should it be null?
          }
          logger.debug("Got code '" + hcr.statusCode() + "' and body '" +
              hcr.toString() + "'");
          event.complete(r);
        } catch(Exception e) {
          event.completeExceptionally(e);
        }
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

  private Future<Void> removeModuleContext(TestContext context, String[] permNames) {
    List<Future> futures = new ArrayList<>();
    Arrays.stream(permNames).forEach(permName -> {
      futures.add(testPermissionExists(context, permName)
          .compose(wr -> {
            Promise<WrappedResponse> p = Promise.promise();
            JsonObject perm = wr.getJson().getJsonArray("permissions").getJsonObject(0);
            perm.remove("moduleName");
            perm.remove("moduleVersion");
            PostgresClient.getInstance(vertx, "diku").update("permissions", perm, perm.getString("id"),
                updateReply -> {
                  if (updateReply.failed()) {
                    p.fail(updateReply.cause());
                    return;
                  }
                  p.complete(wr);
                });
            return p.future();
          }));
    });
    return CompositeFuture.all(futures).mapEmpty();
  }

  private Future<Void> testModuleContextWasAdded(TestContext context, String[] permNames) {
    List<Future> futures = new ArrayList<>();
    Arrays.stream(permNames).forEach(permName -> {
      futures.add(testPermissionExists(context, permName)
          .compose(wr -> {
            JsonObject perm = wr.getJson().getJsonArray("permissions").getJsonObject(0);
            if (perm.getString("moduleName") == null) {
              return Future
                  .failedFuture("Module context wasn't added to " + permName + " during upgrade");
            }
            return Future.succeededFuture(wr);
          }));
    });
    return CompositeFuture.all(futures).mapEmpty();
  }

  private Future<WrappedResponse> sendPermissionSet(TestContext context, JsonObject permissionSet) {
    return sendPermissionSet(context, permissionSet, 201);
  }

  private Future<WrappedResponse> sendPermissionSet(TestContext context, JsonObject permissionSet,
      int expectedCode) {
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("accept", CONTENT_TYPE_TEXT_JSON);
    return TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
        HttpMethod.POST, headers, permissionSet.encode(), expectedCode);
  }

  private Future<WrappedResponse> sendInitialPermissionSet(TestContext context) {
    JsonObject permissionSet = new JsonObject()
        .put("moduleId","dummy-1.0.0")
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
                .put("permissionName", "dummy.update")
                .put("displayName", "Dummy Update")
                .put("description", "Update Dummy Entries")
            )
            .add(new JsonObject()
                .put("permissionName", "dummy.collection.get")
                .put("displayName", "Dummy Collection Get")
                .put("description", "Get Dummy Entry Collection")
            )
            .add(new JsonObject()
                .put("permissionName", "dummy.collection.item.get")
                .put("displayName", "Dummy Collection Item Get")
                .put("description", "Get Dummy Entry Collection Item")
            )
            .add(new JsonObject()
                .put("permissionName", "dummy.all")
                .put("displayName", "Dummy All")
                .put("description", "All Dummy Permissions")
                .put("subPermissions", new JsonArray()
                    .add("dummy.read")
                    .add("dummy.write")
                    .add("dummy.update")
                    .add("dummy.collection.get")
                    .add("dummy.collection.item.get")
                )
            )
        );
    return sendPermissionSet(context, permissionSet);
  }

  private Future<WrappedResponse> sendPermissionSetWithCollision(TestContext context) {
    JsonObject permissionSet = new JsonObject()
        .put("moduleId","collision-1.0.0")
        .put("perms", new JsonArray()
            .add(new JsonObject()
                .put("permissionName", "dummy.read")
                .put("displayName", "Colliding Read")
                .put("description", "Colliding Permission for Reading Dummy Entries")
                .put("visible", true)
            )
        );
    return sendPermissionSet(context, permissionSet, 400);
  }

  private Future<WrappedResponse> sendUpdatedPermissionSet(TestContext context, boolean shouldFail) {
    JsonObject permissionSet = new JsonObject()
        .put("moduleId", "dummy-2.0.0")
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
                .put("permissionName", "dummy.delete")
                .put("displayName", "Dummy Delete")
                .put("description", "Delete Dummy Entries")
            )
            .add(new JsonObject()
                .put("permissionName", "dummy.collection.read")
                .put("displayName", "Dummy Collection Read")
                .put("description", "Read Dummy Entry Collection")
                .put("replaces", new JsonArray()
                    .add("dummy.collection.get")
                    .add("dummy.collection.item.get")
                )
            )
            .add(new JsonObject()
                .put("permissionName", "dummy.all")
                .put("displayName", "Dummy All")
                .put("description", "All Dummy Permissions")
                .put("subPermissions", new JsonArray()
                    .add("dummy.read")
                    .add("dummy.write")
                    .add("dummy.collection.read")
                    .add("dummy.delete")
                )
            )
        );
    return sendPermissionSet(context, permissionSet, shouldFail ? 400 : 201);
  }

  private Future<WrappedResponse> sendOtherPermissionSet(TestContext context) {
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

    return sendPermissionSet(context, permissionSet);
  }

  private Future<WrappedResponse> sendBadPermissionSet(TestContext context) {
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
    return sendPermissionSet(context, permissionSet);
  }

  //need test to find bad.delete and verify that it is a dummy perm
  private Future<WrappedResponse> testBadPermissionSet(TestContext context) {
    return testPermissionExists(context, "bad.delete", true)
      .compose(wr -> {
        JsonObject json = new JsonObject(wr.getBody());
        boolean dummy = json.getJsonArray("permissions").getJsonObject(0)
          .getBoolean("dummy");
        if (!dummy) {
          return Future.failedFuture("bad.delete is not flagged as a dummy perm");
        }
        return Future.succeededFuture(wr);
      });
  }

  private Future<WrappedResponse> sendOtherBadPermissionSet(TestContext context) {
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
    return sendPermissionSet(context, permissionSet);
  }

  private Future<WrappedResponse> sendAlienPermissionSet(TestContext context) {
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
    return sendPermissionSet(context, permissionSet);
  }

  // test soft delete of removed permission
  private Future<WrappedResponse> testSoftDeleteOfRemovedPermission(TestContext context, String permName) {
    return testPermissionExists(context, permName, true).compose(wr -> {
      JsonObject perm = new JsonObject(wr.getBody()).getJsonArray("permissions").getJsonObject(0);
      if (!perm.getBoolean("deprecated")) {
        return Future.failedFuture(permName + " should be deprecated");
        }
        if (!perm.getString("displayName").startsWith(TenantPermsAPI.DEPRECATED_PREFIX)) {
            return Future.failedFuture("the displayName of deprecated permission " + permName
              + " was not updated to indicate it was deprecated");
        }
       return Future.succeededFuture(wr);
    });
  }

  // test soft delete of removed permission
  private Future<WrappedResponse> testDowngradeRestoredPermission(TestContext context) {
    return testPermissionExists(context, "dummy.update", true).compose(wr -> {
      JsonObject perm = new JsonObject(wr.getBody()).getJsonArray("permissions").getJsonObject(0);
      if (perm.getBoolean("deprecated")) {
        return Future.failedFuture("dummy.update should no longer be deprecated");
        }
        if (perm.getString("displayName").startsWith(TenantPermsAPI.DEPRECATED_PREFIX)) {
          return Future.failedFuture(
              "downgrade should have removed the deprecated prefix from the displayName of dummy.update");
      }
      return Future.succeededFuture(wr);
      });
  }

  // test permission rename
  private Future<WrappedResponse> testPermissionRename(TestContext context) {
    return testPermissionExists(context, "dummy.collection.read")
        .compose(resp -> testPermissionExists(context, "dummy.collection.get"))
        .compose(wr -> {
          if (!wr.getJson().getJsonArray("permissions").getJsonObject(0).getBoolean("deprecated")) {
            return Future.failedFuture(
                "permission dummy.collection.get should be deprecated due to being renamed");
          }
          return Future.succeededFuture(wr);
        }).compose(resp -> testPermissionExists(context, "dummy.collection.item.get"))
        .compose(wr -> {
          if (!wr.getJson().getJsonArray("permissions").getJsonObject(0).getBoolean("deprecated")) {
            return Future.failedFuture(
                "permission dummy.collection.item.get should be deprecated due to being renamed");
          }
          return Future.succeededFuture(wr);
        });
  }

  private Future<WrappedResponse> testGrantedTo(TestContext context, WrappedResponse perm,
      String permUserId) {
    if (!perm.getJson().getJsonArray("permissions").getJsonObject(0).getJsonArray("grantedTo")
        .contains(permUserId)) {
      return Future.failedFuture("permission not granted to " + permUserId);
    } else {
      return Future.succeededFuture(perm);
    }
  }

  // test update for parent permission
  private Future<WrappedResponse> testUpdateParentPermission(TestContext context) {
    return testPermissionExists(context, "dummy.all", true).compose(wr -> {
      JsonObject json = new JsonObject(wr.getBody());
      JsonArray subPermissions = json.getJsonArray("permissions").getJsonObject(0)
        .getJsonArray("subPermissions");
      if (subPermissions.size() != 4 || !subPermissions.contains("dummy.delete")) {
        return Future.failedFuture("dummy.all should contain three " + subPermissions.toString() +
          " subPermissions including dummy.delete");
      } else {
        return Future.succeededFuture(wr);
      }
    });
  }

  // test update for child permission
  private Future<WrappedResponse> testUpdateChildPermission(TestContext context) {
    return testPermissionExists(context, "dummy.delete", true).compose( wr -> {
        JsonObject json = new JsonObject(wr.getBody());
        JsonArray childOf = json.getJsonArray("permissions").getJsonObject(0)
            .getJsonArray("childOf");
        if (childOf.size() != 1 || !childOf.contains("dummy.all")) {
          return Future.failedFuture("dummy.delete should be child of dummy.all");
        } else {
          return Future.succeededFuture(wr);
        }
    });
  }

  //load a permission set that includes alien.woo
  //test that alien.woo is a real permission
  private Future<WrappedResponse> testAlienPermissionSet(TestContext context) {
    return testPermissionExists(context, "alien.woo").compose(wr -> {
      JsonObject json = new JsonObject(wr.getBody());
      boolean dummy = json.getJsonArray("permissions").getJsonObject(0)
        .getBoolean("dummy");
      if (dummy) {
        return Future.failedFuture("alien.woo is flagged as a dummy perm");
      } else {
        return Future.succeededFuture(wr);
      }
    });
  }

  private Future<WrappedResponse> postPermUser(TestContext context, String userId, String[] permsToAdd) {
    return postPermUser(context, userId, UUID.randomUUID().toString(), permsToAdd);
  }

  private Future<WrappedResponse> postPermUser(TestContext context, String userId,
    String permUserId, String[] permsToAdd) {
    JsonArray perms = new JsonArray();
    Arrays.stream(permsToAdd).forEach(perms::add);
    JsonObject newUser = new JsonObject()
      .put("id", permUserId)
      .put("userId", userId)
      .put("permissions", perms);
    return TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/users",
      HttpMethod.POST, null, newUser.encode(), 201);
  }

  private Future<WrappedResponse> putPermUserBad(TestContext context,
    String permsUserId) {
    JsonObject modifiedUser = new JsonObject()
      .put("id", permsUserId)
      .put("userId", userId1)
      .put("permissions", new JsonArray().add("spurious.all"));
    return TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/users/123",
      HttpMethod.PUT, null, modifiedUser.encode(), 404);
  }

  private Future<WrappedResponse> testUserPerms(TestContext context, String permsUserId, String[] expected) {
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
    return TestUtil.doRequest(vertx, "http://localhost:"+port+"/perms/users/" + permsUserId +
        "/permissions?expanded=true", HttpMethod.GET, headers, null, 200).compose(res -> {
      try {
          JsonArray nameList = res.getJson().getJsonArray("permissionNames");
          if(nameList == null) {
            return Future.failedFuture("Could not find 'permissionNames' in " + res.getBody());
          } else {
            for (String exp : expected) {
              if (!nameList.contains(exp)) {
                return Future.failedFuture("Namelist does not contain '" + exp + "' " + "( "
                  + res.getBody() + " )");
              }
            }
            return Future.succeededFuture(res);
          }
      } catch(Exception e) {
        return Future.failedFuture(e);
      }
    });
  }

  private Future<WrappedResponse> testUserPermsQuery(TestContext context) {
    Promise<WrappedResponse> promise = Promise.promise();
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    String url;
    try {
      headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
      url = "http://localhost:" + port + "/perms/users?query=" +
          URLEncoder.encode("permissions=dummy*", "UTF-8");
    } catch(Exception e) {
      promise.fail(e);
      return promise.future();
    }
    TestUtil.doRequest(vertx, url, HttpMethod.GET, headers, null, 200).onComplete(res -> {
      if(res.failed()) {
        promise.fail(res.cause());
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
            promise.fail("Permissions record for userId matching "
                + userId1 + " not found in permissionUsers listing");
            return;
          } else {
            promise.complete(res.result());
          }
        } catch(Exception e) {
          promise.fail(e);
        }
      }
    });

    return promise.future();
  }

  private Future<WrappedResponse> testTenantPermissionVisible(TestContext context) {
    Promise<WrappedResponse> promise = Promise.promise();
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    String url;
    try {
      headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
      url = "http://localhost:"+port+ "/perms/permissions?query=" + URLEncoder.encode("permissionName=dummy*", "UTF-8");
    } catch(Exception e) {
      promise.fail(e);
      return promise.future();
    }
    TestUtil.doRequest(vertx, url, HttpMethod.GET, headers, null, 200).onComplete(res -> {
      if(res.failed()) { promise.fail(res.cause()); } else {
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
                promise.fail("visible field of dummy.read should be true. Value is " + visible);
                return;
              }
            } else if (permJson.getString("permissionName").equals("dummy.write")) {
              dummyWriteFound = true;
              boolean visible = permJson.getBoolean("visible");
              if (visible != false) {
                promise.fail("visible field of dummy.write should be true. Value is " + visible);
                return;
              }
            }
          }
          if (dummyReadFound && dummyWriteFound) {
            promise.complete(res.result());
          } else {
            promise.fail(new Exception("Resultset does not contain 'dummy.read' and 'dummy.write'"));
          }
        } catch(Exception e) {
          promise.fail(e);
        }
      }
    });

    return promise.future();
  }

  private Future<WrappedResponse> resolvePermNameConflict(TestContext context) {
    return TestUtil
      .doRequest(vertx, "http://localhost:" + port + "/perms/permissions/" + userDefinedPermId,
        HttpMethod.DELETE,
        MultiMap.caseInsensitiveMultiMap().add(HttpHeaders.ACCEPT, "text/plain"), null, 204);
  }

  private Future<WrappedResponse> testPostPermission(TestContext context) {
    JsonObject json = new JsonObject()
        .put("id", userDefinedPermId)
        .put("permissionName", "dummy.delete")
        .put("displayName", "Dummy Delete")
        .put("description", "Delete Dummy Entries");

    return TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/permissions", HttpMethod.POST,
        null, json.encode(), 201);
  }

  private Future<WrappedResponse> testPostBadPermission(TestContext context) {
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
    return TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/permissions", HttpMethod.POST,
      null, badPermission.encode(), 400);
  }

  private Future<WrappedResponse> testPostNullPermissionName(TestContext context) {
    JsonObject nullPermission = new JsonObject()
        .put("displayName", "nullPermName");
    return TestUtil.doRequest(vertx, "http://localhost:"+port+"/perms/permissions", HttpMethod.POST,
        null, nullPermission.encode(), 201).compose(Future::succeededFuture);
  }

  private Future<WrappedResponse> testNonAsciiUser(TestContext context) {
    Promise<WrappedResponse> promise = Promise.promise();
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
      promise.fail(e);
      return promise.future();
    }
    TestUtil.doRequest(vertx, url, HttpMethod.POST, null, newUser.encode(), 201).onComplete(res -> {
      if(res.failed()) { promise.fail(res.cause()); } else {
        TestUtil.doRequest(vertx, url2, HttpMethod.GET, headers, null, 200 ).onComplete(res2 -> {
          if(res2.failed()) { promise.fail(res2.cause()); } else {
            promise.complete(res2.result());
          }
        });
      }
    });
    return promise.future();
  }

  private Future<WrappedResponse> testPermissionExists(TestContext context, String permissionName,
      boolean includeDummies) {
    String dummyFlag = includeDummies ? "includeDummy=true&" : "";

    String url = "http://localhost:" + port + "/perms/permissions?" + dummyFlag
        + "query=permissionName==" + permissionName;
    return TestUtil.doRequest(vertx, url, HttpMethod.GET, null, null, 200).compose(wr -> {
        JsonObject json = new JsonObject(wr.getBody());
        if(json.getInteger("totalRecords") < 1) {
         return Future.failedFuture("permission " + permissionName + " not found");
        } else {
          return Future.succeededFuture(wr);
      }
    });
  }

  private Future<WrappedResponse> testPermissionExists(TestContext context, String permissionName) {
    return testPermissionExists(context, permissionName, false);
  }

  private Future<WrappedResponse> testPermUserMetadata(TestContext context) {
    String url = "http://localhost:" + port + "/perms/users";
    JsonObject newUser = new JsonObject()
      .put("userId", UUID.randomUUID().toString())
      .put("permissions", new JsonArray());
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    String fakeUserId = UUID.randomUUID().toString();
    headers.add("X-Okapi-Token", makeFakeJWT("mcdonald", fakeUserId, "diku"));
    headers.add("X-Okapi-User-Id", fakeUserId);
    return TestUtil.doRequest(vertx, url, HttpMethod.POST, headers, newUser.encode(), 201).compose(
      res -> {
        try {
          String newUserId = res.getJson().getString("id");
          String url2 = String.format("http://localhost:%s/perms/users/%s", port,
            newUserId);
          return TestUtil.doRequest(vertx, url2, HttpMethod.GET, null, null, 200).compose(res2 -> {
            try {
              JsonObject metadata = res2.getJson().getJsonObject("metadata");
              if (metadata == null) {
                return Future
                  .failedFuture("No metadata found in result: " + res2.getJson().encode());
              } else {
                return Future.succeededFuture(res2);
              }
            } catch (Exception e) {
              return Future.failedFuture(e);
            }
          });
        } catch (Exception e) {
          return Future.failedFuture(e);
        }
      });
  }

  private Future<WrappedResponse> testPermMetadata(TestContext context) {
    String url = "http://localhost:" + port + "/perms/permissions";
    JsonObject newPerm = new JsonObject()
      .put("permissionName", "testmeta.test")
      .put("description", "a permission to test metadata create")
      .put("displayName", "testmeta test");
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    String fakeUserId = UUID.randomUUID().toString();
    headers.add("X-Okapi-Token", makeFakeJWT("mcdonald", fakeUserId, "diku"));
    headers.add("X-Okapi-User-Id", fakeUserId);
    return TestUtil.doRequest(vertx, url, HttpMethod.POST, headers, newPerm.encode(), 201).compose(
      res -> {
        try {
          String newPermId = res.getJson().getString("id");
          String url2 = String.format("http://localhost:%s/perms/permissions/%s", port,
            newPermId);
          return TestUtil.doRequest(vertx, url2, HttpMethod.GET, null, null, 200).compose(res2 -> {
            try {
              JsonObject metadata = res2.getJson().getJsonObject("metadata");
              if (metadata == null) {
                return Future
                  .failedFuture("No metadata found in result: " + res2.getJson().encode());
              } else {
                return Future.succeededFuture(res2);
              }
            } catch (Exception e) {
              return Future.failedFuture(e);
            }
          });
        } catch (Exception e) {
          return Future.failedFuture(e);
        }
      });
  }

  private Future<WrappedResponse> sendNestedSubPerms(TestContext context) {
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
    return TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
        HttpMethod.POST, headers, permissionSet.encode(), 201);
  }

  private Future<WrappedResponse> testNestedSubPermExpansion(TestContext context) {
    Set<String> perms = new HashSet<>(Arrays.asList("test.b", "test.c", "test.d"));
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("X-Okapi-Permissions", new JsonArray().add("perms.permissions.get").encode());
    Promise<WrappedResponse> promise = Promise.promise();
    return TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/permissions?expanded=true&query=(permissionName==test.a)",
        HttpMethod.GET, headers, null, 200).compose(res -> {
      try {
          JsonArray subPermList = res.getJson().getJsonArray("permissions").getJsonObject(0).getJsonArray("subPermissions");
          Set<String> set = new HashSet<>();
          subPermList.forEach(subPerm -> set.add(subPerm.toString()));
          if (set.containsAll(perms) && set.size() == perms.size()) {
            return Future.succeededFuture(res);
          } else {
            return Future.failedFuture("SubPermList does not match " + perms + " ( " + res.getBody() + " )");
          }
      } catch (Exception e) {
        return Future.failedFuture(e);
      }
    });
  }

  private Future<WrappedResponse> sendNestedSubPermsWithException(TestContext context) {
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
    return TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions",
        HttpMethod.POST, headers, permissionSet.encode(), 201);
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

