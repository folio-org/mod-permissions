package org.folio.rest.impl;

import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.ws.rs.core.Response;
import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.permstest.TestUtil;
import org.folio.rest.jaxrs.model.PermissionNameListObject;
import org.folio.rest.jaxrs.model.PermissionNameObject;
import org.folio.rest.jaxrs.model.PermissionUpload;
import org.folio.rest.jaxrs.model.PermissionUser;
import org.folio.rest.persist.PostgresClient;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(VertxUnitRunner.class)
public class PermsAPITest {

  private static Vertx vertx;

  @BeforeClass
  public static void setup(TestContext context) {
    vertx = Vertx.vertx();
    TestUtil.setupDiku(vertx).onComplete(context.asyncAssertSuccess());
  }

  @AfterClass
  public static void tearDown(TestContext context) {
    vertx.close().onComplete(context.asyncAssertSuccess());
  }

  @Test
  public void testSplitStringList() {
    List<List<String>> lists;

    lists = PermsAPI.splitStringList(Arrays.asList(), 1);
    Assert.assertEquals(0, lists.size());

    lists = PermsAPI.splitStringList(Arrays.asList("a"), 2);
    Assert.assertEquals(1, lists.size());
    Assert.assertEquals(Arrays.asList("a"), lists.get(0));

    lists = PermsAPI.splitStringList(Arrays.asList("a", "b"), 2);
    Assert.assertEquals(1, lists.size());
    Assert.assertEquals(Arrays.asList("a", "b"), lists.get(0));

    lists = PermsAPI.splitStringList(Arrays.asList("a", "b", "c"), 2);
    Assert.assertEquals(2, lists.size());
    Assert.assertEquals(Arrays.asList("a", "b"), lists.get(0));
    Assert.assertEquals(Arrays.asList("c"), lists.get(1));

    lists = PermsAPI.splitStringList(Arrays.asList("a", "b", "c", "d"), 2);
    Assert.assertEquals(2, lists.size());
    Assert.assertEquals(Arrays.asList("a", "b"), lists.get(0));
    Assert.assertEquals(Arrays.asList("c", "d"), lists.get(1));
  }

  @Test
  public void testPostPermsUsersTransFailure(TestContext context) {
    PermsAPI api = new PermsAPI();
    PermissionUser permissionUser = new PermissionUser();

    permissionUser.setUserId(UUID.randomUUID().toString());
    Map<String,String> headers = new CaseInsensitiveMap<>();
    headers.put(XOkapiHeaders.TENANT, "badTenant");

    api.postPermsUsersTrans(permissionUser, vertx.getOrCreateContext(), headers,
        context.asyncAssertSuccess(res -> {
          context.assertEquals(400, res.getStatus());
        }));
  }

  @Test
  public void testRefreshCacheFail(TestContext context) {
    PermsCache.getFullPerms("foo",
        vertx.getOrCreateContext(), "badTenant").onComplete(context.asyncAssertFailure(res -> {
      assertThat(res.getMessage(), allOf(
          anyOf(containsString("password authentication failed"), containsString("does not exist")),
          containsString("badtenant_mod_permissions")));
    }));
  }

  @Test
  public void testUpdateUserPermissionsFail(TestContext context) {
    String tenantId = "badTenant";
    Context vertxContext = vertx.getOrCreateContext();
    PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    postgresClient.withTrans(conn ->
          PermsAPI.updateUserPermissions(conn, "bad",
              new JsonArray().add("this"), new JsonArray().add("that"),
              vertxContext, tenantId, null)
    ).onComplete(context.asyncAssertFailure());
  }

  @Test
  public void testPostPermsUsersNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.postPermsUsers(null, null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testPutPermsUsersByIdNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.putPermsUsersById(null, null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testDeletePermsUsersByIdNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.deletePermsUsersById(null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testGetPermsUsersPermissionsByIdNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.getPermsUsersPermissionsById(null, null, null, null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testPostPermsUsersPermissionsByIdNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.postPermsUsersPermissionsById(null, null, null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testDeletePermsUsersPermissionsByIdAndPermissionnameNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.deletePermsUsersPermissionsByIdAndPermissionname(null, null, null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testPostPermsPermissionsNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.postPermsPermissions(null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testGetPermsPermissionsByIdNullId(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.getPermsPermissionsById(null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(404, res.getStatus());
    }), null);
  }

  @Test
  public void testGetPermsPermissionsByIdNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.getPermsPermissionsById(UUID.randomUUID().toString(), null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testPutPermsPermissionsByIdNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.putPermsPermissionsById(null, null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testDeletePermsPermissionsByIdNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.deletePermsPermissionsById(null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testGetPermsPermissionsNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.getPermsPermissions(null, null, null, null,  0, 1, 1, 1, null, null,
        null, null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testPostPermsPurgeDeprecatedNullPointer(TestContext context) {
    PermsAPI api = new PermsAPI();

    api.postPermsPurgeDeprecated(null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

  @Test
  public void testPostPermsPurgeDeprecatedBadTenant(TestContext context) {
    PermsAPI api = new PermsAPI();

    Map<String, String> headers = new CaseInsensitiveMap<>();
    headers.put(XOkapiHeaders.TENANT, "foo");
    api.postPermsPurgeDeprecated(headers, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), vertx.getOrCreateContext());
  }

  @Test
  public void testTokenPayload() {
    Assert.assertNull(PermsAPI.getPayloadWithoutValidation(null));
    Assert.assertThrows(IllegalArgumentException.class, () -> PermsAPI.getPayloadWithoutValidation("a"));
    Assert.assertThrows(IllegalArgumentException.class, () -> PermsAPI.getPayloadWithoutValidation("a.b"));
    Assert.assertThrows(IllegalArgumentException.class, () -> PermsAPI.getPayloadWithoutValidation("a.b.c"));
    Assert.assertThrows(IllegalArgumentException.class, () -> PermsAPI.getPayloadWithoutValidation("a.YQo=.c"));
    Assert.assertEquals("{}", PermsAPI.getPayloadWithoutValidation("a.e30K.c").encode());
  }

  @Test
  public void testUserPermissions(TestContext context) {
    var perm1 = new PermissionUpload().withId(randomUuid()).withPermissionName("canBrewCoffee");
    var perm2 = new PermissionUpload().withId(randomUuid()).withPermissionName("canDrinkCoffee");
    var user = new PermissionUser().withId(randomUuid()).withUserId(randomUuid());
    postPermission(perm1)
    .compose(x -> postPermission(perm2))
    .compose(x -> postUsers(user))
    .compose(x -> postUsersPermissions(user, perm1))
    .compose(x -> postUsersPermissions(user, perm2))
    .compose(x -> postUsersPermissions(user, perm1))
    .compose(x -> getUsersPermissions(user))
    .compose(x -> assertPermissions(context, user, List.of("canBrewCoffee", "canDrinkCoffee")))
    .compose(x -> deleteUsersPermissions(user, perm1))
    .compose(x -> assertPermissions(context, user, List.of("canDrinkCoffee")))
    .onComplete(context.asyncAssertSuccess());
  }

  @Test
  public void testDuplicatePermission(TestContext context) {
    var perm1 = new PermissionUpload().withId(randomUuid()).withPermissionName("canSing");
    var perm2 = new PermissionUpload().withId(randomUuid()).withPermissionName("canDance");
    var user = new PermissionUser().withId(randomUuid()).withUserId(randomUuid())
        .withPermissions(List.of("canSing", "canDance", "canSing"));
    postPermission(perm1)
    .compose(x -> postPermission(perm2))
    .compose(x -> postUsers(user))
    .compose(x -> getUsersPermissions(user))
    .compose(x -> assertPermissions(context, user, List.of("canSing", "canDance")))
    .compose(x -> putUsers(user))
    .compose(x -> assertPermissions(context, user, List.of("canSing", "canDance")))
    .compose(x -> PostgresClient.getInstance(vertx, "diku")
        .execute("""
                 UPDATE diku_mod_permissions.permissions_users
                 SET jsonb = jsonb_set(jsonb, '{permissions}',
                                       '["canDance", "canSing", "canDance", "canSing"]')
                 WHERE id='""" + user.getId() + "'"))
    .compose(x -> assertPermissions(context, user, List.of("canDance", "canSing", "canDance", "canSing")))
    .compose(x -> getUsersPermissions(user))
    .onComplete(context.asyncAssertSuccess(permissions -> {
      assertThat(permissions, is(List.of("canDance", "canSing")));
    }))
    .compose(x -> deleteUsersPermissions(user, perm2))
    .compose(x -> assertPermissions(context, user, List.of("canSing")))
    .onComplete(context.asyncAssertSuccess());
  }

  private Future<Void> assertPermissions(TestContext context, PermissionUser user, List<String> expected) {
    return PostgresClient.getInstance(vertx, "diku")
      .selectSingle("""
                    SELECT jsonb->'permissions'
                    FROM diku_mod_permissions.permissions_users
                    WHERE id='""" + user.getId() + "'")
      .onComplete(context.asyncAssertSuccess(row -> {
        assertThat(row.getJsonArray(0).getList(), is(expected));
      }))
      .mapEmpty();
  }

  private Future<Response> postUsers(PermissionUser user) {
    var vertxContext = vertx.getOrCreateContext();
    return Future.future(handler -> new PermsAPI()
        .postPermsUsersTrans(user, vertxContext, headers(), handler));
  }

  private Future<Response> putUsers(PermissionUser user) {
    var vertxContext = vertx.getOrCreateContext();
    return Future.future(handler -> new PermsAPI()
        .putPermsUsersById(user.getId(), user, headers(), handler, vertxContext));
  }

  private Future<Response> postPermission(PermissionUpload perm) {
    var vertxContext = vertx.getOrCreateContext();
    return Future.future(handler -> new PermsAPI().postPermsPermissions(perm, headers(), handler, vertxContext));
  }

  private Future<Response> postUsersPermissions(PermissionUser user, PermissionUpload perm) {
    var vertxContext = vertx.getOrCreateContext();
    var permissionNameObject = new PermissionNameObject().withPermissionName(perm.getPermissionName());
    return Future.<Response>future(handler -> new PermsAPI().postPermsUsersPermissionsById(
        user.getId(), null, permissionNameObject, headers(), handler, vertxContext));
  }

  private Future<List<Object>> getUsersPermissions(PermissionUser user) {
    var vertxContext = vertx.getOrCreateContext();
    return Future.<Response>future(handler -> new PermsAPI().getPermsUsersPermissionsById(
        user.getId(), null, null, null, headers(), handler, vertxContext))
        .map(response -> ((PermissionNameListObject) response.getEntity()).getPermissionNames());
  }

  private Future<Response> deleteUsersPermissions(PermissionUser user, PermissionUpload perm) {
    var vertxContext = vertx.getOrCreateContext();
    return Future.<Response>future(handler -> new PermsAPI().deletePermsUsersPermissionsByIdAndPermissionname(
        user.getId(), perm.getPermissionName(), null, headers(), handler, vertxContext));
  }

  private String randomUuid() {
    return UUID.randomUUID().toString();
  }

  private Map<String,String> headers() {
    Map<String,String> headers = new CaseInsensitiveMap<>();
    headers.put(XOkapiHeaders.TENANT, "diku");
    return headers;
  }
}
