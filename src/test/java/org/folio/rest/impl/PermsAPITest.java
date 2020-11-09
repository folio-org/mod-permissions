package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.PermissionUser;
import org.folio.rest.persist.PostgresClient;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import static org.hamcrest.CoreMatchers.containsString;

@RunWith(VertxUnitRunner.class)
public class PermsAPITest {

  static Vertx vertx;

  @BeforeClass
  public static void setup(TestContext context) {
    vertx = Vertx.vertx();
  }

  @AfterClass
  public static void tearDown(TestContext context) {
    PostgresClient.stopEmbeddedPostgres();
    vertx.close();
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

    api.postPermsUsersTrans(permissionUser, vertx.getOrCreateContext(), "badTenant",
        context.asyncAssertSuccess(res -> {
          context.assertEquals(400, res.getStatus());
        }));
  }

  @Test
  public void testRefreshCacheFail(TestContext context) {
    Future<Permission> fullPerms = PermsCache.getFullPerms("foo",
        vertx.getOrCreateContext(), "badTenant").onComplete(context.asyncAssertFailure(res -> {
      Assert.assertThat(res.getMessage(),
          containsString("relation \"badtenant_mod_permissions.permissions\" does not exist"));
    }));
  }

  @Test
  public void testUpdateUserPermissionsFail(TestContext context) {
    PermsAPI api = new PermsAPI();

    String tenantId = "badTenant";
    Context vertxContext = vertx.getOrCreateContext();
    PostgresClient postgresClient = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    postgresClient.startTx(s -> {
      Future<Void> future = PermsAPI.updateUserPermissions(s, "bad",
          new JsonArray().add("this"), new JsonArray().add("that"),
          vertxContext, tenantId);
      future.onComplete(context.asyncAssertFailure());
    });
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

    api.getPermsPermissions(null, null, null, 1, 1, null, null,
        null, null, null, context.asyncAssertSuccess(res -> {
      context.assertEquals(500, res.getStatus());
    }), null);
  }

}
