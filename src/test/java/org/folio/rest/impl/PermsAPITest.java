package org.folio.rest.impl;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import java.util.Arrays;
import java.util.List;
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

  private final Logger logger = LoggerFactory.getLogger(PermsAPITest.class);
  static Vertx vertx;

  @BeforeClass
  public static void setup(TestContext context) {
    vertx = Vertx.vertx();
  }

  @AfterClass
  public static void tearDown(TestContext context) {
    vertx.close(x -> context.asyncAssertFailure(y -> PostgresClient.stopEmbeddedPostgres()));
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
          vertxContext, tenantId, logger);
      future.onComplete(context.asyncAssertFailure());
    });
  }
}
