package org.folio.permstest;

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
import io.vertx.core.Vertx;
import io.vertx.core.http.CaseInsensitiveHeaders;
import io.vertx.core.http.HttpMethod;
import static io.vertx.core.http.HttpMethod.GET;
import static io.vertx.core.http.HttpMethod.POST;
import io.vertx.core.json.JsonObject;
import io.vertx.core.json.JsonArray;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.folio.permstest.TestUtil.WrappedResponse;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;

@RunWith(VertxUnitRunner.class)
public class RestVerticleWithCacheTest {

  private static final String userId1 = "35d05a6a-d61e-4e81-9708-fc44daadbec5";
  private static final String P_ALL = "dummy.all";
  private static final String P_READ = "dummy.read";
  private static final String P_WRITE = "dummy.write";
  private static final String P_DELETE = "dummy.delete";

  private static Vertx vertx;
  static int port;

  @Rule
  public Timeout rule = Timeout.seconds(180); // 3 minutes for loading embedded postgres

  @BeforeClass
  public static void setup(TestContext context) {
    Async async = context.async();
    port = NetworkUtils.nextFreePort();
    TenantClient tenantClient = new TenantClient("http://localhost:" + port, "diku", "diku");
    vertx = Vertx.vertx();
    DeploymentOptions options = new DeploymentOptions().setConfig(new JsonObject().put("http.port", port))
        .setWorker(true);
    try {
      PostgresClient.setIsEmbedded(true);
      PostgresClient.getInstance(vertx).startEmbeddedPostgres();
    } catch (Exception e) {
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
    vertx.close(context.asyncAssertSuccess(res -> {
      PostgresClient.stopEmbeddedPostgres();
      async.complete();
    }));
  }

  @Test
  public void testPermsSeq(TestContext context) {
    Async async = context.async();
    Future<WrappedResponse> startFuture;
    startFuture = sendPermissionSet(context, false).compose(w -> {
      return testPerms(context, Arrays.asList(P_READ, P_WRITE, P_ALL));
    }).compose(w -> {
      return sendPermissionSet(context, true);
    }).compose(w -> {
      return testPerms(context, Arrays.asList(P_READ, P_WRITE, P_ALL, P_DELETE));
    }).compose(w -> {
      return postPermUser(context, userId1);
    }).compose(w -> {
      return testUserPerms(context, w.getJson().getString("id"));
    });

    startFuture.setHandler(res -> {
      if (res.failed()) {
        context.fail(res.cause());
      } else {
        async.complete();
      }
    });
  }

  private Future<WrappedResponse> sendPermissionSet(TestContext context, boolean more) {
    Future<WrappedResponse> future = Future.future();
    JsonObject permissionSet = new JsonObject().put("moduleId", "dummy").put("perms",
        new JsonArray()
            .add(new JsonObject().put("permissionName", "dummy.read").put("displayName", "Dummy Read")
                .put("description", "Read Dummy Entries").put("visible", true))
            .add(new JsonObject().put("permissionName", "dummy.write").put("displayName", "Dummy Write")
                .put("description", "Write Dummy Entries"))
            .add(new JsonObject().put("permissionName", "dummy.all").put("displayName", "Dummy All")
                .put("description", "All Dummy Permissions")
                .put("subPermissions", new JsonArray().add("dummy.read").add("dummy.write"))));

    if (more) {
      permissionSet.getJsonArray("perms")
          .add(new JsonObject().put("permissionName", "dummy.delete").put("displayName", "Dummy Delete")
              .put("description", "Delete Dummy Entries"))
          .getJsonObject(2).getJsonArray("subPermissions").add("dummy.delete");
    }
    ;

    CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
    headers.add("accept", "application/json,text/plain");
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions", HttpMethod.POST, headers,
        permissionSet.encode(), 201).setHandler(res -> {
          if (res.failed()) {
            future.fail(res.cause());
          } else {
            future.complete(res.result());
          }
        });

    return future;
  }

  private Future<WrappedResponse> postPermUser(TestContext context, String userId) {
    JsonObject newUser = new JsonObject().put("userId", userId).put("permissions", new JsonArray().add("dummy.all"));
    Future<WrappedResponse> future = Future.future();
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/users", POST, null, newUser.encode(), 201)
        .setHandler(res -> {
          if (res.failed()) {
            future.fail(res.cause());
          } else {
            future.complete(res.result());
          }
        });

    return future;
  }

  private Future<WrappedResponse> testPerms(TestContext context, List<String> perms) {
    CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
    headers.add("X-Okapi-Permissions", new JsonArray().add("perms.permissions.get").encode());
    Future<WrappedResponse> future = Future.future();
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/permissions?expandSubs=true",
        GET, headers, null, 200).setHandler(res -> {
          try {
            if (res.failed()) {
              future.fail(res.cause());
            } else {
              JsonArray permList = res.result().getJson().getJsonArray("permissions");
              if (permList == null) {
                future.fail("Could not find 'permissions' in " + res.result().getBody());
              } else {
                Set<String> set = new HashSet<>();
                permList.forEach(jo -> set.add(((JsonObject)jo).getString("permissionName")));
                if (set.containsAll(perms)) {
                  future.complete(res.result());
                } else {
                  future.fail("PermList does not contain all " + perms + " ( "
                      + res.result().getBody() + " )");
                }
              }
            }
          } catch (Exception e) {
            future.fail(e);
          }
        });

    return future;
  }

  private Future<WrappedResponse> testUserPerms(TestContext context, String permsUserId) {
    CaseInsensitiveHeaders headers = new CaseInsensitiveHeaders();
    headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
    Future<WrappedResponse> future = Future.future();
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/users/" + permsUserId + "/permissions?expanded=true",
        GET, headers, null, 200).setHandler(res -> {
          try {
            if (res.failed()) {
              future.fail(res.cause());
            } else {
              JsonArray nameList = res.result().getJson().getJsonArray("permissionNames");
              if (nameList == null) {
                future.fail("Could not find 'permissionNames' in " + res.result().getBody());
              } else {
                if (nameList.contains("dummy.read") && nameList.contains("dummy.write")) {
                  future.complete(res.result());
                } else {
                  future.fail("Namelist does not contain 'dummy.read' and 'dummy.write' " + "( "
                      + res.result().getBody() + " )");
                }
              }
            }
          } catch (Exception e) {
            future.fail(e);
          }
        });

    return future;
  }
}
