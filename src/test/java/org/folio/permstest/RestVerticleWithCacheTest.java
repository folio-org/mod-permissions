package org.folio.permstest;

import io.vertx.core.MultiMap;
import io.vertx.core.Promise;
import io.vertx.core.Future;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.core.json.JsonArray;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import org.folio.permstest.TestUtil.WrappedResponse;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.impl.PermsCache;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.rest.tools.utils.TenantInit;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;

import static io.vertx.core.http.HttpMethod.GET;
import static io.vertx.core.http.HttpMethod.POST;

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
  public Timeout rule = Timeout.seconds(240); // 4 minutes for loading embedded postgres

  @BeforeClass
  public static void setup(TestContext context) {
        PostgresClient.setPostgresTester(new PostgresTesterContainer());
    port = NetworkUtils.nextFreePort();
    PermsCache.setCachePeriod(3000);
    TenantClient tenantClient = new TenantClient("http://localhost:" + port, "diku", null);
    vertx = Vertx.vertx();
    DeploymentOptions options = new DeploymentOptions().setConfig(new JsonObject().put("http.port", port))
        .setWorker(false);

    vertx.deployVerticle(RestVerticle.class.getName(), options)
    .compose(x -> TenantInit.purge(tenantClient, 10000))  // purge old data when reusing external database
    .compose(x -> {
      TenantAttributes ta = new TenantAttributes();
      ta.setModuleTo("mod-permissions-1.0.0");
      List<Parameter> parameters = new LinkedList<>();
      parameters.add(new Parameter().withKey("loadSample").withValue("true"));
      ta.setParameters(parameters);
      return TestUtil.tenantOp(tenantClient, ta);
    })
    .onComplete(context.asyncAssertSuccess());
  }

  @AfterClass
  public static void teardown(TestContext context) {
    vertx.close(context.asyncAssertSuccess());
  }

  @Test
  public void testPermsSeq(TestContext context) {
    Future<WrappedResponse> startFuture;
    startFuture = sendPermissionSet(context, false).compose(w -> {
      return testSubPermExpansion(context, Arrays.asList(P_READ, P_WRITE));
    }).compose(w -> {
      return sendPermissionSet(context, true);
    }).compose(w -> {
      return testSubPermExpansion(context, Arrays.asList(P_READ, P_WRITE, P_DELETE));
    }).compose(w -> {
      return postPermUser(context, userId1);
    }).compose(w -> {
      return testUserPerms(context, w.getJson().getString("id"));
    }).compose(w -> {
      // refresh cache
      return testSubPermExpansionAfterWait(context, Arrays.asList(P_READ, P_WRITE), 4);
    }).compose(w -> {
      // test again
      return testSubPermExpansionAfterWait(context, Arrays.asList(P_READ, P_WRITE, P_DELETE), 2);
    });

    startFuture.onComplete(context.asyncAssertSuccess());
  }

  private Future<WrappedResponse> sendPermissionSet(TestContext context, boolean more) {
    Promise<WrappedResponse> promise = Promise.promise();
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

    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("accept", "application/json,text/plain");
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/_/tenantpermissions", HttpMethod.POST, headers,
        permissionSet.encode(), 201).onComplete(res -> {
          if (res.failed()) {
            promise.fail(res.cause());
          } else {
            promise.complete(res.result());
          }
        });

    return promise.future();
  }

  private Future<WrappedResponse> postPermUser(TestContext context, String userId) {
    JsonObject newUser = new JsonObject().put("userId", userId).put("permissions", new JsonArray().add("dummy.all"));
    return TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/users", POST, null, newUser.encode(), 201);
  }

  private Future<WrappedResponse> testSubPermExpansion(TestContext context, List<String> perms) {
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("X-Okapi-Permissions", new JsonArray().add("perms.permissions.get").encode());
    Promise<WrappedResponse> promise = Promise.promise();
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/permissions?expandSubs=true&query=(permissionName==" + P_ALL + ")",
        GET, headers, null, 200).onComplete(res -> {
          try {
            if (res.failed()) {
              promise.fail(res.cause());
            } else {
              JsonArray subPermList = res.result().getJson().getJsonArray("permissions").getJsonObject(0).getJsonArray("subPermissions");
              Set<String> set = new HashSet<>();
              subPermList.forEach(jo -> set.add(((JsonObject)jo).getString("permissionName")));
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

  private Future<WrappedResponse> testSubPermExpansionAfterWait(TestContext context, List<String> perms, long waitSeconds) {
    Promise<WrappedResponse> promise = Promise.promise();
    vertx.setTimer(1000 * waitSeconds, t -> {
      testSubPermExpansion(context, perms).onComplete(h -> {
        promise.complete(h.result());
      });
    });
    return promise.future();
  }

  private Future<WrappedResponse> testUserPerms(TestContext context, String permsUserId) {
    MultiMap headers = MultiMap.caseInsensitiveMultiMap();
    headers.add("X-Okapi-Permissions", new JsonArray().add("perms.users.get").encode());
    Promise<WrappedResponse> promise = Promise.promise();
    TestUtil.doRequest(vertx, "http://localhost:" + port + "/perms/users/" + permsUserId + "/permissions?expanded=true",
        GET, headers, null, 200).onComplete(res -> {
          try {
            if (res.failed()) {
              promise.fail(res.cause());
            } else {
              JsonArray nameList = res.result().getJson().getJsonArray("permissionNames");
              if (nameList == null) {
                promise.fail("Could not find 'permissionNames' in " + res.result().getBody());
              } else {
                if (nameList.contains("dummy.read") && nameList.contains("dummy.write")) {
                  promise.complete(res.result());
                } else {
                  promise.fail("Namelist does not contain 'dummy.read' and 'dummy.write' " + "( "
                      + res.result().getBody() + " )");
                }
              }
            }
          } catch (Exception e) {
            promise.fail(e);
          }
        });
    return promise.future();
  }
}
