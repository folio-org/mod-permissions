package org.folio.rest.impl;

import java.util.LinkedList;
import java.util.List;
import org.folio.permstest.TestUtil;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.Timeout;
import org.junit.runner.RunWith;
import io.vertx.core.DeploymentOptions;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import io.vertx.ext.web.client.WebClient;

@RunWith(VertxUnitRunner.class)
public class TenantRefAPITest {

  private static Vertx vertx;
  private static WebClient client;
  static int port;

  @Rule
  public Timeout rule = Timeout.seconds(240);  // 4 (?!) minutes for loading embedded postgres

  @BeforeClass
  public static void setup(TestContext context) {
        PostgresClient.setPostgresTester(new PostgresTesterContainer());
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
              parameters.add(new Parameter().withKey("loadSample").withValue("false"));
              ta.setParameters(parameters);
              TestUtil.tenantOp(tenantClient, ta)
                  .onComplete(context.asyncAssertSuccess());
            }));
  }

  @AfterClass
  public static void teardown(TestContext context) {
    client.close();
    vertx.close(context.asyncAssertSuccess());
  }

  @Test
  public void testGetSystemPermsBadTenant(TestContext context) {
    String tenantId = "badTenant";

    TenantRefAPI api = new TenantRefAPI();
    PostgresClient postgresClient = PostgresClient.getInstance(vertx, tenantId);

    postgresClient.startTx(connection -> {
      api.getSystemPerms(connection, tenantId, vertx.getOrCreateContext())
          .onComplete(context.asyncAssertFailure());
    });
  }

  @Test
  public void testGetSystemPermsNullContext(TestContext context) {
    String tenantId = "diku";

    TenantRefAPI api = new TenantRefAPI();
    PostgresClient postgresClient = PostgresClient.getInstance(vertx, tenantId);

    postgresClient.startTx(connection -> {
      api.getSystemPerms(connection, tenantId, null)
          .onComplete(context.asyncAssertFailure());
    });
  }
}
