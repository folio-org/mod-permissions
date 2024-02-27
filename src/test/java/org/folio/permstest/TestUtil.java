package org.folio.permstest;

import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.MultiMap;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpRequest;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.okapi.common.XOkapiHeaders;
import org.folio.postgres.testing.PostgresTesterContainer;
import org.folio.rest.RestVerticle;
import org.folio.rest.client.TenantClient;
import org.folio.rest.impl.PermsCache;
import org.folio.rest.impl.TenantRefAPI;
import org.folio.rest.jaxrs.model.Parameter;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.NetworkUtils;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import javax.ws.rs.core.Response;

/**
 *
 * @author kurt
 */
public class TestUtil {
  public static final String CONTENT_TYPE_JSON = "application/json";
  public static final String CONTENT_TYPE_TEXT = "text/plain";
  public static final String CONTENT_TYPE_TEXT_JSON = "application/json,text/plain";
  private static final Logger LOGGER = LogManager.getLogger(TestUtil.class);

  static class WrappedResponse {
    private final String body;
    private JsonObject json;

    public WrappedResponse(String body) {
      this.body = body;
      try {
        json = new JsonObject(body);
      } catch(Exception e) {
        json = null;
      }
    }

    public String getBody() {
      return body;
    }

    public JsonObject getJson() {
      return json;
    }
  }

  public static Future<WrappedResponse> doRequest(Vertx vertx, String url,
    HttpMethod method, MultiMap headers, String payload,
    Integer expectedCode) {
    WebClient client = WebClient.create(vertx);
    HttpRequest<Buffer> request = client.requestAbs(method, url);
    //Add standard headers
    request.putHeader(XOkapiHeaders.TENANT, "diku")
      .putHeader("content-type", CONTENT_TYPE_JSON)
      .putHeader("accept", CONTENT_TYPE_JSON);
    if (headers != null) {
      for (Map.Entry<String, String> entry : headers.entries()) {
        request.putHeader(entry.getKey(), entry.getValue());
      }
    }

    LOGGER.info("Sending {} request to url '{}' with payload: {}", method.toString(), url, payload);
    if (method == HttpMethod.PUT || method == HttpMethod.POST) {
      return request.sendBuffer(Buffer.buffer(payload))
          .compose(httpResponse -> handler(httpResponse, expectedCode, method, url));
    } else {
      return request.send().compose(httpResponse -> handler(httpResponse, expectedCode, method, url));
    }
  }

  public static Future<WrappedResponse> handler(HttpResponse<Buffer> req,
      Integer expectedCode, HttpMethod method, String url) {

    String buf = req.bodyAsString();
    if (expectedCode == null || expectedCode == req.statusCode()) {
      return Future.succeededFuture(new WrappedResponse(buf));
    }
    Error e = new AssertionError(String.format(
        "%s request to %s failed. Expected status code '%s' but got status code '%s': %s",
        method, url, expectedCode, req.statusCode(), buf));
    LOGGER.error(e.getMessage(), e);
    return Future.failedFuture(e);
  }

  public static Future<Void> tenantOp(TenantClient tenantClient, TenantAttributes ta) {
    return tenantClient.postTenant(ta)
        .compose(res -> {
          if (res.statusCode() == 200 || res.statusCode() == 204) {
            return Future.succeededFuture();
          } else if (res.statusCode() != 201) {
            return Future.failedFuture("tenantOp returned status " + res.statusCode());
          }
          return tenantClient.getTenantByOperationId(res.bodyAsJsonObject().getString("id"), 60000);
        })
        .map(res -> {
          if (res == null) {
            return null;
          }
          JsonObject jsonResponse = res.bodyAsJsonObject();
          if (!jsonResponse.getBoolean("complete")) {
            throw new RuntimeException("tenant Op did not complete");
          }
          String error = jsonResponse.getString("error");
          if (error != null) {
            throw new RuntimeException(error);
          }
          return null;
        });
  }

  public static Future<Integer> setupDiku(Vertx vertx) {
    PostgresClient.setPostgresTester(new PostgresTesterContainer());
    Integer port = NetworkUtils.nextFreePort();
    DeploymentOptions options = new DeploymentOptions().setConfig(new JsonObject()
        .put("http.port", port).put(PermsCache.CACHE_HEADER, false)).setWorker(false);
    TenantAttributes ta = new TenantAttributes();
    ta.setModuleTo("mod-permissions-99999.0.0");
    List<Parameter> parameters = new LinkedList<>();
    parameters.add(new Parameter().withKey("loadSample").withValue("true"));
    ta.setParameters(parameters);

    return vertx.deployVerticle(RestVerticle.class.getName(), options)
        .compose(x -> postTenantSync(ta, Map.of(XOkapiHeaders.TENANT, "diku"), vertx))
        .map(port);
  }

  private static Future<Response> postTenantSync(
      TenantAttributes tenantAttributes, Map<String, String> headers, Vertx vertx) {

    return Future.future(handler -> new TenantRefAPI()
        .postTenantSync(tenantAttributes, headers, handler, vertx.getOrCreateContext()));
  }

}
