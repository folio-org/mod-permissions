package org.folio.permstest;

import io.vertx.core.Future;
import io.vertx.core.MultiMap;
import io.vertx.core.Promise;
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
import org.folio.rest.client.TenantClient;
import org.folio.rest.jaxrs.model.TenantAttributes;

import java.util.Map;

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
            return Future.succeededFuture(null);
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

}
