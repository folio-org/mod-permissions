package org.folio.permstest;

import io.vertx.core.Future;
import io.vertx.core.MultiMap;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpRequest;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;
import java.util.Map;

/**
 *
 * @author kurt
 */
public class TestUtil {
  public static final String CONTENT_TYPE_JSON = "application/json";
  public static final String CONTENT_TYPE_TEXT = "text/plain";
  public static final String CONTENT_TYPE_TEXT_JSON = "application/json,text/plain";

  static class WrappedResponse {
    private int code;
    private String body;
    private JsonObject json;
    private HttpResponse response;

    public WrappedResponse(int code, String body, HttpResponse response) {
      this.code = code;
      this.body = body;
      this.response = response;
      try {
        json = new JsonObject(body);
      } catch(Exception e) {
        json = null;
      }
    }

    public int getCode() {
      return code;
    }

    public String getBody() {
      return body;
    }

    public HttpResponse getResponse() {
      return response;
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
    request.putHeader("X-Okapi-Tenant", "diku")
      .putHeader("content-type", CONTENT_TYPE_JSON)
      .putHeader("accept", CONTENT_TYPE_JSON);
    if (headers != null) {
      for (Map.Entry entry : headers.entries()) {
        request.putHeader((String) entry.getKey(), (String) entry.getValue());
      }
    }

    System.out.println("Sending " + method.toString() + " request to url '" +
      url + " with payload: " + payload + "'\n");
    if (method == HttpMethod.PUT || method == HttpMethod.POST) {
      return request.sendBuffer(Buffer.buffer(payload))
          .compose(httpResponse -> handler(httpResponse, expectedCode, method, url));
    } else {
      return request.send().compose(httpResponse -> handler(httpResponse, expectedCode, method, url));
    }
  }

  public static Future<WrappedResponse> handler(HttpResponse<Buffer> req,
    Integer expectedCode, HttpMethod method, String url) {
    Buffer buf = req.bodyAsBuffer();
    if (expectedCode != null && expectedCode != req.statusCode()) {
      return Future.failedFuture(String.format("%s request to %s failed. Expected status code"
          + " '%s' but got status code '%s': %s", method, url,
        expectedCode, req.statusCode(), buf));
    } else {
      System.out
        .println("Got status code " + req.statusCode() + " with payload of " + buf.toString());
      return Future.succeededFuture(new WrappedResponse(req.statusCode(), buf.toString(), req));
    }
  }
}
