package org.folio.permstest;

import io.vertx.core.AsyncResult;
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
    Promise<WrappedResponse> promise = Promise.promise();
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
      request.sendBuffer(Buffer.buffer(payload),
        res -> handler(res, promise, expectedCode, method, url));
    } else {
      request.send(res -> handler(res, promise, expectedCode, method, url));
    }
    return promise.future();
  }

  public static void handler(AsyncResult<HttpResponse<Buffer>> res,
    Promise<WrappedResponse> promise, Integer expectedCode, HttpMethod method, String url) {
    if (res.failed()) {
      promise.fail(res.cause());
    } else {
      HttpResponse<Buffer> req = res.result();
      Buffer buf = req.bodyAsBuffer();
      if (expectedCode != null && expectedCode != req.statusCode()) {
        promise.fail(String.format("%s request to %s failed. Expected status code"
            + " '%s' but got status code '%s': %s", method, url,
          expectedCode, req.statusCode(), buf));
      } else {
        System.out
          .println("Got status code " + req.statusCode() + " with payload of " + buf.toString());
        WrappedResponse wr = new WrappedResponse(req.statusCode(), buf.toString(), req);
        promise.complete(wr);
      }
    }
  }

}
