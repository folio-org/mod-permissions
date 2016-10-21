import io.vertx.core.DeploymentOptions;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpClient;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import org.folio.rest.tools.utils.NetworkUtils;
import org.folio.rest.RestVerticle;
import org.folio.rest.persist.MongoCRUD;
import org.junit.AfterClass;
import org.junit.Test;




@RunWith(VertxUnitRunner.class)
public class RestVerticleTest {
  private static Vertx vertx;
  static int port;
  
  @BeforeClass
  public static void setup(TestContext context) {
    Async async = context.async();
    vertx = Vertx.vertx();
    port = NetworkUtils.nextFreePort();
    DeploymentOptions options = new DeploymentOptions().setConfig(new JsonObject().put("http.port", port));
    vertx.deployVerticle(RestVerticle.class.getName(), options, res -> {
      MongoCRUD.setIsEmbedded(true);
      try {
        MongoCRUD.getInstance(vertx).startEmbeddedMongo();
      } catch(Exception e) {
        e.printStackTrace();
      }
      async.complete();
    });
    
  }
  
  @AfterClass
  public static void teardown(TestContext context) {
    context.async().complete();
    
  }
  
  

  private Future<Void> getEmptyUsers(TestContext context) {
    Future future = Future.future();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/users", res -> {
      if(res.statusCode() != 200) {
        res.bodyHandler(buf -> {
          String body = buf.toString();
          future.fail("Bad status code: " + res.statusCode() + " : " + body);
        });
      } else {
        res.bodyHandler(buf -> {
          JsonObject userCollectionObject = buf.toJsonObject();
          if(userCollectionObject.getJsonArray("users").size() == 0 &&
                  userCollectionObject.getInteger("total_records") == 00) {
            future.complete();
          } else {
            future.fail("Invalid return JSON: " + buf.toString());
          }
        });
      }
    })
            .putHeader("tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "application/json")
            .end();
    return future;
  }
  
  
  
  private Future<Void> postUser(TestContext context) {
    Future future = Future.future();
    JsonObject userObject = new JsonObject()
            .put("username", "joeblock")
            .put("id", "1234567")
            .put("active", true);
    HttpClient client = vertx.createHttpClient();
    client.post(port, "localhost", "/users", res -> {
      if(res.statusCode() >= 200 && res.statusCode() < 300) {
        future.complete();
      } else {
        future.fail("Got status code: " + res.statusCode());
      }
    })
            .putHeader("tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "application/json")
            .end(userObject.encode());
    return future;
  }
  
 private Future<Void> getUser(TestContext context) {
   Future future = Future.future();
   HttpClient client = vertx.createHttpClient();
   client.get(port, "localhost", "/users/1234567", res -> {
     if(res.statusCode() == 200) {
       res.bodyHandler(buf -> {
         JsonObject userObject = buf.toJsonObject();
         if(userObject.getString("username").equals("joeblock")) {
           future.complete();
         } else {
           future.fail("Unable to read proper data from JSON return value: " + buf.toString());
         }
       });
     } else {
       future.fail("Bad response: " + res.statusCode());
     }
   })
           .putHeader("tenant", "diku")
           .putHeader("content-type", "application/json")
           .putHeader("accept", "application/json")
           .end();
   return future;
 }
 
 @Test
  public void doSequentialTests(TestContext context) {
    Async async = context.async();
    Future<Void> startFuture;
    Future<Void> f1 = Future.future();
    getEmptyUsers(context).setHandler(f1.completer());
    startFuture = f1.compose(v -> {
      Future<Void> f2 = Future.future();
      postUser(context).setHandler(f2.completer());
      return f2;
    }).compose(v -> {
      Future<Void> f3 = Future.future();
      getUser(context).setHandler(f3.completer());
      return f3;
    });
            
    
    startFuture.setHandler(res -> {
      if(res.succeeded()) {
        async.complete();
      } else {
        context.fail(res.cause());
      }
    });
  }
  
}