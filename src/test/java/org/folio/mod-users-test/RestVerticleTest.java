import io.vertx.core.DeploymentOptions;
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
  
  
  @Test
  public void getEmptyUsers(TestContext context) {
    Async async = context.async();
    HttpClient client = vertx.createHttpClient();
    client.get(port, "localhost", "/users", res -> {
      if(res.statusCode() != 200) {
        res.bodyHandler(buf -> {
          String body = buf.toString();
          context.fail("Bad status code: " + res.statusCode() + " : " + body);
        });
      } else {
        async.complete();
      }
    })
            .putHeader("tenant", "diku")
            .putHeader("content-type", "application/json")
            .putHeader("accept", "application/json")
            .end();
  };
  
}