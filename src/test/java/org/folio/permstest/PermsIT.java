package org.folio.permstest;

import static io.restassured.RestAssured.given;
import static io.restassured.RestAssured.when;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import io.restassured.RestAssured;
import io.restassured.builder.RequestSpecBuilder;
import io.restassured.http.ContentType;
import io.vertx.core.json.JsonObject;
import java.io.IOException;
import java.nio.file.Path;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.BindMode;
import org.testcontainers.containers.Container.ExecResult;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.images.builder.ImageFromDockerfile;

/**
 * Test that shaded fat uber jar and Dockerfile work.
 *
 * <p>Smoke tests: /admin/health and migration.
 */
public class PermsIT {

  private static final Logger LOG = LoggerFactory.getLogger(PermsIT.class);
  private static final Network NETWORK = Network.newNetwork();

  @ClassRule
  public static final GenericContainer<?> MOD_PERMISSIONS =
    new GenericContainer<>(
      new ImageFromDockerfile("mod-permissions").withFileFromPath(".", Path.of(".")))
    .withNetwork(NETWORK)
    .withExposedPorts(8081)
    .withEnv("DB_HOST", "postgres")
    .withEnv("DB_PORT", "5432")
    .withEnv("DB_USERNAME", "username")
    .withEnv("DB_PASSWORD", "password")
    .withEnv("DB_DATABASE", "postgres");

  @ClassRule
  public static final PostgreSQLContainer<?> POSTGRES =
    new PostgreSQLContainer<>("postgres:12-alpine")
    .withClasspathResourceMapping("v5.14.4.sql", "/v5.14.4.sql", BindMode.READ_ONLY)
    .withNetwork(NETWORK)
    .withNetworkAliases("postgres")
    .withExposedPorts(5432)
    .withUsername("username")
    .withPassword("password")
    .withDatabaseName("postgres");

  @BeforeClass
  public static void beforeClass() {
    RestAssured.reset();
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();
    RestAssured.baseURI = "http://" + MOD_PERMISSIONS.getHost() + ":" + MOD_PERMISSIONS.getFirstMappedPort();

    MOD_PERMISSIONS.followOutput(new Slf4jLogConsumer(LOG).withSeparateOutputStreams());
  }

  @Test
  public void health() {
    when().
      get("/admin/health").
    then().
      statusCode(200).
      body(is("\"OK\""));
  }

  private void tenant(String tenant) {
    RestAssured.requestSpecification = new RequestSpecBuilder()
        .addHeader("X-Okapi-Url", RestAssured.baseURI)
        .addHeader("X-Okapi-Tenant", tenant)
      .setContentType(ContentType.JSON)
      .build();
  }

  private void postTenant(JsonObject body) {
    String location =
        given().
          body(body.encodePrettily()).
        when().
          post("/_/tenant").
        then().
          statusCode(201).
        extract().
          header("Location");

    when().
      get(location + "?wait=30000").
    then().
      statusCode(200).
      body("complete", is(true));
  }

  @Test
  public void installAndUpgrade() {
    tenant("latest");
    postTenant(new JsonObject().put("module_to", "999999.0.0"));
    // migrate from 0.0.0, migration should be idempotent
    postTenant(new JsonObject().put("module_to", "999999.0.0").put("module_from", "0.0.0"));

    String id = "12345678-0123-4567-890a-bcdef0123456";
    String userId = "57a0cab5-1e07-488d-b72e-a73084281a85";
    given().
      body(new JsonObject().put("id", id).put("userId", userId).encode()).
    when().
      post("/perms/users").
    then().
      statusCode(201);

    when().
      get("/perms/users/" + id).
    then().
      statusCode(200).
      body("userId", is(userId));
  }

  @Test
  public void upgradeFromKiwi() {
    postgresExec("psql", "-U", POSTGRES.getUsername(), "-d", POSTGRES.getDatabaseName(),
        "-f", "v5.14.4.sql");

    tenant("kiwi");

    // record without userId the migration should delete: https://issues.folio.org/browse/MODPERMS-177
    when().
      get("/perms/users/eb0f05ab-88f6-4d95-9a54-b1812fe36e90").
    then().
      statusCode(200).
      body("userId", is(nullValue()));

    // migrate
    postTenant(new JsonObject().put("module_to", "999999.0.0").put("module_from", "5.14.4"));

    // migration should have deleted
    when().
      get("/perms/users/eb0f05ab-88f6-4d95-9a54-b1812fe36e90").
    then().
      statusCode(404);

    // migration should have kept
    when().
      get("/perms/users/9b283803-dfca-4d3f-8410-2a8a64ce0033").
    then().
      statusCode(200).
      body("userId", is("956f39c5-92e3-4c26-bcdc-1827674710cf"));

    String id = "12345678-0123-4567-890a-bcdef0123456";
    String userId = "6e483d97-9c98-4dce-b953-64d708d4c760";
    given().
      body(new JsonObject().put("id", id).put("userId", userId).encode()).
    when().
      post("/perms/users").
    then().
      statusCode(201);

    when().
      get("/perms/users/" + id).
    then().
      statusCode(200).
      body("userId", is(userId));
  }

  static void postgresExec(String... command) {
    try {
      ExecResult execResult = POSTGRES.execInContainer(command);
      LOG.info(String.join(" ", command) + " " + execResult);
    } catch (InterruptedException | IOException | UnsupportedOperationException e) {
      throw new RuntimeException(e);
    }
  }

}
