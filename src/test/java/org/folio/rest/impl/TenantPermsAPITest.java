package org.folio.rest.impl;

import static io.restassured.RestAssured.given;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;

import java.util.UUID;
import io.restassured.RestAssured;
import io.restassured.response.ValidatableResponse;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;
import org.folio.permstest.TestUtil;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.PermissionListObject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(VertxExtension.class)
class TenantPermsAPITest {
  static String baseUrl;

  @BeforeAll
  static void setup(Vertx vertx, VertxTestContext vtc) {
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();

    TestUtil.setupDiku(vertx)
    .onComplete(vtc.succeeding(port -> {
      baseUrl = "http://localhost:" + port;
      vtc.completeNow();
    }));
  }

  @Test
  void updateVersion() {
    postTenantPermissions("""
        {
          "moduleId": "mod-update-1.0.0",
          "perms": [
            { "permissionName": "update.get" }
          ]
        }
        """)
    .statusCode(201);
    assertPerm("update.get", "mod-update");

    postTenantPermissions("""
        {
          "moduleId": "mod-update-2.0.0",
          "perms": [
            { "permissionName": "update.get" }
          ]
        }
        """)
    .statusCode(201);
    assertPerm("update.get", "mod-update");
  }

  @Test
  void permissionRename() {
    postTenantPermissions("""
        {
          "moduleId": "mod-foo-1.0.0",
          "perms": [
            { "permissionName": "foo.beg" }
          ]
        }
        """)
    .statusCode(201);
    assertPerm("foo.beg", "mod-foo");

    var userId = createUser("foo.beg");

    postTenantPermissions("""
        {
          "moduleId": "mod-foo-2.0.0",
          "perms": [
            { "permissionName": "foo.get", "replaces": ["foo.beg"] }
          ]
        }
        """)
    .statusCode(201);
    assertDeprecatedPerm("foo.beg", "mod-foo");
    assertPerm("foo.get", "mod-foo");

    getUser(userId)
    .body("permissions", containsInAnyOrder("foo.beg", "foo.get"));
  }

  @Test
  void split() {
    postTenantPermissions("""
        {
          "moduleId": "mod-split-1.0.0",
          "perms": [
            { "permissionName": "split.a" }
          ]
        }
        """)
    .statusCode(201);

    var userId = createUser("split.a");

    postTenantPermissions("""
        {
          "moduleId": "mod-split-2.0.0",
          "perms": [
            { "permissionName": "split.x", "replaces": ["split.a"] },
            { "permissionName": "split.y", "replaces": ["split.a"] }
          ]
        }
        """)
    .statusCode(201);

    assertDeprecatedPerm("split.a", "mod-split");
    assertPerm("split.x", "mod-split");
    assertPerm("split.y", "mod-split");

    getUser(userId)
    .body("permissions", containsInAnyOrder("split.a", "split.x", "split.y"));
}

  @Test
  void splitWithRetain() {
    postTenantPermissions("""
        {
          "moduleId": "mod-retain-1.0.0",
          "perms": [
            { "permissionName": "retain.a" }
          ]
        }
        """)
    .statusCode(201);

    var userId = createUser("retain.a");

    postTenantPermissions("""
        {
          "moduleId": "mod-retain-2.0.0",
          "perms": [
            { "permissionName": "retain.a" },
            { "permissionName": "retain.x", "replaces": ["retain.a"] },
            { "permissionName": "retain.y", "replaces": ["retain.a"] }
          ]
        }
        """)
    .statusCode(201);

    assertPerm("retain.a", "mod-retain");
    assertPerm("retain.x", "mod-retain");
    assertPerm("retain.y", "mod-retain");

    getUser(userId)
    .body("permissions", containsInAnyOrder("retain.a", "retain.x", "retain.y"));
  }

  Permission get1Perm(String permissionName) {
    var list = given()
        .header("X-Okapi-Tenant", "diku")
        .header("Content-type", "application/json")
        .get(baseUrl + "/perms/permissions?query=permissionName==" + permissionName)
        .then()
        .statusCode(200)
        .body("permissions.size()", is(1))
        .extract().as(PermissionListObject.class);
    return list.getPermissions().get(0);
  }

  ValidatableResponse postTenantPermissions(String body) {
    return given()
        .header("X-Okapi-Tenant", "diku")
        .header("Content-type", "application/json")
        .body(body)
        .post(baseUrl + "/_/tenantpermissions")
        .then();
  }

  ValidatableResponse getUser(UUID id) {
    return given()
        .header("X-Okapi-Tenant", "diku")
        .header("Content-type", "application/json")
        .get(baseUrl + "/perms/users/" + id)
        .then()
        .statusCode(200);
  }

  UUID createUser(String permissionName) {
    var uuid = UUID.randomUUID();
    given()
    .header("X-Okapi-Tenant", "diku")
    .header("Content-type", "application/json")
    .body("""
        {
          "id": "%s",
          "userId": "%s",
          "permissions": [ "%s" ]
        }
        """.formatted(uuid, uuid, permissionName))
    .post(baseUrl + "/perms/users")
    .then()
    .statusCode(201);
    return uuid;
  }

  void assertPerm(String permissionName, String moduleName) {
    var y = get1Perm(permissionName);
    assertThat(y.getPermissionName(), is(permissionName));
    assertThat(y.getModuleName(), is(moduleName));
    assertThat(y.getDeprecated(), is(false));
  }

  void assertDeprecatedPerm(String permissionName, String moduleName) {
    var y = get1Perm(permissionName);
    assertThat(y.getPermissionName(), is(permissionName));
    assertThat(y.getModuleName(), is(moduleName));
    assertThat(y.getDeprecated(), is(true));
  }
}
