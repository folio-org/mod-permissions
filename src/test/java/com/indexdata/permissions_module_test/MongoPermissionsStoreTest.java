/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.indexdata.permissions_module_test;

import com.indexdata.permissions_module.impl.MongoPermissionsStore;
import de.flapdoodle.embed.mongo.MongodExecutable;
import de.flapdoodle.embed.mongo.MongodProcess;
import de.flapdoodle.embed.mongo.MongodStarter;
import de.flapdoodle.embed.mongo.config.IMongodConfig;
import de.flapdoodle.embed.mongo.config.MongodConfigBuilder;
import de.flapdoodle.embed.mongo.config.Net;
import de.flapdoodle.embed.mongo.distribution.Version;
import de.flapdoodle.embed.process.runtime.Network;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.unit.Async;
import io.vertx.ext.unit.TestContext;
import io.vertx.ext.unit.junit.RunTestOnContext;
import io.vertx.ext.unit.junit.VertxUnitRunner;
import java.io.IOException;
import java.util.ArrayList;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import java.util.ArrayList;
import org.junit.Rule;
import org.junit.Test;

/**
 *
 * @author kurt
 */
@RunWith(VertxUnitRunner.class)
public class MongoPermissionsStoreTest {
  private MongoPermissionsStore store;
  private MongoClient mongoClient;
  private Vertx vertx;
  //private static MongodProcess MONGO;
  //private static int MONGO_PORT = 12345;
  
  @Rule
  public RunTestOnContext rule = new RunTestOnContext();
  
  @BeforeClass()
  public static void initialize(TestContext context) throws IOException {
    final Async async = context.async();
    async.complete();    
  }

  @AfterClass
  public static void shutDown() {
    //MONGO.stop();
  }
  
  @Before
  public void setUp(TestContext context) throws IOException {
    final Async async = context.async();
    
    int mongoPort = Network.getFreeServerPort();
    MongodStarter starter = MongodStarter.getDefaultInstance();
    IMongodConfig mongodConfig = new MongodConfigBuilder()
            .version(Version.Main.PRODUCTION)
            .net(new Net(mongoPort, Network.localhostIsIPv6()))
            .build();
    MongodExecutable mongodExecutable = starter.prepare(mongodConfig);
    MongodProcess mongoD = mongodExecutable.start();
    
    JsonObject mongoConfig = new JsonObject();
    String host = "localhost";
    mongoConfig.put("connection_string", "mongodb://localhost:" + mongoPort);
    mongoConfig.put("db_name", "test_db");
    vertx = rule.vertx();
    mongoClient = MongoClient.createShared(vertx, mongoConfig);
    
    store = new MongoPermissionsStore(mongoClient);
    ArrayList<JsonObject> permissionList = new ArrayList<>();
    ArrayList<JsonObject> userList = new ArrayList<>();
    permissionList.add(new JsonObject()
            .put("permission_name", "foo.secret")
            .put("sub_permissions", new JsonArray()));
    permissionList.add(new JsonObject()
            .put("permission_name", "bar.secret")
            .put("sub_permissions", new JsonArray()));
    permissionList.add(new JsonObject()
            .put("permission_name", "blip.secret")
            .put("sub_permissions", new JsonArray()));
    permissionList.add(new JsonObject()
            .put("permission_name", "bloop.secret")
            .put("sub_permissions", new JsonArray()));
    permissionList.add(new JsonObject()
            .put("permission_name", "flop.secret")
            .put("sub_permissions", new JsonArray()));
    permissionList.add(new JsonObject()
            .put("permission_name", "foobar")
            .put("sub_permissions", new JsonArray()
              .add("foo.secret")
              .add("bar.secret")));
    permissionList.add(new JsonObject()
        .put("permission_name", "blipbloop")
        .put("sub_permissions", new JsonArray()
          .add("blip.secret")
          .add("bloop.secret")));
    permissionList.add(new JsonObject()
            .put("permission_name", "master")
            .put("sub_permissions", new JsonArray()
              .add("foobar")
              .add("blipbloop")
              .add("flop.secret")));    
    userList.add(new JsonObject().put("user_name", "sonic")
            .put("user_permissions", new JsonArray().add("foo.secret").add("bar.secret")));
    userList.add(new JsonObject().put("user_name", "knuckles")
            .put("user_permissions", new JsonArray().add("bar.secret")));
    userList.add(new JsonObject().put("user_name", "tails")
            .put("user_permissions", new JsonArray()));
    userList.add(new JsonObject().put("user_name", "eggman")
            .put("user_permissions", new JsonArray().add("master")));
    ArrayList<Future> futureList = new ArrayList<>();
    for(JsonObject permObj : permissionList) {
      Future<Void> insertFuture = Future.future();
      futureList.add(insertFuture);
      mongoClient.insert("permissions", permObj, res -> {
        if(res.succeeded()) {
          insertFuture.complete();
        } else {
          insertFuture.fail(res.result());
        }
      });
    }
    for(JsonObject userObj : userList) {
      Future<Void> insertFuture = Future.future();
      futureList.add(insertFuture);
      mongoClient.insert("users", userObj, res-> {
        if( res.succeeded()) { insertFuture.complete(); }
        else { insertFuture.fail(res.result()); }
      });
    }
    CompositeFuture allInsertsFuture = CompositeFuture.all(futureList);
    allInsertsFuture.setHandler(res -> {
      if(res.succeeded()) {
        async.complete();
      } else {
        context.fail();
      }
    });
  }
  
  @Test
  public void basicPermissionTest(TestContext context) {
    final Async async = context.async();
    store.getExpandedPermissions("master").setHandler(res -> {
      if(!res.succeeded()) {
        context.fail();
      } else {
        JsonArray result = res.result();
        String[] permCheck = { "foo.secret", "blip.secret", "flop.secret", "master", "foobar" };
        for(String perm : permCheck) {
          //context.assertTrue(result.contains(perm));
          
          if(!result.contains(perm)) {
            context.fail("Result array does not contain '" + perm + "'");
          }
          
        }
        async.complete();
      }
    });
  }
  
  @Test
  public void deleteSubPermissionTest(TestContext context) {
    final Async async = context.async();
    store.removeSubPermission("foobar", "foo.secret").setHandler(res -> {
      if(!res.succeeded()) {
        context.fail();
      } else {
        store.getExpandedPermissions("foobar").setHandler(res2 -> {
          if(!res2.succeeded()) {
            context.fail();
          } else {
            JsonArray result = res2.result();
            context.assertFalse(result.contains("foo.secret"));
            async.complete();
          }
        });
      }
    });
  }
  
  @Test
  public void deletePermissionTest(TestContext context) {
    final Async async = context.async();
    store.removePermission("foo.secret").setHandler(res -> {
      if(!res.succeeded()) {
        context.fail();
      } else {
        store.getExpandedPermissions("master").setHandler(res2 -> {
          if(!res2.succeeded()) {
            context.fail();
          } else {
            JsonArray result = res2.result();
            context.assertFalse(result.contains("foo.secret"));
            async.complete();
          }
        });
      }
    });
  }
  
  @Test
  public void deleteUserPermissionTest(TestContext context) {
    final Async async = context.async();
    store.getPermissionsForUser("sonic").setHandler(res -> {
      if(!res.succeeded()) { context.fail(); }
      else {
        context.assertTrue(res.result().contains("foo.secret"));
        store.removePermissionFromUser("sonic", "foo.secret").setHandler(res2 -> {
          if(!res2.succeeded()) { context.fail(); }
          else {
            store.getPermissionsForUser("sonic").setHandler(res3 -> {
              if(!res3.succeeded()) { context.fail(); }
              else {
                context.assertFalse(res3.result().contains("foo.secret"));
                async.complete();
              }
            });
          }
        });
      }
    });
  }
}