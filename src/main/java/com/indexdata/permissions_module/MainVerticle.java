/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.indexdata.permissions_module;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;

/**
 *
 * @author kurt
 */
public class MainVerticle extends AbstractVerticle {
  private MongoClient mongoClient;
  private String authApiKey;
  
  public void start() {
    authApiKey = System.getProperty("auth.api.key", "VERY_WEAK_KEY");
    
    String mongoURL = System.getProperty("mongo.url", "mongodb://localhost:27017");
    mongoClient = MongoClient.createShared(vertx, new JsonObject().put("connection_string", mongoURL));
  }
}
