/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.folio.auth.permissions_module;

import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;

/**
 *
 * @author kurt
 */
public interface PermissionsStore {
  Future<Boolean> addPermission(String permission, String tenant);
  Future<Boolean> addSubPermission(String permission, String sub, String tenant);
  Future<Boolean> removePermission(String permission, String tenant);
  Future<Boolean> removeSubPermission(String permission, String sub, String tenant);
  Future<Boolean> addUser(String user, String tenant);
  Future<Boolean> removeUser(String user, String tenant);
  Future<Boolean> addPermissionToUser(String user, String permission, String tenant);
  Future<Boolean> removePermissionFromUser(String user, String permission, String tenant);
  Future<JsonArray> getPermissionsForUser(String user, String tenant);
  Future<JsonArray> getSubPermissions(String permission, String tenant);
  Future<JsonArray> getExpandedPermissions(String permission, String tenant);
}