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
  Future<Boolean> addPermission(String permission);
  Future<Boolean> addSubPermission(String permission, String sub);
  Future<Boolean> removePermission(String permission);
  Future<Boolean> removeSubPermission(String permission, String sub);
  Future<Boolean> addUser(String user);
  Future<Boolean> removeUser(String user);
  Future<Boolean> addPermissionToUser(String user, String permission);
  Future<Boolean> removePermissionFromUser(String user, String permission);
  Future<JsonArray> getPermissionsForUser(String user);
  Future<JsonArray> getSubPermissions(String permission);
  Future<JsonArray> getExpandedPermissions(String permission);
}