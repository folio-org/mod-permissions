package org.folio.rest.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.folio.okapi.common.GenericCompositeFuture;
import org.folio.rest.jaxrs.model.OkapiPermission;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.PermissionNameListObject;
import org.folio.rest.persist.PostgresClient;
import io.vertx.core.Future;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;

public class PermissionUtils {

  private static final String SELECT_DEPRECATED_PERMS = "select id::text as id, jsonb->>'permissionName' as name "
      + "from %s_mod_permissions." + PermsAPI.TABLE_NAME_PERMS + " where jsonb->>'deprecated' = 'true'";
  private static final String PURGE_DEPRECATED_PERMS = "delete from %s_mod_permissions."
      + PermsAPI.TABLE_NAME_PERMS
      + " where jsonb->>'deprecated' = 'true'";
  private static final String PURGE_DEPRECATED_SUB_PERMS = "update %s_mod_permissions."
      + PermsAPI.TABLE_NAME_PERMS
      + " set jsonb = jsonb_set(jsonb, '{subPermissions}', (jsonb->'subPermissions')::jsonb - $1)"
      + " where jsonb->'subPermissions' ? $2";
  private static final String PURGE_DEPRECATED_PERMS_USERS = "update %s_mod_permissions."
      + PermsAPI.TABLE_NAME_PERMSUSERS
      + " set jsonb = jsonb_set(jsonb, '{permissions}', (jsonb->'permissions')::jsonb - $1)"
      + " where jsonb->'permissions' ? $2";

  public static final String PERMS_USERS_ASSIGN_IMMUTABLE = "perms.users.assign.immutable";
  public static final String PERMS_USERS_ASSIGN_MUTABLE = "perms.users.assign.mutable";
  public static final String PERMS_USERS_ASSIGN_OKAPI = "perms.users.assign.okapi";
  public static final String [] PERMS_MIGRATE = { "perms.users.item.post", "perms.users.item.put" };
  public static final String PERMS_OKAPI_ALL = "okapi.all";

  private PermissionUtils() {

  }

  /**
   * Returns true if the provided OkapiPermission and moduleName are equal to the values in the
   * provided Permission.
   *
   * @param okapiPerm a permission passed from Okapi
   * @param moduleName the module name to compare against permission's moduleName property
   * @param perm the Permission to compare against
   * @return true if equal, false otherwise
   */
  public static boolean equals(OkapiPermission okapiPerm, String moduleName, Permission perm) {
    String otherModuleName = perm == null ? null : perm.getModuleName();
    return equals(okapiPerm, perm) && Objects.equals(moduleName, otherModuleName);
  }

  /**
   * Returns true if the values in okapiPerm equal to the values in perm, or if both okapiPerm and perm are null.
   *
   * @param okapiPerm a permission passed from Okapi
   * @param perm the Permission to compare against
   * @return true if equal, false otherwise
   */
  public static boolean equals(OkapiPermission okapiPerm, Permission perm) {
    if (okapiPerm == null && perm == null) {
      return true;
    }
    if (okapiPerm == null || perm == null) {
      return false;
    }
    return Objects.equals(okapiPerm.getPermissionName(), perm.getPermissionName())
        && Objects.equals(okapiPerm.getDescription(), perm.getDescription())
        && Objects.equals(okapiPerm.getDisplayName(), perm.getDisplayName())
        && Objects.equals(okapiPerm.getSubPermissions(), perm.getSubPermissions());
  }

  /**
   * Purge deprecated permissions for a given tenant.
   *
   * @param pgClient {@link PostgresClient}
   * @param tenantId tenant id
   * @return {@link Future} with {@link PermissionNameListObject}
   */
  public static Future<PermissionNameListObject> purgeDeprecatedPermissions(PostgresClient pgClient, String tenantId) {
    return pgClient.withTrans(connection ->
        connection.execute(String.format(SELECT_DEPRECATED_PERMS, tenantId)).compose(result -> {
          PermissionNameListObject permNames = new PermissionNameListObject();
          permNames.setTotalRecords(0);
          List<Future<RowSet<Row>>> futures = new ArrayList<Future<RowSet<Row>>>();
          result
              .forEach(row -> {
                String name = row.getString("name");
                futures.add(connection.execute(String.format(PURGE_DEPRECATED_SUB_PERMS, tenantId), Tuple.of(name, name)));
                futures.add(connection.execute(String.format(PURGE_DEPRECATED_PERMS_USERS, tenantId), Tuple.of(name, name)));
                futures.add(connection.execute(String.format(PURGE_DEPRECATED_PERMS, tenantId)));
                permNames.getPermissionNames().add(name);
                permNames.setTotalRecords(permNames.getTotalRecords() + 1);
              });
          return GenericCompositeFuture.all(futures)
              .map(done -> permNames);
        }));
  }
}
