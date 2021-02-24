package org.folio.rest.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.folio.rest.jaxrs.model.OkapiPermission;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.PermissionNameListObject;
import org.folio.rest.persist.PostgresClient;
import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowSet;

public class PermissionUtils {

  private static final String SELECT_DEPRECATED_PERMS = "select id::text as id, jsonb->>'permissionName' as name "
      + "from %s_mod_permissions.permissions where jsonb->>'deprecated' = 'true'";
  private static final String PURGE_DEPRECATED_PERMS = "delete from %s_mod_permissions.permissions "
      + "where jsonb->>'deprecated' = 'true'";
  private static final String PURGE_DEPRECATED_SUB_PERMS = "update %s_mod_permissions.permissions "
      + "set jsonb = jsonb_set(jsonb, '{subPermissions}', (jsonb->'subPermissions')::jsonb - array['%s', '%s']) "
      + "where jsonb->'subPermissions' ?| array['%s', '%s']";
  private static final String PURGE_DEPRECATED_PERMS_USERS = "update %s_mod_permissions.permissions_users "
      + "set jsonb = jsonb_set(jsonb, '{permissions}', (jsonb->'permissions')::jsonb - array['%s', '%s']) "
      + "where jsonb->'permissions' ?| array['%s', '%s']";

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
    Promise<PermissionNameListObject> promise = Promise.promise();
    pgClient.startTx(tx -> {
      try {
        pgClient.select(tx, String.format(SELECT_DEPRECATED_PERMS, tenantId), res -> {
          if (res.failed()) {
            pgClient.rollbackTx(tx, done -> promise.fail(res.cause()));
            return;
          }
          PermissionNameListObject permNames = new PermissionNameListObject();
          permNames.setTotalRecords(0);
          @SuppressWarnings("rawtypes")
          List<Future> futures = new ArrayList<Future>();
          res.result()
            .forEach(row -> {
              String id = row.getString("id");
              String name = row.getString("name");
              futures.add(Future.<RowSet<Row>>future(p -> pgClient.execute(tx, String.format(PURGE_DEPRECATED_SUB_PERMS, tenantId, id, name, id, name), p)));
              futures.add(Future.<RowSet<Row>>future(p -> pgClient.execute(tx, String.format(PURGE_DEPRECATED_PERMS_USERS, tenantId, id, name, id, name), p)));
              futures.add(Future.<RowSet<Row>>future(p -> pgClient.execute(tx, String.format(PURGE_DEPRECATED_PERMS, tenantId), p)));
              permNames.getPermissionNames().add(name);
              permNames.setTotalRecords(permNames.getTotalRecords() + 1);
            });
          CompositeFuture.all(futures)
            .onSuccess(ignore -> {
              pgClient.endTx(tx, done -> promise.complete(permNames));
            })
            .onFailure(ex -> {
              pgClient.rollbackTx(tx, done -> promise.fail(ex));
            });
        });
      } catch (Exception e) {
        pgClient.rollbackTx(tx, done -> promise.fail(e));
      }
    });
    return promise.future();
  }
}
