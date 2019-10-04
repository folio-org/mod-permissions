package org.folio.rest.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criterion;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

/**
 * A simple cache to avoid frequent database calls to permissions table.
 *
 * @author hji
 *
 */
public class PermsCache {

  private static final Logger LOGGER = LoggerFactory.getLogger(PermsCache.class);

  public static final String CACHE_HEADER = "use.perms.cache";

  // 30 seconds cache
  private static final long CACHE_PERIOD = 30 * 1000L;

  private static final String TAB_PERMS = "permissions";

  // store cached data
  private static final ConcurrentMap<String, PermCache> CACHE = new ConcurrentHashMap<>();

  // cache update in progress
  private static final ConcurrentMap<String, Long> CACHE_WIP = new ConcurrentHashMap<>();

  private PermsCache() {
  }

  /**
   * Expand permission list to include all sub permissions recursively.
   *
   * @param perms
   * @param vertxContext
   * @param tenantId
   * @return
   */
  public static Future<List<String>> expandPerms(List<String> perms, Context vertxContext, String tenantId) {
    Future<List<String>> future = Future.future();
    getPermCache(vertxContext, tenantId).setHandler(ar -> {
      if (ar.succeeded()) {
        future.complete(ar.result().expandPerms(perms));
      } else {
        future.fail(ar.cause());
      }
    });
    return future;
  }

  /**
   * Return full {@link Permission} object for given permission name.
   *
   * @param permissionName
   * @param vertxContext
   * @param tenantId
   * @return
   */
  public static Future<Permission> getFullPerms(String permissionName, Context vertxContext, String tenantId) {
    Future<Permission> future = Future.future();
    getPermCache(vertxContext, tenantId).setHandler(ar -> {
      if (ar.succeeded()) {
        future.complete(ar.result().getFullPerm(permissionName));
      } else {
        future.fail(ar.cause());
      }
    });
    return future;
  }

  private static Future<PermCache> getPermCache(Context vertxContext, String tenantId) {
    PermCache permCache = CACHE.get(tenantId);
    if (permCache == null) {
      LOGGER.debug("Populate perms cache for tenant " + tenantId);
      return refreshCache(vertxContext, tenantId);
    }
    if (permCache.isStale() && !CACHE_WIP.containsKey(tenantId)) {
      CACHE_WIP.put(tenantId, vertxContext.owner().setTimer(1, v -> {
        LOGGER.debug("Refresh perms cache for tenant " + tenantId);
        refreshCache(vertxContext, tenantId);
      }));
    }
    return Future.succeededFuture(permCache);
  }

  private static Future<PermCache> refreshCache(Context vertxContext, String tenantId) {
    Future<PermCache> future = Future.future();
    PostgresClient.getInstance(vertxContext.owner(), tenantId).get(TAB_PERMS, Permission.class, new Criterion(), false,
        false, reply -> {
          if (reply.failed()) {
            future.fail("postgres client 'get' " + TAB_PERMS + " failed: " + reply.cause().getLocalizedMessage());
          } else {
            List<Permission> perms = reply.result().getResults();
            Map<String, Set<String>> subPermMap = new HashMap<>();
            Map<String, Permission> fullPermMap = new HashMap<>();
            PermCache pc = new PermCache(subPermMap, fullPermMap);
            for (Permission perm : perms) {
              fullPermMap.put(perm.getPermissionName(), perm);
              Set<String> subs = new HashSet<>();
              if (perm.getSubPermissions() != null && !perm.getSubPermissions().isEmpty()) {
                perm.getSubPermissions().forEach(e -> subs.add(e.toString()));
              }
              subPermMap.put(perm.getPermissionName(), subs);
            }
            CACHE.put(tenantId, pc);
            LOGGER.debug("Finished perms cache for tenant " + tenantId);
            future.complete(pc);
          }
          CACHE_WIP.remove(tenantId);
        });
    return future;
  }

  /**
   * Simple perm cache object.
   */
  public static class PermCache {

    private long timestamp = System.currentTimeMillis();
    private Map<String, Set<String>> subPermMap = new HashMap<>();
    private Map<String, Permission> fullPermMap = new HashMap<>();

    public PermCache(Map<String, Set<String>> subPermMap, Map<String, Permission> fullPermMap) {
      this.subPermMap = subPermMap;
      this.fullPermMap = fullPermMap;
    }

    @Override
    public String toString() {
      return "PermCache [timestamp=" + timestamp + ", subPermMap=" + subPermMap + ", fullPermMap=" + fullPermMap + "]";
    }

    public boolean isStale() {
      return System.currentTimeMillis() > (timestamp + CACHE_PERIOD);
    }

    public Permission getFullPerm(String permName) {
      return fullPermMap.get(permName);
    }

    public List<String> expandPerms(List<String> perms) {
      Set<String> allPerms = new HashSet<>();
      for (String perm : perms) {
        recurseSub(perm, allPerms);
      }
      return new ArrayList<>(allPerms);
    }

    private void recurseSub(String perm, Set<String> allPerms) {
      if (allPerms.contains(perm)) {
        return;
      }
      allPerms.add(perm);
      Set<String> subs = subPermMap.get(perm);
      if (subs != null) {
        for (String sub : subs) {
          recurseSub(sub, allPerms);
        }
      }
    }
  }
}
