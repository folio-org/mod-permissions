package org.folio.rest.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.folio.rest.persist.PostgresClient;

import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

/**
 * A simple cache to avoid frequent database calls to get sub permissions.
 *
 * @author hji
 *
 */
public class PermsCache {

  private static final Logger LOGGER = LoggerFactory.getLogger(PermsCache.class);

  public static final String CACHE_HEADER = "use.perms.cache";

  private static final long CACHE_PERIOD = 30 * 1000L;

  private static final String QUERY_PERMS = "select jsonb->>'permissionName' as p_name, jsonb->>'subPermissions' as p_sub from %s_mod_permissions.permissions";
  private static final String PERMS_PNAME = "p_name";
  private static final String PERMS_PSUB = "p_sub";

  private static final ConcurrentMap<String, PermCache> CACHE = new ConcurrentHashMap<>();

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
    PermCache permCache = CACHE.get(tenantId);
    if (permCache == null || permCache.isStale()) {
      LOGGER.info("Populate perms cache for tenant " + tenantId);
      Future<Map<String, Set<String>>> f = queryAllPerms(vertxContext, tenantId);
      f.setHandler(ar -> {
        if (ar.succeeded()) {
          PermCache pc = new PermCache(f.result());
          LOGGER.debug("new perms cache: " + pc);
          CACHE.put(tenantId, pc);
          future.complete(pc.expandPerms(perms));
        } else {
          future.fail(ar.cause());
        }
      });
    } else {
      future.complete(permCache.expandPerms(perms));
    }
    return future;
  }

  private static Future<Map<String, Set<String>>> queryAllPerms(Context vertxContext, String tenantId) {
    Future<Map<String, Set<String>>> future = Future.future();
    String query = String.format(QUERY_PERMS, tenantId.toLowerCase());
    PostgresClient pc = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    Map<String, Set<String>> perms = new HashMap<>();
    pc.select(query, reply -> {
      if (reply.succeeded()) {
        reply.result().getRows().forEach(jo -> {
          String pname = jo.getString(PERMS_PNAME);
          String psub = jo.getString(PERMS_PSUB);
          Set<String> subs = new HashSet<>();
          if (psub != null && !psub.trim().isEmpty()) {
            new JsonArray(psub).forEach(e -> subs.add(e.toString()));
          }
          perms.put(pname, subs);
        });
        future.complete(perms);
      } else {
        future.fail(reply.cause());
      }
    });
    return future;
  }

  /**
   * Simple class about permissions cache object.
   *
   * @author hji
   *
   */
  public static class PermCache {

    private long timestamp = System.currentTimeMillis();
    private Map<String, Set<String>> subPermMap = new HashMap<>();

    public PermCache(Map<String, Set<String>> subPermMap) {
      this.subPermMap = subPermMap;
    }

    @Override
    public String toString() {
      return "PermCache [timestamp=" + timestamp + ", subPermMap=" + subPermMap + "]";
    }

    public boolean isStale() {
      return System.currentTimeMillis() > (timestamp + CACHE_PERIOD);
    }

    public List<String> expandPerms(List<String> perms) {
      Set<String> allPerms = new HashSet<>();
      for (String perm : perms) {
        allPerms.addAll(recurseSub(perm));
      }
      return new ArrayList<>(allPerms);
    }

    private Set<String> recurseSub(String perm) {
      Set<String> perms = new HashSet<>();
      perms.add(perm);
      Set<String> subs = subPermMap.get(perm);
      if (subs != null) {
        for (String sub : subs) {
          perms.addAll(recurseSub(sub));
        }
      }
      return perms;
    }
  }
}
