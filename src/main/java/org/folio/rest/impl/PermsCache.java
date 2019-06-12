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

import io.vertx.core.CompositeFuture;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
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

  private static final long CACHE_PERIOD = 30 * 1000;

  private static final String QUERY_PERMS = "select jsonb->>'permissionName' as p_name, jsonb->>'subPermissions' as p_sub from %s_mod_permissions.permissions";
  private static final String PERMS_PNAME = "p_name";
  private static final String PERMS_PSUB = "p_sub";

  private static final String QUERY_PG_STAT = "select n_tup_ins, n_tup_upd, n_tup_del from pg_stat_user_tables where schemaname = '%s_mod_permissions' and relname = 'permissions'";
  private static final String PG_STAT_INS = "n_tup_ins";
  private static final String PG_STAT_UPD = "n_tup_upd";
  private static final String PG_STAT_DEL = "n_tup_del";

  private static final ConcurrentMap<String, PermCache> CACHE = new ConcurrentHashMap<>();

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
    if (permCache == null) {
      LOGGER.info("Populate perms cache for tenant " + tenantId);
      Future<PgStat> f1 = queryPermsPgStat(vertxContext, tenantId);
      Future<Map<String, Set<String>>> f2 = queryAllPerms(vertxContext, tenantId);
      CompositeFuture.all(f1, f2).setHandler(ar -> {
        if (ar.succeeded()) {
          PermCache pc = new PermCache(f1.result(), f2.result());
          LOGGER.debug("new perms cache: " + pc);
          CACHE.put(tenantId, pc);
          future.complete(pc.expandPerms(perms));
        } else {
          future.fail(ar.cause());
        }
      });
    } else if (permCache.isStale()) {
      Future<PgStat> f1 = queryPermsPgStat(vertxContext, tenantId);
      f1.setHandler(ar -> {
        if (ar.succeeded()) {
          if (ar.result().equals(permCache.pgStat)) {
            LOGGER.debug("Reset perm cache timestamp for tenant " + tenantId);
            permCache.resetTimeStamp();
            future.complete(permCache.expandPerms(perms));
          } else {
            LOGGER.info("Update perms cache for tenant " + tenantId);
            Future<Map<String, Set<String>>> f2 = queryAllPerms(vertxContext, tenantId);
            f2.setHandler(ar2 -> {
              if (ar2.succeeded()) {
                PermCache pc = new PermCache(f1.result(), f2.result());
                LOGGER.debug("upated perms cache: " + pc);
                CACHE.put(tenantId, pc);
                future.complete(pc.expandPerms(perms));
              } else {
                future.fail(ar2.cause());
              }
            });
          }
        } else {
          future.fail(ar.cause());
        }
      });
    } else {
      future.complete(permCache.expandPerms(perms));
    }
    return future;
  }

  private static Future<PgStat> queryPermsPgStat(Context vertxContext, String tenantId) {
    Future<PgStat> future = Future.future();
    String query = String.format(QUERY_PG_STAT, tenantId.toLowerCase());
    PostgresClient pc = PostgresClient.getInstance(vertxContext.owner(), tenantId);
    pc.select(query, reply -> {
      if (reply.succeeded()) {
        JsonObject jo = reply.result().getRows().get(0);
        PgStat pgStat = new PgStat(jo.getInteger(PG_STAT_INS), jo.getInteger(PG_STAT_UPD), jo.getInteger(PG_STAT_DEL));
        future.complete(pgStat);
      } else {
        future.fail(reply.cause());
      }
    });
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
    private PgStat pgStat;
    private Map<String, Set<String>> subPermMap = new HashMap<>();

    public PermCache(PgStat pgStat, Map<String, Set<String>> subPermMap) {
      this.pgStat = pgStat;
      this.subPermMap = subPermMap;
    }

    @Override
    public String toString() {
      return "PermCache [timestamp=" + timestamp + ", pgStat=" + pgStat + ", subPermMap=" + subPermMap + "]";
    }

    public boolean isStale() {
      return System.currentTimeMillis() > (timestamp + CACHE_PERIOD);
    }

    public void resetTimeStamp() {
      this.timestamp = System.currentTimeMillis();
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

  /**
   * Simple class about PG_STAT insert, update, and delete info.
   *
   * @author hji
   *
   */
  public static class PgStat {
    private int ins;
    private int upd;
    private int del;

    public PgStat(int ins, int upd, int del) {
      this.ins = ins;
      this.upd = upd;
      this.del = del;
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + del;
      result = prime * result + ins;
      result = prime * result + upd;
      return result;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj)
        return true;
      if (obj == null)
        return false;
      if (getClass() != obj.getClass())
        return false;
      PgStat other = (PgStat) obj;
      if (del != other.del)
        return false;
      if (ins != other.ins)
        return false;
      if (upd != other.upd)
        return false;
      return true;
    }

    @Override
    public String toString() {
      return "PgStat [ins=" + ins + ", upd=" + upd + ", del=" + del + "]";
    }
  }

}
