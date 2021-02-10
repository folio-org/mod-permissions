package org.folio.rest.impl;

import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.tools.utils.TenantLoading;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

public class TenantRefAPI extends TenantAPI {

  private static final Logger log = LogManager.getLogger(TenantRefAPI.class);
  private static final String TABLE_NAME_PERMS = "permissions";

  private String filter(String content) {
    JsonObject jInput = new JsonObject(content);
    JsonObject jOutput = new JsonObject();
    jOutput.put("userId", jInput.getString("id"));
    return jOutput.encodePrettily();
  }

  @Override
  Future<Integer> loadData(TenantAttributes attributes, String tenantId,
                           Map<String, String> headers, Context vertxContext) {
    return super.loadData(attributes, tenantId, headers, vertxContext)
        .compose(num -> deprecateSystemPerms(tenantId, vertxContext))
        .compose(num -> new TenantLoading()
            .withKey("loadSample").withLead("sample-data")
            .withPostOnly()
            .withFilter(this::filter)
            .withAcceptStatus(422)
            .add("users", "perms/users")
            .perform(attributes, headers, vertxContext, num));
  }

  private Future<Integer> deprecateSystemPerms(String tenantId, Context context) {
    Promise<Integer> promise = Promise.promise();
    String schema = tenantId + "_mod_permissions";
    String sql = String.format(
        "UPDATE %s.%s SET jsonb = jsonb || '{\"deprecated\": true}'::jsonb WHERE jsonb->'mutable' = 'false';",
        schema, TABLE_NAME_PERMS);
    PostgresClient.getInstance(context.owner(), tenantId).execute(sql, ar -> {
      if (ar.failed()) {
        log.error("Failed to mark permissions as deprecated", ar.cause());
        promise.fail(ar.cause());
        return;
      }
      log.info("Marked {} permissions deprecated.", ar.result().size());
      promise.complete(ar.result().size());
    });
    return promise.future();
  }

}
