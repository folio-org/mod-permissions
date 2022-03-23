package org.folio.rest.impl;

import java.util.List;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.folio.rest.jaxrs.model.Permission;
import org.folio.rest.jaxrs.model.TenantAttributes;
import org.folio.rest.persist.Conn;
import org.folio.rest.persist.PostgresClient;
import org.folio.rest.persist.Criteria.Criteria;
import org.folio.rest.persist.Criteria.Criterion;
import org.folio.rest.tools.utils.TenantLoading;
import io.vertx.core.Context;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class TenantRefAPI extends TenantAPI {

  private static final Logger log = LogManager.getLogger(TenantRefAPI.class);
  private static final String TABLE_NAME_PERMS = "permissions";
  private static final String MUTABLE_FIELD = "'mutable'";

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
        .compose(num -> deprecateSystemPerms(tenantId, vertxContext, num))
        .compose(num -> new TenantLoading()
            .withKey("loadSample").withLead("sample-data")
            .withPostOnly()
            .withFilter(this::filter)
            .withAcceptStatus(400)
            .add("users", "perms/users")
            .perform(attributes, headers, vertxContext, num));
  }

  private Future<Integer> deprecateSystemPerms(String tenantId, Context context, Integer num) {

    PostgresClient pgClient = PostgresClient.getInstance(context.owner(), tenantId);
    return pgClient.withTrans(connection ->
        getSystemPerms(connection)
            .compose(perms -> {
              TenantPermsAPI tenantPermsApi = new TenantPermsAPI();
              return tenantPermsApi.softDeletePermList(perms, connection).map(num);
            })
    );
  }

  protected Future<List<Permission>> getSystemPerms(Conn connection) {
    Criteria crit = new Criteria();
    crit.addField(MUTABLE_FIELD);
    crit.setOperation("=");
    crit.setVal("false");

    return connection.get(TABLE_NAME_PERMS, Permission.class, new Criterion(crit), false)
        .map(result -> result.getResults());
  }
}
