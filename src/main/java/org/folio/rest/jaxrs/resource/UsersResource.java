
package org.folio.rest.jaxrs.resource;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.StreamingOutput;
import io.vertx.core.Context;
import org.folio.rest.annotations.Validate;
import org.folio.rest.jaxrs.model.User;
import org.folio.rest.jaxrs.model.UserdataCollection;


/**
 * Collection of user items.
 * 
 */
@Path("users")
public interface UsersResource {


    /**
     * Return a list of users
     * 
     * @param offset
     *     Skip over a number of elements by specifying an offset value for the query e.g. 0
     * @param query
     *     JSON array [{"field1","value1","operator1"},{"field2","value2","operator2"},...,{"fieldN","valueN","operatorN"}] with valid searchable fields
     *      e.g. ["user.active", "true", "="]
     *     
     * @param limit
     *     Limit the number of elements returned in the response e.g. 10
     * @param vertxContext
     *      The Vertx Context Object <code>io.vertx.core.Context</code> 
     * @param orderBy
     *     Order by field: field A, field B
     *     
     * @param asyncResultHandler
     *     A <code>Handler<AsyncResult<Response>>></code> handler {@link io.vertx.core.Handler} which must be called as follows - Note the 'GetPatronsResponse' should be replaced with '[nameOfYourFunction]Response': (example only) <code>asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetPatronsResponse.withJsonOK( new ObjectMapper().readValue(reply.result().body().toString(), Patron.class))));</code> in the final callback (most internal callback) of the function.
     * @param lang
     *     Requested language. Optional. [lang=en]
     *     
     * @param order
     *     Order
     */
    @GET
    @Produces({
        "application/json",
        "text/plain"
    })
    @Validate
    void getUsers(
        @QueryParam("query")
        String query,
        @QueryParam("orderBy")
        String orderBy,
        @QueryParam("order")
        @DefaultValue("desc")
        UsersResource.Order order,
        @QueryParam("offset")
        @DefaultValue("0")
        @Min(0L)
        @Max(1000L)
        int offset,
        @QueryParam("limit")
        @DefaultValue("10")
        @Min(1L)
        @Max(100L)
        int limit,
        @QueryParam("lang")
        @DefaultValue("en")
        @Pattern(regexp = "[a-zA-Z]{2}")
        String lang, io.vertx.core.Handler<io.vertx.core.AsyncResult<Response>>asyncResultHandler, Context vertxContext)
        throws Exception
    ;

    /**
     * Create a user
     * 
     * @param vertxContext
     *      The Vertx Context Object <code>io.vertx.core.Context</code> 
     * @param asyncResultHandler
     *     A <code>Handler<AsyncResult<Response>>></code> handler {@link io.vertx.core.Handler} which must be called as follows - Note the 'GetPatronsResponse' should be replaced with '[nameOfYourFunction]Response': (example only) <code>asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetPatronsResponse.withJsonOK( new ObjectMapper().readValue(reply.result().body().toString(), Patron.class))));</code> in the final callback (most internal callback) of the function.
     * @param lang
     *     Requested language. Optional. [lang=en]
     *     
     * @param entity
     *      e.g. {
     *         "username" : "jhandey",
     *             "tenant" : "biglibrary",
     *         "id" : "7261ecaae3a74dc68b468e12a70b1aec",
     *         "active" : true,
     *         "meta" : {
     *             "creation_date" : "2016-11-05T0723",
     *             "last_login_date" : ""
     *         },
     *         "personal" : {
     *             "full_name" : "Jack Michael Handey",
     *             "birth_date" : "1965-07-08",
     *             "phone_home" : "+1 (212) 456-7891",
     *             "phone_work" : "+1 (212) 567-8912",
     *             "phone_mobile" : "+1 (212) 678-9123",
     *             "email_primary" : "jhandey@biglibrary.org",
     *             "email_alternate" : "handyjack65@gmail.com",
     *             "mailing_address" : "4567 Langford Ave, Manhattan, NY 11025"
     *         }
     *             
     *     }
     *     
     *     
     */
    @POST
    @Consumes("application/json")
    @Produces({
        "application/json",
        "text/plain"
    })
    @Validate
    void postUsers(
        @QueryParam("lang")
        @DefaultValue("en")
        @Pattern(regexp = "[a-zA-Z]{2}")
        String lang, User entity, io.vertx.core.Handler<io.vertx.core.AsyncResult<Response>>asyncResultHandler, Context vertxContext)
        throws Exception
    ;

    /**
     * Get a single user
     * 
     * @param vertxContext
     *      The Vertx Context Object <code>io.vertx.core.Context</code> 
     * @param asyncResultHandler
     *     A <code>Handler<AsyncResult<Response>>></code> handler {@link io.vertx.core.Handler} which must be called as follows - Note the 'GetPatronsResponse' should be replaced with '[nameOfYourFunction]Response': (example only) <code>asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetPatronsResponse.withJsonOK( new ObjectMapper().readValue(reply.result().body().toString(), Patron.class))));</code> in the final callback (most internal callback) of the function.
     * @param lang
     *     Requested language. Optional. [lang=en]
     *     
     * @param userId
     *     
     */
    @GET
    @Path("{userId}")
    @Produces({
        "application/json",
        "text/plain"
    })
    @Validate
    void getUsersByUserId(
        @PathParam("userId")
        @NotNull
        String userId,
        @QueryParam("lang")
        @DefaultValue("en")
        @Pattern(regexp = "[a-zA-Z]{2}")
        String lang, io.vertx.core.Handler<io.vertx.core.AsyncResult<Response>>asyncResultHandler, Context vertxContext)
        throws Exception
    ;

    /**
     * Delete user item with given {userId}
     * 
     * 
     * @param vertxContext
     *      The Vertx Context Object <code>io.vertx.core.Context</code> 
     * @param asyncResultHandler
     *     A <code>Handler<AsyncResult<Response>>></code> handler {@link io.vertx.core.Handler} which must be called as follows - Note the 'GetPatronsResponse' should be replaced with '[nameOfYourFunction]Response': (example only) <code>asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetPatronsResponse.withJsonOK( new ObjectMapper().readValue(reply.result().body().toString(), Patron.class))));</code> in the final callback (most internal callback) of the function.
     * @param lang
     *     Requested language. Optional. [lang=en]
     *     
     * @param userId
     *     
     */
    @DELETE
    @Path("{userId}")
    @Produces({
        "text/plain"
    })
    @Validate
    void deleteUsersByUserId(
        @PathParam("userId")
        @NotNull
        String userId,
        @QueryParam("lang")
        @DefaultValue("en")
        @Pattern(regexp = "[a-zA-Z]{2}")
        String lang, io.vertx.core.Handler<io.vertx.core.AsyncResult<Response>>asyncResultHandler, Context vertxContext)
        throws Exception
    ;

    /**
     * Update user item with given {userId}
     * 
     * 
     * @param vertxContext
     *      The Vertx Context Object <code>io.vertx.core.Context</code> 
     * @param asyncResultHandler
     *     A <code>Handler<AsyncResult<Response>>></code> handler {@link io.vertx.core.Handler} which must be called as follows - Note the 'GetPatronsResponse' should be replaced with '[nameOfYourFunction]Response': (example only) <code>asyncResultHandler.handle(io.vertx.core.Future.succeededFuture(GetPatronsResponse.withJsonOK( new ObjectMapper().readValue(reply.result().body().toString(), Patron.class))));</code> in the final callback (most internal callback) of the function.
     * @param lang
     *     Requested language. Optional. [lang=en]
     *     
     * @param userId
     *     
     * @param entity
     *      e.g. {
     *         "username" : "jhandey",
     *             "tenant" : "biglibrary",
     *         "id" : "7261ecaae3a74dc68b468e12a70b1aec",
     *         "active" : true,
     *         "meta" : {
     *             "creation_date" : "2016-11-05T0723",
     *             "last_login_date" : ""
     *         },
     *         "personal" : {
     *             "full_name" : "Jack Michael Handey",
     *             "birth_date" : "1965-07-08",
     *             "phone_home" : "+1 (212) 456-7891",
     *             "phone_work" : "+1 (212) 567-8912",
     *             "phone_mobile" : "+1 (212) 678-9123",
     *             "email_primary" : "jhandey@biglibrary.org",
     *             "email_alternate" : "handyjack65@gmail.com",
     *             "mailing_address" : "4567 Langford Ave, Manhattan, NY 11025"
     *         }
     *             
     *     }
     *     
     *     
     */
    @PUT
    @Path("{userId}")
    @Consumes("application/json")
    @Produces({
        "text/plain"
    })
    @Validate
    void putUsersByUserId(
        @PathParam("userId")
        @NotNull
        String userId,
        @QueryParam("lang")
        @DefaultValue("en")
        @Pattern(regexp = "[a-zA-Z]{2}")
        String lang, User entity, io.vertx.core.Handler<io.vertx.core.AsyncResult<Response>>asyncResultHandler, Context vertxContext)
        throws Exception
    ;

    public class DeleteUsersByUserIdResponse
        extends org.folio.rest.jaxrs.resource.support.ResponseWrapper
    {


        private DeleteUsersByUserIdResponse(Response delegate) {
            super(delegate);
        }

        /**
         * Item deleted successfully
         * 
         */
        public static UsersResource.DeleteUsersByUserIdResponse withNoContent() {
            Response.ResponseBuilder responseBuilder = Response.status(204);
            return new UsersResource.DeleteUsersByUserIdResponse(responseBuilder.build());
        }

        /**
         * Item with a given ID not found e.g. "user not found"
         * 
         * 
         * @param entity
         *     "user not found"
         *     
         */
        public static UsersResource.DeleteUsersByUserIdResponse withPlainNotFound(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(404).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.DeleteUsersByUserIdResponse(responseBuilder.build());
        }

        /**
         * Bad request, e.g malformed request body or query parameter. Details of the error (e.g name of the parameter or line/character number with malformed data) provided in the response. e.g. "unable to delete user -- constraint violation"
         * 
         * 
         * @param entity
         *     "unable to delete user -- constraint violation"
         *     
         */
        public static UsersResource.DeleteUsersByUserIdResponse withPlainBadRequest(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(400).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.DeleteUsersByUserIdResponse(responseBuilder.build());
        }

        /**
         * Internal server error, e.g. due to misconfiguration e.g. Internal server error, contact administrator
         * 
         * @param entity
         *     Internal server error, contact administrator
         */
        public static UsersResource.DeleteUsersByUserIdResponse withPlainInternalServerError(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(500).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.DeleteUsersByUserIdResponse(responseBuilder.build());
        }

    }

    public class GetUsersByUserIdResponse
        extends org.folio.rest.jaxrs.resource.support.ResponseWrapper
    {


        private GetUsersByUserIdResponse(Response delegate) {
            super(delegate);
        }

        /**
         * Returns item with a given ID e.g. {
         *     "username" : "jhandey",
         *         "tenant" : "biglibrary",
         *     "id" : "7261ecaae3a74dc68b468e12a70b1aec",
         *     "active" : true,
         *     "meta" : {
         *         "creation_date" : "2016-11-05T0723",
         *         "last_login_date" : ""
         *     },
         *     "personal" : {
         *         "full_name" : "Jack Michael Handey",
         *         "birth_date" : "1965-07-08",
         *         "phone_home" : "+1 (212) 456-7891",
         *         "phone_work" : "+1 (212) 567-8912",
         *         "phone_mobile" : "+1 (212) 678-9123",
         *         "email_primary" : "jhandey@biglibrary.org",
         *         "email_alternate" : "handyjack65@gmail.com",
         *         "mailing_address" : "4567 Langford Ave, Manhattan, NY 11025"
         *     }
         *         
         * }
         * 
         * 
         * 
         * @param entity
         *     {
         *         "username" : "jhandey",
         *             "tenant" : "biglibrary",
         *         "id" : "7261ecaae3a74dc68b468e12a70b1aec",
         *         "active" : true,
         *         "meta" : {
         *             "creation_date" : "2016-11-05T0723",
         *             "last_login_date" : ""
         *         },
         *         "personal" : {
         *             "full_name" : "Jack Michael Handey",
         *             "birth_date" : "1965-07-08",
         *             "phone_home" : "+1 (212) 456-7891",
         *             "phone_work" : "+1 (212) 567-8912",
         *             "phone_mobile" : "+1 (212) 678-9123",
         *             "email_primary" : "jhandey@biglibrary.org",
         *             "email_alternate" : "handyjack65@gmail.com",
         *             "mailing_address" : "4567 Langford Ave, Manhattan, NY 11025"
         *         }
         *             
         *     }
         *     
         *     
         */
        public static UsersResource.GetUsersByUserIdResponse withJsonOK(User entity) {
            Response.ResponseBuilder responseBuilder = Response.status(200).header("Content-Type", "application/json");
            responseBuilder.entity(entity);
            return new UsersResource.GetUsersByUserIdResponse(responseBuilder.build());
        }

        /**
         * Item with a given ID not found e.g. "user not found"
         * 
         * 
         * @param entity
         *     "user not found"
         *     
         */
        public static UsersResource.GetUsersByUserIdResponse withPlainNotFound(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(404).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.GetUsersByUserIdResponse(responseBuilder.build());
        }

        /**
         * Internal server error, e.g. due to misconfiguration e.g. internal server error, contact administrator
         * 
         * @param entity
         *     internal server error, contact administrator
         */
        public static UsersResource.GetUsersByUserIdResponse withPlainInternalServerError(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(500).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.GetUsersByUserIdResponse(responseBuilder.build());
        }

    }

    public class GetUsersResponse
        extends org.folio.rest.jaxrs.resource.support.ResponseWrapper
    {


        private GetUsersResponse(Response delegate) {
            super(delegate);
        }

        /**
         * Returns a list of user items e.g. {
         *     "users" : [
         *         {
         *             "username" : "jhandey",
         *                 "tenant" : "biglibrary",
         *             "id" : "7261ecaae3a74dc68b468e12a70b1aec",
         *             "active" : true,
         *             "meta" : {
         *                 "creation_date" : "2016-11-05T0723",
         *                 "last_login_date" : ""
         *             },
         *             "personal" : {
         *                 "full_name" : "Jack Michael Handey",
         *                 "birth_date" : "1965-07-08",
         *                 "phone_home" : "+1 (212) 456-7891",
         *                 "phone_work" : "+1 (212) 567-8912",
         *                 "phone_mobile" : "+1 (212) 678-9123",
         *                 "email_primary" : "jhandey@biglibrary.org",
         *                 "email_alternate" : "handyjack65@gmail.com",
         *                 "mailing_address" : "4567 Langford Ave, Manhattan, NY 11025"
         *             }
         *                 
         *         }
         *     ],
         *     "total_records" : 1
         * }
         * 
         *             
         * 
         * 
         * @param entity
         *     {
         *         "users" : [
         *             {
         *                 "username" : "jhandey",
         *                     "tenant" : "biglibrary",
         *                 "id" : "7261ecaae3a74dc68b468e12a70b1aec",
         *                 "active" : true,
         *                 "meta" : {
         *                     "creation_date" : "2016-11-05T0723",
         *                     "last_login_date" : ""
         *                 },
         *                 "personal" : {
         *                     "full_name" : "Jack Michael Handey",
         *                     "birth_date" : "1965-07-08",
         *                     "phone_home" : "+1 (212) 456-7891",
         *                     "phone_work" : "+1 (212) 567-8912",
         *                     "phone_mobile" : "+1 (212) 678-9123",
         *                     "email_primary" : "jhandey@biglibrary.org",
         *                     "email_alternate" : "handyjack65@gmail.com",
         *                     "mailing_address" : "4567 Langford Ave, Manhattan, NY 11025"
         *                 }
         *                     
         *             }
         *         ],
         *         "total_records" : 1
         *     }
         *     
         *                 
         *     
         */
        public static UsersResource.GetUsersResponse withJsonOK(UserdataCollection entity) {
            Response.ResponseBuilder responseBuilder = Response.status(200).header("Content-Type", "application/json");
            responseBuilder.entity(entity);
            return new UsersResource.GetUsersResponse(responseBuilder.build());
        }

        /**
         * Bad request, e.g malformed request body or query parameter. Details of the error (e.g name of the parameter or line/character number with malformed data) provided in the response. e.g. unable to list users -- malformed parameter 'query', syntax error at column 6
         * 
         * @param entity
         *     unable to list users -- malformed parameter 'query', syntax error at column 6
         */
        public static UsersResource.GetUsersResponse withPlainBadRequest(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(400).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.GetUsersResponse(responseBuilder.build());
        }

        /**
         * Internal server error, e.g due to misconfiguration e.g. internal server error, contact administrator
         * 
         * @param entity
         *     internal server error, contact administrator
         */
        public static UsersResource.GetUsersResponse withPlainInternalServerError(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(500).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.GetUsersResponse(responseBuilder.build());
        }

    }

    public enum Order {

        desc,
        asc;

    }

    public class PostUsersResponse
        extends org.folio.rest.jaxrs.resource.support.ResponseWrapper
    {


        private PostUsersResponse(Response delegate) {
            super(delegate);
        }

        /**
         * Returns a newly created item, with server-controlled fields like 'id' populated e.g. {
         *     "username" : "jhandey",
         *         "tenant" : "biglibrary",
         *     "id" : "7261ecaae3a74dc68b468e12a70b1aec",
         *     "active" : true,
         *     "meta" : {
         *         "creation_date" : "2016-11-05T0723",
         *         "last_login_date" : ""
         *     },
         *     "personal" : {
         *         "full_name" : "Jack Michael Handey",
         *         "birth_date" : "1965-07-08",
         *         "phone_home" : "+1 (212) 456-7891",
         *         "phone_work" : "+1 (212) 567-8912",
         *         "phone_mobile" : "+1 (212) 678-9123",
         *         "email_primary" : "jhandey@biglibrary.org",
         *         "email_alternate" : "handyjack65@gmail.com",
         *         "mailing_address" : "4567 Langford Ave, Manhattan, NY 11025"
         *     }
         *         
         * }
         * 
         * 
         * 
         * @param location
         *     URI to the created user item
         * @param entity
         *     {
         *         "username" : "jhandey",
         *             "tenant" : "biglibrary",
         *         "id" : "7261ecaae3a74dc68b468e12a70b1aec",
         *         "active" : true,
         *         "meta" : {
         *             "creation_date" : "2016-11-05T0723",
         *             "last_login_date" : ""
         *         },
         *         "personal" : {
         *             "full_name" : "Jack Michael Handey",
         *             "birth_date" : "1965-07-08",
         *             "phone_home" : "+1 (212) 456-7891",
         *             "phone_work" : "+1 (212) 567-8912",
         *             "phone_mobile" : "+1 (212) 678-9123",
         *             "email_primary" : "jhandey@biglibrary.org",
         *             "email_alternate" : "handyjack65@gmail.com",
         *             "mailing_address" : "4567 Langford Ave, Manhattan, NY 11025"
         *         }
         *             
         *     }
         *     
         *     
         */
        public static UsersResource.PostUsersResponse withJsonCreated(String location, StreamingOutput entity) {
            Response.ResponseBuilder responseBuilder = Response.status(201).header("Content-Type", "application/json").header("Location", location);
            responseBuilder.entity(entity);
            return new UsersResource.PostUsersResponse(responseBuilder.build());
        }

        /**
         * Bad request, e.g malformed request body or query parameter. Details of the error (e.g name of the parameter or line/character number with malformed data) provided in the response. e.g. "unable to add user -- malformed JSON at 13:3"
         * 
         * 
         * @param entity
         *     "unable to add user -- malformed JSON at 13:3"
         *     
         */
        public static UsersResource.PostUsersResponse withPlainBadRequest(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(400).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.PostUsersResponse(responseBuilder.build());
        }

        /**
         * Internal server error, e.g. due to misconfiguration e.g. Internal server error, contact administrator
         * 
         * @param entity
         *     Internal server error, contact administrator
         */
        public static UsersResource.PostUsersResponse withPlainInternalServerError(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(500).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.PostUsersResponse(responseBuilder.build());
        }

    }

    public class PutUsersByUserIdResponse
        extends org.folio.rest.jaxrs.resource.support.ResponseWrapper
    {


        private PutUsersByUserIdResponse(Response delegate) {
            super(delegate);
        }

        /**
         * Item successfully updated
         * 
         */
        public static UsersResource.PutUsersByUserIdResponse withNoContent() {
            Response.ResponseBuilder responseBuilder = Response.status(204);
            return new UsersResource.PutUsersByUserIdResponse(responseBuilder.build());
        }

        /**
         * Item with a given ID not found e.g. "user not found"
         * 
         * 
         * @param entity
         *     "user not found"
         *     
         */
        public static UsersResource.PutUsersByUserIdResponse withPlainNotFound(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(404).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.PutUsersByUserIdResponse(responseBuilder.build());
        }

        /**
         * Bad request, e.g malformed request body or query parameter. Details of the error (e.g name of the parameter or line/character number with malformed data) provided in the response. e.g. "unable to update user -- malformed JSON at 13:4"
         * 
         * 
         * @param entity
         *     "unable to update user -- malformed JSON at 13:4"
         *     
         */
        public static UsersResource.PutUsersByUserIdResponse withPlainBadRequest(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(400).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.PutUsersByUserIdResponse(responseBuilder.build());
        }

        /**
         * Internal server error, e.g. due to misconfiguration e.g. internal server error, contact administrator
         * 
         * @param entity
         *     internal server error, contact administrator
         */
        public static UsersResource.PutUsersByUserIdResponse withPlainInternalServerError(String entity) {
            Response.ResponseBuilder responseBuilder = Response.status(500).header("Content-Type", "text/plain");
            responseBuilder.entity(entity);
            return new UsersResource.PutUsersByUserIdResponse(responseBuilder.build());
        }

    }

}
