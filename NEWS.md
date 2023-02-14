## 2023-02-14 v6.3.0

New features:

 * [MODPERMS-208](https://issues.folio.org/browse/MODPERMS-208) Honor module replaces
 * [MODPERMS-209](https://issues.folio.org/browse/MODPERMS-209) UserId: validate and store normalized

Fixes:

 * [MODPERMS-211](https://issues.folio.org/browse/MODPERMS-211) RMB 35.0.6, Vert.x 4.3.8

## 2022-10-24 v6.2.0

 * [MODPERMS-205](https://issues.folio.org/browse/MODPERMS-205) Upgrade to RMB 35.0.1, Vert.x 4.3.4, log4j 2.19.0

## 2022-06-14 v6.1.0

 * [MODPERMS-203](https://issues.folio.org/browse/MODPERMS-203) Upgrade to RMB 34.0.0, Vert.x.4.3.1
 * [MODPERMS-181](https://issues.folio.org/browse/MODPERMS-181) Integration test and Kiwi-to-Lotus migration test

## 2022-05-30 v6.0.6

Fixes:

 * [MODPERMS-201](https://issues.folio.org/browse/MODPERMS-201) Deadlock (timeout) when creating new permission user
 * [MODPERMS-198](https://issues.folio.org/browse/MODPERMS-198) Update mod-users submodule from v17.2.2 to v18.2.0
 * [MODPERMS-199](https://issues.folio.org/browse/MODPERMS-199) Issue in adding permissions on user record

## 2022-05-06 v6.0.5

Fixes:

 * [MODPERMS-197](https://issues.folio.org/browse/MODPERMS-197) RMB 33.2.9, Vert.x 4.2.7, jackson-databind 2.13.2.1 (CVE-2020-36518)
 * [MODPERMS-195](https://issues.folio.org/browse/MODPERMS-195) perms_users_sanitize (userId is null) ignores tenant

## 2022-04-07 v6.0.4

Fixes:

 * [MODPERMS-191](https://issues.folio.org/browse/MODPERMS-191) Enabling of 6.0.3 fails

## 2022-04-05 v6.0.3

Fixes:

 * [MODPERMS-188](https://issues.folio.org/browse/MODPERMS-188) Enabling of mod-permissions fails
 * [MODPERMS-189](https://issues.folio.org/browse/MODPERMS-189) Faster user migration

## 2022-03-24 v6.0.2

Fixes:

 * [MODPERMS-185](https://issues.folio.org/browse/MODPERMS-185) Stack issue
   with user migration. Change SQL update of `grantedTo`/`ChildOf` to atomic.
 * [MODPERMS-184](https://issues.folio.org/browse/MODPERMS-184) Migrate only
   users having `okapi.all`. Set `perms.all` includes
  `perms.users.assign.immutable`, `perms.users.assign.mutable`
 * [MODPERMS-186](https://issues.folio.org/browse/MODPERMS-186) loadSample
   fails on 2nd init

## 2022-03-10 v6.0.1

Fixes:

 * [MODPERMS-177](https://issues.folio.org/browse/MODPERMS-177) Delete bad users, add constraint
 * [MODPERMS-179](https://issues.folio.org/browse/MODPERMS-179) Remove blank in permission name
 * [FOLIO-3231](https://issues.folio.org/browse/FOLIO-3231) Replace runLintRamlCop/publishAPI by doApiLint/doApiDoc

## 2022-02-18 v6.0.0

Changes:

 * [MODPERMS-157](https://issues.folio.org/browse/MODPERMS-157) Check assign permissions for operating user
 * [MODPERMS-160](https://issues.folio.org/browse/MODPERMS-160) Migrate assignment permissions
 * [MODPERMS-56](https://issues.folio.org/browse/MODPERMS-56) Support offset and limit from traits/pageable.raml. Interface `permissions` bumped to 5.5.

Fixes:

 * Update to RMB 33.2.5, Vert.x 4.2.4

## 2021-12-23 v5.14.4

 * [MODPERMS-173](https://issues.folio.org/browse/MODPERMS-173) log4j 2.17.0, RMB 33.2.3, Vert.x 4.2.3

## 2021-12-16 v5.14.3

 * [MODPERMS-171](https://issues.folio.org/browse/MODPERMS-171) Update to RMB 33.2.2 Log4j 2.16.0
 * [MODPERMS-167](https://issues.folio.org/browse/MODPERMS-167) RMB 33.2.1, Vertx 4.2.1, Log4j 2.15.0 fixing remote execution (CVE-2021-44228)

## 2021-10-01 v5.14.2

 * [MODPERMS-154](https://issues.folio.org/browse/MODPERMS-154) Update RMB to 33.1.1 and Vert.x 4.1.4

NOTE: There was no v5.14.1 due to errors preparing the release.

## 2021-05-27 v5.14.0

No bug fixes for this release. JIRA issues:

 * [MODPERMS-148](https://issues.folio.org/browse/MODPERMS-148) RMB 33 and Vert.x 4
 * [MODPERMS-117](https://issues.folio.org/browse/MODPERMS-117) Add personal data disclosure form

## 2021-02-27 v5.13.0

### Static Permission Migration
There are several changes related to migration of static (system-defined) permissions introduced in this release:

* When permissions that once appeared in a module descriptor are removed in a newer version of the module descriptor, they will be marked deprecated. 
  * For now, this means the `displayName` of these permissions will be prefixed with `(deprecated) `, but the permissions will not be filtered out of any API calls. 
  * In the future we'd like to filter deprecated permissions out of API responses unless they're specifically requested.  This feature, however, did not make it into v5.13.0.
* Permissions can now be renamed via a new `replaces` property.  Perm-users (Assignments) and PermissionSet sub-permissions will be updated automatically.
  * Example:  `foo.get` replaces `foo.read`, `foo.view`.  Any users which were assigned either `foo.read` or `foo.view` would automatically be granted `foo.get`.  Permissions `foo.read` and `foo.view` would be marked deprecated.
* A new [purgeDeprecated API](https://s3.amazonaws.com/foliodocs/api/mod-permissions/permissions.html#perms_purge_deprecated_post) has been introduced to allow an operator to purge deprecated permissions.  This will:
  * Remove deprecated permissions from the system
  * Remove deprecated permission names from user's permission assignments
  * Remove deprecated permissions from permission sets
  * *N.B.* This API should only be called once you're confident that a module downgrade will not be required.  Once deprecated permissions are purged, the system will not know how to "undo" the permission migration for that module. 
  * *N.B.* Currently this is an all-or-nothing operation.  There is no way to purge individual deprecated permissions, or remove deprecated permissions for a particular module.
* If a permission name collides with a user-defined permission with the same name,
  * For now, the call to enable the module (install/upgrade) will fail with an appropriate error message.  
  * In the future we'd like to do something like rename the user-defined permissions and adjust assignments as needed. 
* All system-defined permissions will now include context about the module that defined them (new permission fields `moduleName`/`moduleVersion`).
* Once v5.13.0 is enabled, duplicate permission definitions are not allowed.  If a permission (name) is defined by multiple module descriptors the upgrade will fail with an appropriate error message. 
* The `mutable` property will now be ignored when creating or updating permissions.  This is a system-controlled field.
* Orphaned system-defined permissions - those which are marked as immutable and are no longer present in any enabled module descriptor - will automatically be deprecated when upgrading mod-permissions.

### Deployment Considerations
* OKAPI [v4.6.0](https://github.com/folio-org/okapi/releases/tag/v4.6.0) or greater ([v4.7.2](https://github.com/folio-org/okapi/releases/tag/v4.7.2) or greater is highly recommended) is required to benefit from these changes.
* ***Contrary to earlier communications, it is NOT required to upgrade mod-permissions first or last.  It is also NOT required that you resolve duplicate permissions prior to upgrading to mod-permissions-5.13.0***

### Stories / Bugs:

* [MODPERMS-135](https://issues.folio.org/browse/MODPERMS-135) replaces processing doesn't update permissions, only permission assignments
* [MODPERMS-133](https://issues.folio.org/browse/MODPERMS-133) Dummy permissions assigned a context
* [MODPERMS-131](https://issues.folio.org/browse/MODPERMS-131) Some problems reported by IntelliJ IDEA
* [MODPERMS-130](https://issues.folio.org/browse/MODPERMS-130) Allow re-use of permissions when deprecated
* [MODPERMS-128](https://issues.folio.org/browse/MODPERMS-128) Wrong _tenant version in module descriptor
* [MODPERMS-126](https://issues.folio.org/browse/MODPERMS-126) Fix mutable handling
* [MODPERMS-121](https://issues.folio.org/browse/MODPERMS-121) Upgrade mod-permissions to RMB v32
* [MODPERMS-116](https://issues.folio.org/browse/MODPERMS-116) Create purge Deprecated API
* [MODPERMS-115](https://issues.folio.org/browse/MODPERMS-115) Implement static permission migration
* [MODPERMS-108](https://issues.folio.org/browse/MODPERMS-108) Upgrade to RMB 31.1.2 due to [RMB-732](https://issues.folio.org/browse/RMB-732) fix

## 2020-10-06 v5.12.0

 * [MODPERMS-102](https://issues.folio.org/browse/MODPERMS-102) Upgrade to RMB 31.1.0 and JDK 11
 * [MODPERMS-101](https://issues.folio.org/browse/MODPERMS-101) Disable/purge tenant and then re-enable failed
 * [MODPERMS-94](https://issues.folio.org/browse/MODPERMS-94) function count_estimate(unknown) does not exist during second mod-permissions install
 * [MODPERMS-85](https://issues.folio.org/browse/MODPERMS-85) Invalid CQL when encoding permission name value
 * [MODPERMS-83](https://issues.folio.org/browse/MODPERMS-83) Increase code coverage and fix code smells

## 2020-06-21 v5.11.0

 * [MODPERMS-84](https://issues.folio.org/browse/MODPERMS-84) Creating permissions set in FOLIO Settings ignores provided UUID
 * [MODPERMS-36](https://issues.folio.org/browse/MODPERMS-36) 500 Errors need to be handled gracefully
 * Update RAML so that 204: no content, really returns no content.
 * Add HTTP status 400 as a possible return status for several end-points.
 * Update to RMB 30.0.3.

## 2020-06-04 v5.10.0

 * [MODPERMS-82](https://issues.folio.org/browse/MODPERMS-82) Update to RMB 30
 * [MODPERMS-75](https://issues.folio.org/browse/MODPERMS-75) Remove `gen_random_uuid()`, remove unused template_*.sql
 * Provide _tenantPermissions interface 1.1.
 * subpermissions are expanded recursively.
 * Provide permissions interface 5.3, because of new query parameter `expanded`.

## 2019-12-06 v5.9.0

 * [MODPERMS-72](https://issues.folio.org/browse/MODPERMS-72) Use JVM features to manage container memory
 * [MODPERMS-68](https://issues.folio.org/browse/MODPERMS-68) Fix user permissions can be accessed w/o being logged in

## 2019-10-07 v5.8.3

 * [MODPERMS-67](https://issues.folio.org/browse/MODPERMS-67) Verify and reduce the cost of expanding permissions

## 2019-09-25 v5.8.2

 * [MODPERMS-64](https://issues.folio.org/browse/MODPERMS-64) Expanded permissions causing stack overflow preventing login
 * [MODPERMS-66](https://issues.folio.org/browse/MODPERMS-66) Add index to permissionName to improve performance

## 2019-07-23 v5.8.1

 * [MODPERMS-58](https://issues.folio.org/browse/MODPERMS-58) Upgrade to RMB 26.2.2
 * [MODPERMS-60](https://issues.folio.org/browse/MODPERMS-60) POST to perms/permissions fails with 500 error

## 2019-06-12 v5.8.0
 * [MODPERMS-59](https://issues.folio.org/browse/MODPERMS-59) Using cache to further improve mod-permission performance

## 2019-06-12 v5.7.0
 * [MODPERMS-57](https://issues.folio.org/browse/MODPERMS-57) Improve mod-permission performance

## 2019-05-10 v5.6.0
 * [MODPERMS-53](https://issues.folio.org/browse/MODPERMS-53) Change limit from 1000 to 2147483647 (permission.raml)
 * [MODPERMS-55](https://issues.folio.org/browse/MODPERMS-55) Update to RMB 24 / CQLPG 4.0.0

## 2019-03-15 v5.5.0
 * [MODPERMS-50](https://issues.folio.org/browse/MODPERMS-50) use loadSample to load sample data
 * [MODPERMS-48](https://issues.folio.org/browse/MODPERMS-48) Use description fields in RAML JSON schemas
 * [MODPERMS-49](https://issues.folio.org/browse/MODPERMS-49) Move RAMLs and Schemas from shared raml-util
 * [MODPERMS-51](https://issues.folio.org/browse/MODPERMS-51) Update to Vert.X 3.5.4
 * [MODPERMS-52](https://issues.folio.org/browse/MODPERMS-52) Fix raml files in multiple places

## 2018-12-05 v5.4.0
 * Update to RAML 1.0 ([MODPERMS-47](https://issues.folio.org/browse/MODPERMS-47))
 * Reduce unnecessary logging ([MODPERMS-42](https://issues.folio.org/browse/MODPERMS-42))

## 2018-08-08 v5.3.2
 * Fix metadata population for permissions

## 2018-08-06 v5.3.1
 * Handle TenantPerms update as per [MODPERMS-45](https://issues.folio.org/browse/MODPERMS-45)

## 2018-07-13 v5.3.0
 * Add 'metadata' field to permissionUsers

## 2018-06-13 v5.2.5
 * Improve error messages when adding non-existent perms to users or other permissions
 * Update RMB to 19.3.1

## 2018-05-22 v5.2.4
 * Update RMB to 19.1.1 to keep stack size smaller
 * Refactor recursive queries for expansion to put less load on Postgres

## 2018-04-03 v5.2.3
 * Use criteria instead of CQL to get permissions for expanded queries

## 2018-03-28 v5.2.2
 * Fix bug in deleting permissions from user when employing "indexField=userId"

## 2018-03-28 v5.2.1
 * Fix bug in adding permissions to user when employing "indexField=userId"

## 2018-03-27 v5.2.0
 * In order to maintain proper linkage while allowing out-of-order loading for modules with permission dependencies, allow for "dummy" permissions to be created as placeholders. These permissions cannot be assigned to users and do not return in result sets. When an actual permission that exists as a dummy is created, the dummy is replaced.

## 2018-03-21 v5.1.2
 * Fix recursion bug while checking dependencies

## 2018-03-21 v5.1.1
 * Return 422 instead of 500 when a permissionSet with unsatisfiable dependencies is added via tenantPermissions

## 2018-03-20 v5.1.0
 * [MODPERMS-29](https://issues.folio.org/browse/MODPERMS-29) Add 'grantedTo' and 'childOf' fields to permission object, keep accounting of users owning a given permission and parent permissions listing a permission as a sub

## 2017-09-14 v5.0.0
 * Change permissions/permission users to be indexed by UUID instead of permissionName/username

## 2017-07-25 v4.0.4
 * Unescape input from URL before using it for search criteria

## 2017-07-24 v4.0.3
 * Upgrade to RMB 13.0.1
 * Provide more detail for error messages

## 2017-06-21 v4.0.2
 * Close hole that allowed erroneously formatted permissions to be added

## 2017-06-09 v4.0.1
 * Update to RMB 12.1.3
 * Fix flaw in exception handling for expanded subpermissions

## 2017-05-25
 * Update to RMB release 12.1.2
 * Add new "visible" field to permissions schema
 * Fix bug with CQL for /perms/users endpoint
 * Disallow extra properties in permissions and permission user schemas

## 2017-05-11
 * Update to RMB release 11.0.0
