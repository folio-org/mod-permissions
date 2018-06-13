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
 * MODPERMS-29 Add 'grantedTo' and 'childOf' fields to permission object, keep accounting of users owning a given permission and parent permissions listing a permission as a sub

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

