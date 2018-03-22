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

