# mod-permissions

Copyright (C) 2016-2023 The Open Library Foundation

This software is distributed under the terms of the Apache License,
Version 2.0. See the file "[LICENSE](LICENSE)" for more information.

## Introduction

This module stores permissions and associations between permissions and users.
It also maintains a hierarchy of permissions and sub-permissions, allowing for
permissions to act as roles, rather than simple bits. It is used primarily by
the Authtoken module, though it is possible that some Authentication
implementations may have reason to make calls to the Permissions module as well.

There are two types of permissions:

*  Immutable permissions that are defined in module descriptors. When modules
   are enabled for a tenant, they are passed from Okapi to mod-permissions via
   the [tenantPermissions](ramls/tenantPermissions.raml) interface.

*  Mutable, User-defined, permissions that any module can manage if they
   have permissions to do so. This is part of the
   [permissions](ramls/permissions.raml) interface.

Mod-permission also has the notion of users and permissions associated
with them. The `/perms/users` API has two user identifiers:
`id` and `userId`. `id` is the mod-permissions identifier for the user
while `userId` points to the `id` field of the mod-users `/users` API.
Having the two is confusing, they could be the same.
User management is also part of the
[permissions](ramls/permissions.raml) interface.

As for any module in FOLIO, normal permission restrictions apply: either
user must have permissions to access an interface or the modulePermissions
includes it for the request.

On top of that, and to prevent users from getting arbitrary permissions,
there are further restrictions on permissions that can be assigned to a user.

This was introduced in mod-permissions version 6.0.0.

The restrictions work as follows:

1. If auth is disabled for the tenant where permissions are added, the operation
is allowed.

2. If module permissions or operating user permissions contains the permission
to be granted for a user, the operation is allowed.

3. If new permission name is `perms.users.assign.okapi` or starts with
`okapi.` and operating user permissions and module permissions doesn't
contain `perms.users.assign.okapi`, the operation is denied.

4. If the new permission is mutable and operating user permissions and module
permissions doesn't contain `perms.users.assign.immutable`, the operation
is denied.

5. If the new permission is immutable and operating user permissions and
module permissions doesn't contain `perms.users.assign.mutable`, the
operation is denied.

6. Otherwise, the operation is allowed.

## Additional information

The [raml-module-builder](https://github.com/folio-org/raml-module-builder)
framework.

Other [modules](https://dev.folio.org/source-code/#server-side).

Other FOLIO Developer documentation is at
[dev.folio.org](https://dev.folio.org/)

### Issue tracker

See project [MODPERMS](https://issues.folio.org/browse/MODPERMS)
at the [FOLIO issue tracker](https://dev.folio.org/guidelines/issue-tracker).

### ModuleDescriptor

See the [ModuleDescriptor.json](descriptors/ModuleDescriptor-template.json)
for the interfaces that this module requires and provides, the permissions,
and the additional module metadata.

### API documentation

This module's
[API documentation](https://dev.folio.org/reference/api/#mod-permissions).

### Code analysis

[SonarQube analysis](https://sonarcloud.io/dashboard?id=org.folio%3Amod-permissions).

### Download and configuration

The built artifacts for this module are available.
See [configuration](https://dev.folio.org/download/artifacts) for repository
access, and the
[Docker image](https://hub.docker.com/r/folioorg/mod-permissions/).

