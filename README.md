# mod-permissions

Copyright (C) 2016-2019 The Open Library Foundation

This software is distributed under the terms of the Apache License,
Version 2.0. See the file "[LICENSE](LICENSE)" for more information.

# Introduction

This module stores permissions and associations between permissions and users.
It also maintains a hierarchy of permissions and sub-permissions, allowing for
permissions to act as roles, rather than simple bits. It is used primarily by
the Authtoken module, though it is possible that some Authentication
implementations may have reason to make calls to the Permissions module as well.

# Additional information

The [raml-module-builder](https://github.com/folio-org/raml-module-builder) framework.

Other [modules](https://dev.folio.org/source-code/#server-side).

See project [MODPERMS](https://issues.folio.org/browse/MODPERMS)
at the [FOLIO issue tracker](https://dev.folio.org/guidelines/issue-tracker).

Other FOLIO Developer documentation is at [dev.folio.org](https://dev.folio.org/)
