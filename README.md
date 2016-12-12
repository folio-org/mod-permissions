# mod-users

Copyright (C) 2016 The Open Library Foundation

This software is distributed under the terms of the Apache License,
Version 2.0. See the file "[LICENSE](LICENSE)" for more information.

Module to provide central user management for FOLIO systems.

This module is built using the raml-module-builder framework.

## Troubleshooting

If you are using a version of the Oracle JDK prior to 1.8.0_101, the
[Let's Encrypt](https://letsencrypt.org/)
certificate authority is not in the Java trust store, and so it will
not be possible to download components from the FOLIO Maven
repository. You will see error messages like:

> Could not resolve dependencies for project com.indexdata:mod-users:jar:0.1-SNAPSHOT: Failed to collect dependencies at org.folio:domain-models-api-interfaces:jar:0.0.1-SNAPSHOT: Failed to read artifact descriptor for org.folio:domain-models-api-interfaces:jar:0.0.1-SNAPSHOT: Could not transfer artifact org.folio:domain-models-api-interfaces:pom:0.0.1-SNAPSHOT from/to folio-nexus (https://repository.folio.org/repository/maven-folio): sun.security.validator.ValidatorException: PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: unable to find valid certification path to requested target -> [Help 1]

The fix is just to replace your JDK with a sufficiently recent
replacement. (Or you can use OpenJDK, which has supported Let's
Encrypt for longer.)

## Additional information

The [raml-module-builder](https://github.com/folio-org/raml-module-builder) framework.

Other [modules](http://dev.folio.org/source-code/#server-side).

Other FOLIO Developer documentation is at [dev.folio.org](http://dev.folio.org/)
