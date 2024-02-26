package org.folio.rest.tools.utils;

public class ModuleName {
  private static final String MODULE_NAME = "mod_permissions";
  private static final String MODULE_VERSION = "99999.0.0";

  /**
   * The module name with minus replaced by underscore, for example {@code mod_foo_bar}.
   */
  public static String getModuleName() {
    return MODULE_NAME;
  }

  /**
   * The module version taken from pom.xml at compile time.
   */
  public static String getModuleVersion() {
    return MODULE_VERSION;
  }
}
