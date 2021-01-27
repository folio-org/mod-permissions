package org.folio.rest.impl;

import java.util.Objects;
import org.folio.rest.jaxrs.model.OkapiPermission;
import org.folio.rest.jaxrs.model.Permission;

public class PermissionUtils {

  private PermissionUtils() {
    
  }

  /**
   * Returns true if the provided OkapiPermission and moduleName are equal to the values in the
   * provided Permission.
   * 
   * @param okapiPerm a permission passed from Okapi
   * @param moduleName the module name to compare against permission's moduleName property
   * @param perm the Permission to compare against
   * @return true if equal, false otherwise
   */
  public static boolean equals(OkapiPermission okapiPerm, String moduleName, Permission perm) {
    String otherModuleName = perm == null ? null : perm.getModuleName();
    return equals(okapiPerm, perm) && Objects.equals(moduleName, otherModuleName);
  }
  
  /**
   * Returns true if the values in okapiPerm equal to the values in perm, or if both okapiPerm and perm are null.
   * 
   * @param okapiPerm a permission passed from Okapi
   * @param perm the Permission to compare against
   * @return true if equal, false otherwise
   */
  public static boolean equals(OkapiPermission okapiPerm, Permission perm) {
    if (okapiPerm == null && perm == null) {
      return true;
    }
    if (okapiPerm == null || perm == null) {
      return false;
    }
    return Objects.equals(okapiPerm.getPermissionName(), perm.getPermissionName())
        && Objects.equals(okapiPerm.getDescription(), perm.getDescription())
        && Objects.equals(okapiPerm.getDisplayName(), perm.getDisplayName())
        && Objects.equals(okapiPerm.getSubPermissions(), perm.getSubPermissions());
  }  
}
