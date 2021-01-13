package org.folio.rest.impl;

import org.folio.rest.jaxrs.model.OkapiPermission;
import org.folio.rest.jaxrs.model.Permission;

public class PermissionUtils {

  private PermissionUtils() {
    
  }
  
  /**
   * Compare two objects for equality, w/ checks for null, etc.
   * 
   * @param a LHS of comparison
   * @param b RHS of comparison
   * @return true if equal, false otherwise
   */
  public static boolean equals(Object a, Object b) {
    if ((a == null && b != null) || (a != null && b == null)) {
      return false;
    }
    return (a == null && b == null) || (a.equals(b));
  }

  /**
   * Compare an OkapiPermission to a Permission, including definedBy.
   * 
   * @param okapiPerm a permission passed from Okapi
   * @param moduleName the module name to compare against permission's definedBy.moduleName property
   * @param perm the Permission to compare against
   * @return true if equal, false otherwise
   */
  public static boolean comparePerms(OkapiPermission okapiPerm, String moduleName, Permission perm) {
    return comparePerms(okapiPerm, perm)
        && perm.getDefinedBy() != null 
        && equals(moduleName, perm.getDefinedBy().getModuleName());
  }
  
  /**
   * Compare an OkapiPermission to a Permission
   * 
   * @param okapiPerm a permission passed from Okapi
   * @param perm the Permission to compare against
   * @return true if equal, false otherwise
   */
  public static boolean comparePerms(OkapiPermission okapiPerm, Permission perm) {
    if ((okapiPerm == null || perm == null)) {
      return false;
    }
    return equals(okapiPerm.getPermissionName(), perm.getPermissionName())
        && equals(okapiPerm.getDescription(), perm.getDescription())
        && equals(okapiPerm.getDisplayName(), perm.getDisplayName())
        && equals(okapiPerm.getSubPermissions(), perm.getSubPermissions());
  }  
}
