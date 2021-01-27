package org.folio.rest.impl;

import static org.junit.Assert.*;
import java.util.ArrayList;
import java.util.List;
import org.assertj.core.util.Arrays;
import org.folio.rest.jaxrs.model.OkapiPermission;
import org.folio.rest.jaxrs.model.Permission;
import org.junit.Test;

public class PermissionUtilsTest {

  @Test
  public void testequalsOkapiPermissionStringPermission() {
    List<String> subPerms = new ArrayList<>();
    subPerms.add("bar");
    subPerms.add("baz");
    OkapiPermission lhs = new OkapiPermission()
        .withPermissionName("foo.all")
        .withDescription("everything foo")
        .withDisplayName("all foo permissions")
        .withSubPermissions(subPerms);
    Permission rhs = new Permission()
        .withPermissionName("foo.all")
        .withDescription("everything foo")
        .withDisplayName("all foo permissions")
        .withSubPermissions(Arrays.asList(subPerms.toArray()));
    assertTrue(PermissionUtils.equals(lhs, null, rhs));
    
    rhs.setModuleName("mod-dummy");
    assertTrue(PermissionUtils.equals(lhs, "mod-dummy", rhs));
    assertFalse(PermissionUtils.equals(lhs, "mod-smarty", rhs));
    assertFalse(PermissionUtils.equals(lhs, null, rhs));
    assertFalse(PermissionUtils.equals(null, null, rhs));
    assertFalse(PermissionUtils.equals(lhs, null, null));
    assertTrue(PermissionUtils.equals(null, null, null));
  }

  @Test
  public void testequalsOkapiPermissionPermission() {
    OkapiPermission lhs = new OkapiPermission();
    Permission rhs = new Permission();

    lhs.setPermissionName("foo.all");
    assertFalse(PermissionUtils.equals(lhs, rhs));
    rhs.setPermissionName("bar.all");
    assertFalse(PermissionUtils.equals(lhs, rhs));
    rhs.setPermissionName("foo.all");
    assertTrue(PermissionUtils.equals(lhs, rhs));

    lhs.setDescription("everything foo");
    assertFalse(PermissionUtils.equals(lhs, rhs));
    rhs.setDescription("everything bar");
    assertFalse(PermissionUtils.equals(lhs, rhs));
    rhs.setDescription("everything foo");
    assertTrue(PermissionUtils.equals(lhs, rhs));

    lhs.setDisplayName("all foo permissions");
    assertFalse(PermissionUtils.equals(lhs, rhs));
    rhs.setDisplayName("all bar permissions");
    assertFalse(PermissionUtils.equals(lhs, rhs));
    rhs.setDisplayName("all foo permissions");
    assertTrue(PermissionUtils.equals(lhs, rhs));

    List<String> lhsSubPerms = new ArrayList<>();
    lhsSubPerms.add("bar");
    lhsSubPerms.add("baz");
    lhs.setSubPermissions(lhsSubPerms);
    assertFalse(PermissionUtils.equals(lhs, rhs));
    List<Object> rhsSubPerms = Arrays.asList(lhsSubPerms.toArray());
    rhs.setSubPermissions(rhsSubPerms);
    assertTrue(PermissionUtils.equals(lhs, rhs));
    rhsSubPerms.remove(0);
    rhs.setSubPermissions(rhsSubPerms);
    assertFalse(PermissionUtils.equals(lhs, rhs));
  }

}
