package org.folio.rest.impl;

import static org.junit.Assert.*;
import java.util.ArrayList;
import java.util.List;
import org.assertj.core.util.Arrays;
import org.folio.rest.jaxrs.model.DefinedBy;
import org.folio.rest.jaxrs.model.OkapiPermission;
import org.folio.rest.jaxrs.model.Permission;
import org.junit.Test;

public class PermissionUtilsTest {

  @Test
  public void testEquals() {
    assertTrue(PermissionUtils.equals(null, null));
    assertFalse(PermissionUtils.equals("", null));
    assertFalse(PermissionUtils.equals(null, ""));
    assertTrue(PermissionUtils.equals("", ""));
    assertFalse(PermissionUtils.equals("a", "b"));
  }

  @Test
  public void testComparePermsOkapiPermissionStringPermission() {
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
    assertFalse(PermissionUtils.comparePerms(lhs, null, rhs));
    
    DefinedBy definedBy = new DefinedBy();
    rhs.setDefinedBy(definedBy);
    assertTrue(PermissionUtils.comparePerms(lhs, null, rhs));
    
    definedBy.setModuleName("mod-dummy");
    rhs.setDefinedBy(definedBy);
    assertTrue(PermissionUtils.comparePerms(lhs, "mod-dummy", rhs));
    assertFalse(PermissionUtils.comparePerms(lhs, "mod-smarty", rhs));
    assertFalse(PermissionUtils.comparePerms(null, null, rhs));
    assertFalse(PermissionUtils.comparePerms(lhs, null, null));
    assertFalse(PermissionUtils.comparePerms(null, null, null));
  }

  @Test
  public void testComparePermsOkapiPermissionPermission() {
    OkapiPermission lhs = new OkapiPermission();
    Permission rhs = new Permission();

    lhs.setPermissionName("foo.all");
    assertFalse(PermissionUtils.comparePerms(lhs, rhs));
    rhs.setPermissionName("bar.all");
    assertFalse(PermissionUtils.comparePerms(lhs, rhs));
    rhs.setPermissionName("foo.all");
    assertTrue(PermissionUtils.comparePerms(lhs, rhs));

    lhs.setDescription("everything foo");
    assertFalse(PermissionUtils.comparePerms(lhs, rhs));
    rhs.setDescription("everything bar");
    assertFalse(PermissionUtils.comparePerms(lhs, rhs));
    rhs.setDescription("everything foo");
    assertTrue(PermissionUtils.comparePerms(lhs, rhs));

    lhs.setDisplayName("all foo permissions");
    assertFalse(PermissionUtils.comparePerms(lhs, rhs));
    rhs.setDisplayName("all bar permissions");
    assertFalse(PermissionUtils.comparePerms(lhs, rhs));
    rhs.setDisplayName("all foo permissions");
    assertTrue(PermissionUtils.comparePerms(lhs, rhs));

    List<String> lhsSubPerms = new ArrayList<>();
    lhsSubPerms.add("bar");
    lhsSubPerms.add("baz");
    lhs.setSubPermissions(lhsSubPerms);
    assertFalse(PermissionUtils.comparePerms(lhs, rhs));
    List<Object> rhsSubPerms = Arrays.asList(lhsSubPerms.toArray());
    rhs.setSubPermissions(rhsSubPerms);
    assertTrue(PermissionUtils.comparePerms(lhs, rhs));
    rhsSubPerms.remove(0);
    rhs.setSubPermissions(rhsSubPerms);
    assertFalse(PermissionUtils.comparePerms(lhs, rhs));
  }

}
