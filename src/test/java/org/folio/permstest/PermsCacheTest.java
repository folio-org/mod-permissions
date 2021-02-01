package org.folio.permstest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.folio.rest.impl.PermsCache.PermCache;
import org.folio.rest.jaxrs.model.Permission;
import org.junit.Test;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PermsCacheTest {

  private static final Logger LOGGER = LogManager.getLogger(PermsCacheTest.class);

  private static final String P1 = "perm-1";
  private static final String S1 = "sub-1";
  private static final String S2 = "sub-2";
  private static final String S21 = "sub-2-1";
  private static final String S22 = "sub-2-2";
  private static final String S221 = "sub-2-2-1";
  private static final String S222 = "sub-2-2-2";

  private static final String C1 = "cperm-1";
  private static final String C2 = "cperm-2";

  @Test
  public void testPermsCache() {
    Map<String, Set<String>> subPermMap = new HashMap<>();
    subPermMap.put(P1, new HashSet<>(Arrays.asList(S1, S2)));
    subPermMap.put(S2, new HashSet<>(Arrays.asList(S21, S22)));
    subPermMap.put(S22, new HashSet<>(Arrays.asList(S221, S222)));
    PermCache pc = new PermCache(subPermMap, null);
    LOGGER.debug(pc.toString());

    assertFalse(pc.isStale());

    List<String> rs = pc.expandPerms(Arrays.asList(P1));
    assertEquals(7, rs.size());
    assertTrue(rs.contains(P1));
    assertTrue(rs.contains(S1));
    assertTrue(rs.contains(S2));
    assertTrue(rs.contains(S21));
    assertTrue(rs.contains(S22));
    assertTrue(rs.contains(S221));
    assertTrue(rs.contains(S222));

    rs = pc.expandPerms(Arrays.asList(S2));
    assertEquals(5, rs.size());
    assertTrue(rs.contains(S2));
    assertTrue(rs.contains(S21));
    assertTrue(rs.contains(S22));
    assertTrue(rs.contains(S221));
    assertTrue(rs.contains(S222));
  }

  @Test
  public void testCircularPerms() {
    Map<String, Set<String>> subPermMap = new HashMap<>();
    subPermMap.put(C1, new HashSet<>(Arrays.asList(C1, C2, P1)));
    subPermMap.put(C2, new HashSet<>(Arrays.asList(C2, C1, P1)));
    subPermMap.put(P1, new HashSet<>(Arrays.asList(S1, S2)));
    PermCache pc = new PermCache(subPermMap, null);
    LOGGER.debug(pc.toString());

    List<String> rs = pc.expandPerms(Arrays.asList(C1));
    assertEquals(5, rs.size());
    assertTrue(rs.contains(C1));
    assertTrue(rs.contains(C2));
    assertTrue(rs.contains(P1));
    assertTrue(rs.contains(S1));
    assertTrue(rs.contains(S2));
  }

  @Test
  public void testFulPerms() {
    Map<String, Permission> fullPermMap = new HashMap<>();
    Permission p1 = new Permission();
    p1.setPermissionName(P1);
    p1.setDisplayName(P1);
    fullPermMap.put(P1, p1);
    PermCache pc = new PermCache(null, fullPermMap);
    LOGGER.debug(pc.toString());

    Permission p = pc.getFullPerm(P1);
    assertEquals(P1, p.getPermissionName());
    assertEquals(P1, p.getDisplayName());
    assertTrue(p.getSubPermissions().isEmpty());
  }

}
