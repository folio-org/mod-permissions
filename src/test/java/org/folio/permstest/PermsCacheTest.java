package org.folio.permstest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.folio.rest.impl.PermsCache.PermCache;
import org.folio.rest.impl.PermsCache.PgStat;
import org.junit.Test;

public class PermsCacheTest {

  private static final String P1 = "perm-1";
  private static final String S1 = "sub-1";
  private static final String S2 = "sub-2";
  private static final String S21 = "sub-2-1";
  private static final String S22 = "sub-2-2";
  private static final String S221 = "sub-2-2-1";
  private static final String S222 = "sub-2-2-2";

  @Test
  public void testPermsCache() {
    PgStat pgStat = new PgStat(1, 2, 3);
    Map<String, Set<String>> subPermMap = new HashMap<>();
    subPermMap.put(P1, new HashSet<>(Arrays.asList(S1, S2)));
    subPermMap.put(S2, new HashSet<>(Arrays.asList(S21, S22)));
    subPermMap.put(S22, new HashSet<>(Arrays.asList(S221, S222)));
    PermCache pc = new PermCache(pgStat, subPermMap);

    assertFalse(pc.isStale());
    assertEquals(new PgStat(1, 2, 3), pgStat);
    assertNotEquals(new PgStat(2, 2, 3), pgStat);

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
}
