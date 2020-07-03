package org.folio.rest.impl;

import java.util.Arrays;
import java.util.List;
import org.junit.Assert;
import org.junit.Test;

public class PermsAPITest {

  @Test
  public void testSplitStringList() {
    List<List<String>> lists;

    lists = PermsAPI.splitStringList(Arrays.asList(), 1);
    Assert.assertEquals(0, lists.size());

    lists = PermsAPI.splitStringList(Arrays.asList("a"), 2);
    Assert.assertEquals(1, lists.size());
    Assert.assertEquals(Arrays.asList("a"), lists.get(0));

    lists = PermsAPI.splitStringList(Arrays.asList("a", "b"), 2);
    Assert.assertEquals(1, lists.size());
    Assert.assertEquals(Arrays.asList("a", "b"), lists.get(0));

    lists = PermsAPI.splitStringList(Arrays.asList("a", "b", "c"), 2);
    Assert.assertEquals(2, lists.size());
    Assert.assertEquals(Arrays.asList("a", "b"), lists.get(0));
    Assert.assertEquals(Arrays.asList("c"), lists.get(1));

    lists = PermsAPI.splitStringList(Arrays.asList("a", "b", "c", "d"), 2);
    Assert.assertEquals(2, lists.size());
    Assert.assertEquals(Arrays.asList("a", "b"), lists.get(0));
    Assert.assertEquals(Arrays.asList("c", "d"), lists.get(1));

  }

}
