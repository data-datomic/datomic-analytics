package org.benf.cfr.tests;

/**
 * Created by IntelliJ IDEA.
 * User: lee
 * Date: 05/05/2011
 * Time: 06:28
 * To change this template use File | Settings | File Templates.
 */
public class ArrayTest2 {
    int x;
    int y;
    int z;

    void test1(int a, int b) {
        String[][] r;
        r = new String[a][];
        r[1] = new String[b];
    }

}
