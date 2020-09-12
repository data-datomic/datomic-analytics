package org.benf.cfr.tests;


import org.benf.cfr.tests.support.Nullable;

/**
 * Created by IntelliJ IDEA.
 * User: lee
 * Date: 05/05/2011
 * Time: 06:28
 * To change this template use File | Settings | File Templates.
 */
public class AnnotationTest3 {

    @Deprecated
    void foo(int x) {
    }

    @AnnotationTestAnnotation(value = {"fred", "jim\u1000"}, fred = 1)
//    @AnnotationTestAnnotation2("fred")
    void foo(int x, @Nullable Double y) {
        System.out.println("Foo!");
    }

    @AnnotationTestAnnotation({"fred", "jim\u800d"})
//    @AnnotationTestAnnotation2("fred")
    static void foo2(int x, @Nullable /*@AnnotationTestAnnotation2("pants") */ @Deprecated Double y) {
        System.out.println("Foo!");
    }

}
