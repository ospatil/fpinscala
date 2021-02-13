import org.junit.Test
import org.junit.Assert._
import List._

class Chapter3Tests:
    @Test def tailTest()=
        assertEquals(List(2, 3), tail(List(1, 2, 3)))

    @Test(expected = classOf[RuntimeException]) def tailTestException(): Unit =
        tail(Nil)

    @Test def setHeadTest() =
        assertEquals(List(0, 2), setHead(List(1, 2), 0))

    @Test def dropTest() =
        assertEquals(List(2, 3), drop(List(1, 2, 3), 1))
        assertEquals(List(1, 2, 3), drop(List(1, 2, 3), 0))
        assertEquals(Nil, drop(List("a", "b"), 2))
        assertEquals(Nil, drop(List(1, 2), 3))
        assertEquals(Nil, drop(Nil, 1))

    @Test def dropWhileTest() =
        assertEquals(List(2, 3), dropWhile(List(1, 2, 3), x => x < 2))
        assertEquals(List(1, 2, 3), dropWhile(List(1, 2, 3), x => x > 2))
        assertEquals(Nil, dropWhile(List(1, 2, 3), x => x > 0))
        assertEquals(Nil, dropWhile(Nil, (x: Int) => x > 0)) // Explicit type needed for x
        assertEquals(List(3, 1, 2), dropWhile(List(3, 1, 2), x => x < 0))

    @Test def initTest() =
        assertEquals(List(1, 2), init(List(1, 2, 3)))
        assertEquals(List(1), init(List(1, 2)))
        assertEquals(Nil, init(List(1)))

    @Test(expected = classOf[RuntimeException]) def initTestException(): Unit =
        init(Nil)

    @Test def sum2Test() =
        assertEquals(6, sum2(List(1, 2, 3)))

    @Test def product2Test() =
        assertEquals(6.0, product2(List(1.0, 2.0, 3.0)), 0.5) // 0.5 is delta required for float and double comparisons
    @Test def lengthTest() =
        assertEquals(3, length(List(1, 2, 3)))
        assertEquals(0, length(Nil))

    @Test(expected = classOf[StackOverflowError]) def foldRightNonTailRecursiveTest(): Unit =
        val l = collection.immutable.List.range(0, 10_000)
        val lc = List.apply(l: _*) // splat the scala list into our list
        length(lc) // This will cause stack overflow since foldRight is not tail-recursive

    @Test def sum3Test() =
        assertEquals(6, sum3(List(1, 2, 3)))

    @Test def product3Test() =
        assertEquals(6, product3(List(1, 2, 3)))

    @Test def length2Test() =
        assertEquals(3, length2(List(1, 2, 3)))
        assertEquals(0, length2(Nil))

    @Test def reverseTest() =
        assertEquals(List(3, 2, 1), reverse(List(1, 2, 3)))
  
    @Test def appendViaFoldRightTest() =
        assertEquals(List(1, 2, 3, 4, 5), appendViaFoldRight(List(1, 2, 3), List(4, 5)))

//    @Test def addOneTest() =
//        assertEquals(List(2, 3, 4), addOne(List(1, 2, 3)))