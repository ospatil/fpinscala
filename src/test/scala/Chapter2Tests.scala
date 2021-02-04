import org.junit.Test
import org.junit.Assert._

class Chapter2Tests:
    @Test def factorialTest() =
        assertEquals(6, factorial(3))

    @Test def fibTest() =
        // 0, 1, 1, 2, 3, 5, 8 ...
        assertEquals(2, fib(3))
        assertEquals(8, fib(6))

    @Test def findFirstTest() =
        assertEquals(1, findFirst(Array(1, 2, 3), x => x == 2))
        assertEquals(2, findFirst(Array(1, 2, 3), x => x == 3))
        assertEquals(-1, findFirst(Array(1, 2, 3), x => x == 5))

        assertEquals(1, findFirst(Array("a", "b", "c"), x => x == "b"))

    @Test def isSortedTest() =
        assertEquals(true, isSorted(Array(1, 2, 3), (x, y) => x <= y))
        assertEquals(false, isSorted(Array(1, 4, 3), (x, y) => x <= y))
        assertEquals(true, isSorted(Array("a", "b", "c"), (x, y) => x <= y))

