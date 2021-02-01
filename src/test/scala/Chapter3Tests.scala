import org.junit.Test
import org.junit.Assert._
import List._

class Chapter3Tests:
  @Test def tailTest(): Unit =
    assertEquals(List(2, 3), tail(List(1, 2, 3)))

  @Test(expected = classOf[RuntimeException]) def tailTestException(): Unit =
    tail(Nil)

  @Test def setHeadTest(): Unit =
    assertEquals(List(0, 2), setHead(List(1, 2), 0))

  @Test def dropTest(): Unit =
    assertEquals(List(2, 3), drop(List(1, 2, 3), 1))
    assertEquals(List(1, 2, 3), drop(List(1, 2, 3), 0))
    assertEquals(Nil, drop(List("a", "b"), 2))
    assertEquals(Nil, drop(List(1, 2), 3))
    assertEquals(Nil, drop(Nil, 1))

  @Test def dropWhileTest(): Unit =
    assertEquals(List(2, 3), dropWhile(List(1, 2, 3), x => x < 2))
    assertEquals(List(1, 2, 3), dropWhile(List(1, 2, 3), x => x > 2))
    assertEquals(Nil, dropWhile(List(1, 2, 3), x => x > 0))
    assertEquals(Nil, dropWhile(Nil, (x: Int) => x > 0)) // Explicit type needed for x
    assertEquals(List(3, 1, 2), dropWhile(List(3, 1, 2), x => x < 0))

  @Test def initTest(): Unit =
    assertEquals(List(1, 2), init(List(1, 2, 3)))
    assertEquals(List(1), init(List(1, 2)))
    assertEquals(Nil, init(List(1)))

  @Test(expected = classOf[RuntimeException]) def initTestException(): Unit =
    init(Nil)