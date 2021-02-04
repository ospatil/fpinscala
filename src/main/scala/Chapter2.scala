// Listing 2.4.1
def factorial(n: Int): Int =
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
        if (n <= 0) acc
        else go(n - 1, n * acc)
    go(n, 1)

// Exercise 2.1
def fib(n: Int): Int =
    @annotation.tailrec
    def go(n: Int, acc: Int, cur: Int): Int =
        if (n == 0) acc
        else go(n - 1, cur, acc + cur)
    go(n, 0, 1)

// Listing 2.4 polymorphic function to find element in array
def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
        if (n >= as.length) -1
        else if (p(as(n))) n
        else loop(n + 1)
    loop(0)

// Exercise 2.2
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    @annotation.tailrec
    def loop(n: Int): Boolean =
        if (n == (as.length - 2)) ordered(as(n), as(n+1))
        else if (ordered(as(n), as(n+1))) loop(n+1)
        else false
    loop(0)

// Listing 2.6
def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

// Exercise 2.3
def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

// Exercise 2.4
def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

// Exercise 2.5
def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
