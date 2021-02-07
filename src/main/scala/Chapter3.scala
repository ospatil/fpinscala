// Listing 3.1

// In scala REPL, Nil and Cons data constructors have be accessed as List.Nil and List.Cons
// Here is what https://docs.scala-lang.org/scala3/book/types-adts-gadts.html says:
// As with normal enum values, the cases of an enum are defined in the enums companion object, so they’re referred to as Option.Some and Option.None (unless the definitions are “pulled out” with an import):
// val ex1: List[Double] = List.Nil
enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])

object List:
    def sum(ints: List[Int]): Int = ints match
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)

    def product(ds: List[Double]): Double = ds match
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

import List._

// Exercise 3.1
val x = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
// The value of x = 3. It will match the 3rd pattern.

// Exercise 3.2
def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t

// Exercise 3.3
def setHead[A](l: List[A], h: A): List[A]= l match
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)

// Exercise 3.4
def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
        l match
            case Nil => Nil
            case Cons(_, t) => drop(t, n - 1)

// Exercise 3.5
def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l

// Listing 3.3.1 Efficiency of data sharing
// The runtime and memory usage are determined only by length of a1
def append[A](a1: List[A], a2: List[A]): List[A] = a1 match
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))

def init[A](l: List[A]): List[A] = l match
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))

// Listing 3.2
def sum(ints: List[Int]): Int = ints match
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)

def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)

/*
 * Generalizing recursion over lists
 * z is the init value of the accumulator and the type B shows that it can be different
 * from list elements.
 * f denotes the action to be carried out recursively. The arguments are list element A
 * and accumulator B.
*/
def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match
    case Nil =>
//        println(s"foldRight case Nil => $z")  
        z
    case Cons(h, t) =>
//        println(s"foldRight case Cons(h, t) => h = $h, t = $t")  
        f(h, foldRight(t, z)(f))

// Let's write sum and product using foldRight
def sum2(as: List[Int]) =
    foldRight(as, 0)((x, y) => x + y)

def product2(as: List[Double]) =
    foldRight(as, 1.0)(_ * _)

// Exercise 3.7
// It's not possible since foldRight has to traverse the whole list before it
// starts applying our function.

// Exercise 3.8
//val lst = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
// lst will be the original list. So basically z (init) is Nil data
// constructor while f (operator) is Cons.

// Exercise 3.9
def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

// Exercise 3.10
// Part 1 - Convince yourself foldRight is not tail-recursive - check
// out the foldRightNonTailRecursiveTest testcase in Chapter3Tests

// Part2
@annotation.tailrec
def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match
    case Nil =>
//        println(s"foldLeft case Nil => $z")
        z
    case Cons(h, t) =>
//        println(s"foldLeft case Cons(h, t) => h = $h, t = $t")
        foldLeft(t, f(z, h))(f)

// Exercise 3.11
def sum3(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

def product3(as: List[Int]): Int =
    foldLeft(as, 1)(_ * _)

def length2(as: List[Int]): Int =
    foldLeft(as, 0)((acc, h) => acc + 1)

// Exercise 3.12
def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

/*
Tracing foldRight for clearer understanding
println(traverse(List(1, 2, 3))) // which is same as foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) in 3.8

input: Cons(1,Cons(2,Cons(3,Nil)))
foldRight case Cons(h, t) => h = 1, t = Cons(2,Cons(3,Nil))
foldRight case Cons(h, t) => h = 2, t = Cons(3,Nil)
foldRight case Cons(h, t) => h = 3, t = Nil
foldRight case Nil => Nil
operator i/p: h = 3, acc = Nil
operator o/p: Cons(3,Nil)
operator i/p: h = 2, acc = Cons(3,Nil)
operator o/p: Cons(2,Cons(3,Nil))
operator i/p: h = 1, acc = Cons(2,Cons(3,Nil))
operator o/p: Cons(1,Cons(2,Cons(3,Nil)))
output: Cons(1,Cons(2,Cons(3,Nil)))
 */
def traverse[A](l: List[A]): List[A] =
    println(s"input: $l")
    def fn(h: A, acc:List[A]) =
        println(s"operator i/p: h = $h, acc = $acc")
        val ret = Cons(h, acc)
        println(s"operator o/p: $ret")
        ret
    foldRight(l, List.Nil: List[A])(fn)

/*
    Tracing foldLeft for clearer understanding
    println(rev(List(1, 2, 3))) // rev is same as reverse in 3.12 with logs added
    
    input: Cons(1,Cons(2,Cons(3,Nil)))
    foldLeft case Cons(h, t) => h = 1, t = Cons(2,Cons(3,Nil))
    operator i/p: acc = Nil, h = 1
    operator o/p: Cons(1,Nil)
    foldLeft case Cons(h, t) => h = 2, t = Cons(3,Nil)
    operator i/p: acc = Cons(1,Nil), h = 2
    operator o/p: Cons(2,Cons(1,Nil))
    foldLeft case Cons(h, t) => h = 3, t = Nil
    operator i/p: acc = Cons(2,Cons(1,Nil)), h = 3
    operator o/p: Cons(3,Cons(2,Cons(1,Nil)))
    foldLeft case Nil => Cons(3,Cons(2,Cons(1,Nil)))
    output: Cons(3,Cons(2,Cons(1,Nil)))
*/
def rev[A](l: List[A]): List[A] =
    println(s"input = $l")
    def fn(acc: List[A], h: A) =
        println(s"operator i/p: acc = $acc, h = $h")
        val ret = Cons(h, acc)
        println(s"operator o/p: $ret")
        ret
    foldLeft(l, List[A]())(fn)

// Exercise 3.13
def foldRightAsLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match
    case Nil => z
    case Cons(h, t) => f(h, foldRightAsLeft(t, z)(f))