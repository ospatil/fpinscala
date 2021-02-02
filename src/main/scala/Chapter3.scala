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
