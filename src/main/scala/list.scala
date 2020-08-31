package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product(xs)
    }

    def tail[A](as: List[A]): List[A] = as match {
        case Nil => Nil
        case Cons(_, t) => t
    }

    @annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = 
        if (n > 0) l match {
            case Nil => Nil
            case Cons(_, t) => drop(t, n - 1)
        }
        else l

    @annotation.tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(a, as) => {
            if (f(a)) dropWhile(as, f)
            else l
        }
    }

    def init[A](l: List[A]): List[A] = {
        l match {
            case Nil => Nil
            case Cons(_, Nil) => Nil
            case Cons(x, xs) => Cons(x, init(xs))
        }
    }

    def setHead[A](a: A, as: List[A]): List[A] = as match {
        case Nil => Cons(a, Nil)
        case Cons(_, t) => Cons(a, t)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}