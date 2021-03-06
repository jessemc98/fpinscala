package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def append[A](as: List[A], bs: List[A]): List[A] = as match {
        case Nil => bs
        case Cons(h, t) => Cons(h, append(t, bs))
    }

    def appendUsingFoldRight[A](as: List[A], bs: List[A]): List[A] =
        foldRight(as, bs)(Cons(_,_))

    def flatten[A](as: List[List[A]]): List[A] =
        foldRight(as, Nil: List[A])(appendUsingFoldRight(_, _))

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def increment(ints: List[Int]): List[Int] =
        foldRight(ints, Nil: List[Int])((h,t) => Cons(h+1, t))

    def doubleToString(ds: List[Double]): List[String] =
        foldRight(ds, Nil: List[String])((h,t) => Cons(h.toString(), t))

    def map[A,B](as: List[A])(f: A => B): List[B] =
        foldRight(as, Nil: List[B])((h,t) => Cons(f(h), t))

    def filter[A](as: List[A])(p: A => Boolean): List[A] =
        foldRight(as, Nil: List[A])((h,t) => 
            if (p(h)) Cons(h, t)
            else t
        )

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
        flatten(map(as)(f))

    def filterUsingFlatMap[A](as: List[A])(p: A => Boolean): List[A] =
        flatMap(as)(x => 
            if (p(x)) List(x)
            else Nil: List[A]
        )
    
    def mergeInts(as: List[Int], bs: List[Int]): List[Int] = (as,bs) match {
        case (_,Nil) => Nil
        case (Nil,_) => Nil
        case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, mergeInts(t1,t2))
    }
    def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = (as,bs) match {
        case (_,Nil) => Nil
        case (Nil,_) => Nil
        case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
        def startsWith(sup: List[A], sub: List[A]): Boolean = {
            (sup, sub) match {
                case (_,Nil) => true
                case (Nil,_) => false
                case (Cons(h1,t1), Cons(h2,t2)) if (h1 == h2) => startsWith(t1,t2) 
                case _ => false
            }
        }

        sup match {
            case Nil => sub == Nil
            case _ if startsWith(sup, sub) => true
            case Cons(h,t) => hasSubsequence(t, sub)
        }
    }

    @annotation.tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    def foldRightUsingFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(reverse(as), z)((b, a) => f(a, b))

    def foldLeftUsingFoldRight[A,B](as: List[A], z: B)(f: (B,A) => B): B =
        foldRight(as, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

    def length[A](as: List[A]): Int =
        foldRight(as, 0)((_, len) => len + 1)

    def sum2(ns: List[Int]) =
        foldRight(ns, 0)(_ + _)

    def product2(ns: List[Double]) =
        foldRight(ns, 1.0)(_ * _)

    def lengthLeft[A](as: List[A]): Int = 
        foldLeft(as, 0)((x, _) => x + 1)

    def sum2Left(ns: List[Int]) =
        foldLeft(ns, 0)(_ + _)

    def product2Left(ns: List[Double]) =
        foldLeft(ns, 1.0)(_ * _)
    
    def reverse[A](as: List[A]) = 
        foldLeft(as, Nil: List[A])((a, b) => Cons(b, a))

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