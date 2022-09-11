package org.justinhj

import scala.annotation.tailrec

 sealed trait LazyList[+A] {
    def head: A
    def tail: LazyList[A]

    def isEmpty: Boolean

    def headOption = if (!isEmpty)     
        Some(head) 
      else 
        None

    @tailrec
    final def forEach(f: A => Unit): Unit = {
      if (!isEmpty) { 
        f(head)
        tail.forEach(f)
      }
    }

    def take(n: Int): LazyList[A] = {
      if (n == 0 || isEmpty) {
        LazyList.empty
      } 
      else LazyList.cons(head, tail.take(n - 1))
    }

    def zip[B](other: LazyList[B]): LazyList[(A, B)] = {
      if (isEmpty || other.isEmpty) LazyList.empty
      else LazyList.cons((head, other.head), tail.zip(other.tail))
    }

    def map[B](f: A => B): LazyList[B] =
      if (isEmpty) LazyList.empty
      else LazyList.cons(f(head), tail.map(f))

    def dropWhile(f: A => Boolean): LazyList[A] =
      if (isEmpty) LazyList.empty
      else if (f(head)) tail.dropWhile(f)
      else this

    def filter(f: A => Boolean): LazyList[A] = {
      val dropped = this.dropWhile(a => !f(a))
      if (dropped.isEmpty) LazyList.empty
      else LazyList.cons(dropped.head, dropped.tail.filter(f))
    }

    @tailrec
    final def foldLeft[B](z: B)(f: (B, A) => B): B = {
      if (isEmpty) z
      else tail.foldLeft(f(z, head))(f)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
       if (isEmpty) z
       else
         f(head, tail.foldRight(z)(f))
    }

    // Another way to do this is this.filter(f).headOption
    def first(f: A => Boolean) : Option[A] = {
      if (isEmpty) None
      else foldRight(None: Option[A]) {
        (a, b) =>
          println(s"observe $a")
          if (f(a))
            Some(a)
          else
            b
      }
    }

    def partition(f: A => Boolean): (LazyList[A],LazyList[A]) = {
      (filter(f), filter(a => !f(a)))
    }
  }

  object LazyList {
    val empty = new LazyList[Nothing] {
      def head = throw new NoSuchElementException("No head of empty")
      def tail = throw new UnsupportedOperationException("No tail of empty")
      def isEmpty = true
    }

    def cons[A](hd: => A, tl: => LazyList[A]) = new LazyList[A] {
      lazy val head = hd
      lazy val tail = tl

      def isEmpty = false
    }

    def apply[A](as: A*): LazyList[A] = {
      if (as.isEmpty) LazyList.empty
      else cons(as.head, apply(as.tail: _*))
    }

    def repeat[A](a: A): LazyList[A] = LazyList.cons(a, repeat(a))

    def from(n: Int) : LazyList[Int] = LazyList.cons(n, from(n+1))

  }
    // Note: right associative extension methods need to swap the parameters
    // see https://docs.scala-lang.org/scala3/reference/contextual/right-associative-extension-methods.html
    // extension [A](hd: => A)
    //   def #::(tl: => LazyList[A]): LazyList[A] =
    //     LazyList.cons(hd, tl)

  // Note: without extension methods this would have been written:
  //  class Deferrer[A](tl: => LazyList[A]) {
  //    def #::(hd: A): LazyList[A] =
  //      LazyList.cons(hd, tl)
  //  }
  //
  //  implicit def toDeferrer[A](l: => LazyList[A]): Deferrer[A] =
  //    new Deferrer[A](l)

  // object #:: {
  //   def unapply[A](s: LazyList[A]): Option[(A, LazyList[A])] =
  //     if (!s.isEmpty) Some((s.head, s.tail)) else None
  // }