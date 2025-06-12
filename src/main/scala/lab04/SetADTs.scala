package scala.lab04

import u04.datastructures.Sequences.*
import Sequence.*

import scala.collection.immutable
import scala.math.Ordering.comparatorToOrdering

object SetADTs:
  
  trait SetADT:
    type Set[A]
    def empty[A](): Set[A]
    extension [A](s: Set[A])
      def add(element: A): Set[A]
      def contains(a: A): Boolean
      def union(other: Set[A]): Set[A]
      def intersection(other: Set[A]): Set[A]
      infix def ||(other: Set[A]): Set[A] = s.union(other)
      infix def &&(other: Set[A]): Set[A] = s.intersection(other)
      def remove(a: A): Set[A]
      def toSequence(): Sequence[A]
      def size(): Int
      def ===(other: Set[A]): Boolean

  object TreeSetADT extends SetADT:
    import HashTrees.*

    opaque type Set[A] = Tree[A]

    override def empty[A](): Set[A] = EmptyHashTree

    extension [A](s: Set[A])
      override def add(element: A): Set[A] = s.insert(element)
      override def contains(element: A): Boolean = s.contains(element)
      override def union(other: Set[A]): Set[A] =
        other.toList.foldLeft(s)(_.insert(_))
      override def intersection(other: Tree[A]): Tree[A] =
        s.filter(other.contains(_))
      override def remove(a: A): Tree[A] = s.filter(_ != a)
      override def toSequence(): Sequence[A] = s.toList.toSequence
      override def size(): Int = s.size
      override def ===(other: Tree[A]): Boolean =
        val interSize = s.intersection(other).size
        interSize == s.size && interSize == other.size

    extension[A] (l: List[A])
      def toSequence: Sequence[A] = l match
        case h :: t => Cons(h, t.toSequence)
        case immutable.Nil => Nil()

  object BasicSetADT extends SetADT:

    opaque type Set[A] = Sequence[A]

    def empty[A](): Set[A] = Nil()

    extension [A](s: Set[A])
      def add(element: A): Set[A] = s match
        case Cons(h, _) if h == element => s
        case Cons(h, t)  => Cons(h, t.add(element))
        case _ => Cons(element, Nil())

      def remove(a: A): Set[A] = s.filter(_ != a)  

      def contains(a: A): Boolean = s match
        case Cons(h, t) => h == a || t.contains(a)
        case Nil() => false

      def toSequence(): Sequence[A] = s

      def union(s2: Set[A]): Set[A] = s2 match
        case Cons(h, t) => Cons(h, s.remove(h).union(t))
        case Nil() => s

      def intersection(s2: Set[A]): Set[A] = s match
        case Cons(h, t) if s2.contains(h) => Cons(h, t.intersection(s2.remove(h)))
        case Cons(_, t) => t.intersection(s2)
        case Nil() => Nil()

      def size(): Int = s match
        case Cons(_, t) => 1 + t.size()
        case Nil() => 0

      def ===(other: Set[A]): Boolean =
        s.union(other).size() == s.size()


@main def trySetADTModule =
  import SetADTs.*
  val setADT: SetADT = TreeSetADT
  import setADT.*

  val s1: Set[Int] = empty().add(10).add(20).add(30)
  val s2: Set[Int] = empty().add(10).add(11)
  // val s3: Set[Int] = Cons(10, Nil()) // because Set is defined opaque
  println(s1.toSequence()) // (10, 20, 30)
  println(s2.toSequence()) // (10, 11)
  println(s1.union(s2).toSequence()) // (10, 20, 30, 11)
  println(s1.intersection(s2).toSequence()) // (10)