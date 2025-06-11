package scala.lab04

import u04.datastructures.Sequences.*
import Sequence.*

import javax.xml.crypto.NodeSetData
import scala.collection.immutable.TreeSet

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
    import scala.collection.immutable.HashSet
    // Not the best tree-like structure, but HashSet are backed by Hash Array Mapped Trie
    // quite good for general purpose sets. TreeSets could be used by they require
    // the generic type to have some kind of Ordering.
    opaque type Set[A] = HashSet[A]

    override def empty[A](): Set[A] = HashSet.empty

    extension [A](s: Set[A])
      override def add(element: A): Set[A] = s + element

      override def contains(a: A): Boolean = s.contains(a)

      override def union(other: Set[A]): Set[A] = s.union(other)

      override def intersection(other: Set[A]): Set[A] = s.intersect(other)

      override def remove(a: A): Set[A] = s.filter(_ != a)

      override def toSequence(): Sequence[A] =
        if s.isEmpty then Nil()
        else
          val head = s.head
          val tail = s - head
          Cons(head, tail.toSequence())

      override def size(): Int = s.size

      override def ===(other: Set[A]): Boolean =
        s.union(other).size() == s.size()


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
  val setADT: SetADT = BasicSetADT
  import setADT.*

  val s1: Set[Int] = empty().add(10).add(20).add(30)
  val s2: Set[Int] = empty().add(10).add(11)
  // val s3: Set[Int] = Cons(10, Nil()) // because Set is defined opaque
  println(s1.toSequence()) // (10, 20, 30)
  println(s2.toSequence()) // (10, 11)
  println(s1.union(s2).toSequence()) // (10, 20, 30, 11)
  println(s1.intersection(s2).toSequence()) // (10)