package scala.lab04

import scala.math.Ordered.orderingToOrdered

trait Tree[+A]:
  def insert[B >: A](value: B): Tree[B]
  def contains[B >: A](value: B): Boolean
  def filter(p: A => Boolean): Tree[A]
  def toList: List[A]
  def size: Int
  def isEmpty: Boolean

object HashTrees:
  case object EmptyHashTree extends Tree[Nothing]:
    override def insert[A](value: A): Tree[A] = HashLeaf(value)
    override def contains[A](value: A): Boolean = false
    override def filter(p: Nothing => Boolean): Tree[Nothing] = EmptyHashTree
    override def toList: List[Nothing] = Nil
    override def size: Int = 0
    override def isEmpty: Boolean = true

  case class HashLeaf[A](value: A) extends Tree[A]:
    override def insert[B >: A](newValue: B): Tree[B] =
      if (newValue == value) this
      else HashBranch(Map(value.hashCode -> List(value), newValue.hashCode -> List(newValue)))

    override def contains[B >: A](elem: B): Boolean =
      elem == value

    override def filter(p: A => Boolean): Tree[A] =
      if (p(value)) this else EmptyHashTree

    override def toList: List[A] = List(value)
    override def size: Int = 1
    override def isEmpty: Boolean = false

  case class HashBranch[A](buckets: Map[Int, List[A]]) extends Tree[A]:
    override def insert[B >: A](newValue: B): Tree[B] =
      val hash = newValue.hashCode
      buckets.get(hash) match
        case Some(b) if b.contains(newValue) => this
        case Some(b) => HashBranch(buckets + (hash -> (newValue :: b)))
        case _       => HashBranch(buckets + (hash -> List(newValue)))

    override def contains[B >: A](elem: B): Boolean =
      buckets.get(elem.hashCode) match
        case None => false
        case Some(b) => b.contains(elem)

    override def filter(p: A => Boolean): Tree[A] =
      val filteredBuckets = buckets.view.mapValues(_.filter(p)).filter(_._2.nonEmpty).toMap
      if filteredBuckets.isEmpty then EmptyHashTree else HashBranch(filteredBuckets)

    override def toList: List[A] = buckets.values.flatten.toList
    override def size: Int = buckets.values.map(_.size).sum
    override def isEmpty: Boolean = buckets.isEmpty