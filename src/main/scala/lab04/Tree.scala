package scala.lab04

import scala.math.Ordered.orderingToOrdered

trait Tree[A: Ordering]:
  def insert(value: A): Tree[A]
  def contains(value: A): Boolean
  def filter(p: A => Boolean): Tree[A]
  def toList: List[A]
  def size: Int
  def isEmpty: Boolean


object BSTs:
  case class EmptyNode[A: Ordering]() extends Tree[A]:
    def insert(value: A): Tree[A] = Node(value, EmptyNode(), EmptyNode())
    def contains(value: A): Boolean = false
    def filter(p: A => Boolean): Tree[A] = this
    def toList: List[A] = List.empty
    def size: Int = 0
    def isEmpty: Boolean = true

  case class Node[A: Ordering](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]:
    def insert(newValue: A): Tree[A] =
      newValue match
        case `value` => this
        case v if v < value => Node(value, left.insert(newValue), right)
        case v if v > value => Node(value, left, right.insert(newValue))
      
    def contains(elem: A): Boolean = elem match
      case v if v < value => left.contains(v)
      case v if v > value => right.contains(v)
      case _ => true

    def filter(p: A => Boolean): Tree[A] =
      toList.filter(p).foldLeft(EmptyNode(): Tree[A])(_.insert(_))

    def toList: List[A] = left.toList ++ (value :: right.toList)
    def size: Int = 1 + left.size + right.size
    def isEmpty: Boolean = false