package scala.lab04

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.lab04.SetADTs.{OrderedSetADT, TreeSetADT}
import scala.language.postfixOps

abstract class SetADTCheck(name: String) extends Properties(name):
  val setADT: OrderedSetADT
  import setADT.*

  // generating a small Int
  def smallInt(): Gen[Int] = Gen.choose(0, 10)
  // generating a Set of Int with approximate size (modulo clashes)
  def setGen[A: Arbitrary: Ordering](size: Int): Gen[Set[A]] =
    if size == 0
      then Gen.const(empty())
    else for
      a <- Arbitrary.arbitrary[A]
      s <- setGen(size - 1)
    yield s.add(a)
  // a given instance to generate sets with small size
  given arb: Arbitrary[Set[Int]] = Arbitrary:
    for
      i <- smallInt()
      s <- setGen[Int](i)
    yield s

  property("commutativity of union") =
    forAll: (s1: Set[Int], s2: Set[Int]) =>
      (s1 || s2) === (s2 || s1)

  property("commutativity of intersection") =
    forAll: (s1: Set[Int], s2: Set[Int]) =>
      (s1 && s2) === (s2 && s1)

  property("associativity of union") =
    forAll: (s1: Set[Int], s2: Set[Int], s3: Set[Int]) =>
      ((s1 || s2) || s3) === (s1 || (s2 || s3))

  property("associativity of intersection") =
    forAll: (s1: Set[Int], s2: Set[Int], s3: Set[Int]) =>
      ((s1 && s2) && s3) === (s1 && (s2 && s3))

  property("idempotence of union") =
    forAll: (s: Set[Int]) =>
      (s || s) === s

  property("idempotence of intersection") =
    forAll: (s: Set[Int]) =>
      (s && s) === s

  // Axioms of union in relation with intersection
  property("Axiom of distributive law with union and intersection") =
    forAll: (a: Set[Int], b: Set[Int], c: Set[Int]) =>
      (a || (b && c)) === ((a || b) && (a || c))
    &&
      forAll: (a: Set[Int], b: Set[Int], c: Set[Int]) =>
        (a && (b || c)) === ((a && b) || (a && c))

  // Some Mathematical axioms for Sets taken from https://cdn.britannica.com/46/78246-004-10DAA5A8/Zermelo-Fraenkel-axioms.jpg
  property("axiom of extensionality") =
    // If A and B are sets and if, for all x, x \in A if and only if x \in B, then A = B.
    forAll: (s1: Set[Int], s2: Set[Int]) =>
      if s1 === s2 then (s1.toSequence().flatMap(x => s2.remove(x).toSequence()).size == 0)
      else true

  property("axiom of the empty set") =
    forAll: (s: Set[Int]) =>
      s.toSequence().filter(empty[Int]().contains(_)).size == 0

  property("axiom of pairing") =
    forAll: (s1: Set[Int], s2: Set[Int]) =>
      (s1 || s2).size() == s1.size() +  s2.size() - (s1 && s2).size()

  property("axiom of union") =
    forAll: (s1: Set[Int], s2: Set[Int], x: Int) =>
      (s1 || s2).contains(x) == (s1.contains(x) || s2.contains(x))

  /**
    * axioms defining contains based on empty/add:
    * contains(empty, x) = false
    * contains(add(x,s), y) = (x == y) || contains(s, y)
  */
  property("axioms for contains") =
     forAll: (s: Set[Int], x: Int, y:Int) =>
        s.add(x).contains(y) == (x == y) || s.contains(y)
   &&
     forAll: (x: Int) =>
        !empty[Int]().contains(x)

/**
 * axioms defining union and remove:
 * union(empty, s) = s
 * union(add(x, s2), s) = add(x, union(s2, s)
 * remove(x, empty) = empty
 * remove(x, add(x, s)) = remove(x, s)
 * remove(x, add(y, s)) = add(y, remove(x, s)) if x!=y
 *
 * and so on: write axioms and correspondingly implement checks
 */
  property("axioms for union and remove") =
      forAll: (s: Set[Int]) =>
        (s || setADT.empty()) === s
    &&
      forAll: (s1: Set[Int], s2: Set[Int], x: Int) =>
        (s1 || s2.add(x)) === (s1 || s2).add(x)
    &&
      forAll: (x: Int) =>
        setADT.empty[Int]().remove(x) === setADT.empty()
    &&
      forAll: (s: Set[Int], x: Int) =>
        s.add(x).remove(x) === s.remove(x)
    &&
      forAll: (s: Set[Int], x: Int, y: Int) =>
        if x != y then s.add(y).remove(x) === s.remove(x).add(y)
        else true

object BasicSetADTCheck extends SetADTCheck("SequenceBased Set"):
//  val setADT: SetADT = BasicSetADT
  val setADT: OrderedSetADT = TreeSetADT

  @main def visuallingCheckArbitrarySets =
    Range(0,20).foreach(i => println(summon[Arbitrary[setADT.Set[Int]]].arbitrary.sample))
