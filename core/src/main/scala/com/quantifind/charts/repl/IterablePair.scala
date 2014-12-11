package com.qf.charts.repl

/**
 * User: austin
 * Date: 12/2/14
 *
 * Uses the magnet pattern to resolve that Iterable with PartialFunction should be treated like
 * Iterable instead of PartialFunction, since I want a method with the same name + type signature
 * to work on both Iterable and Functions
 *
 * */
trait IterablePair[A, B, C, D] {
  def toIterables: (Iterable[C], Iterable[D])
}

class IterableIterable[A: Numeric, B: Numeric](a: Iterable[A], b: Iterable[B]) extends IterablePair[Iterable[A], Iterable[B], A, B] {
  def toIterables: (Iterable[A], Iterable[B]) = (a, b)
}

class IterableFunction[A: Numeric, B: Numeric](a: Iterable[A], b: A => B) extends IterablePair[Iterable[A], A => B, A, B] {
  def toIterables: (Iterable[A], Iterable[B]) = (a, a.map(b))
}

class FunctionIterable[A: Numeric, B: Numeric](a: B => A, b: Iterable[B]) extends IterablePair[B => A, Iterable[B], A, B] {
  def toIterables: (Iterable[A], Iterable[B]) = (b.map(a), b)
}

trait IterablePairLowerPriorityImplicits {
  implicit def mkIterableFunction[A: Numeric, B: Numeric](ab: (Iterable[A], A => B)): IterablePair[Iterable[A], A => B, A, B] = new IterableFunction(ab._1, ab._2)
  implicit def mkFunctionIterable[A: Numeric, B: Numeric](ab: (B => A, Iterable[B])): IterablePair[B => A, Iterable[B], A, B] = new FunctionIterable(ab._1, ab._2)
}

object IterablePair extends IterablePairLowerPriorityImplicits {
  implicit def mkIterableIterable[A: Numeric, B: Numeric](ab: (Iterable[A], Iterable[B])) = new IterableIterable(ab._1, ab._2)
  implicit def mkIterableIterable[A: Numeric, B: Numeric](ab: (Iterable[(A, B)])) = new IterableIterable(ab.map(_._1), ab.map(_._2))
  implicit def mkIterableIterable[B: Numeric](b: (Iterable[B])) = new IterableIterable((0 until b.size), b)
}