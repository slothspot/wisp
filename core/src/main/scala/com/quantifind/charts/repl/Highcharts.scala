package com.quantifind.charts.repl

import com.quantifind.charts.highcharts.{SeriesType}
import com.quantifind.charts.highcharts.LeastSquareRegression

/**
* User: austin
* Date: 12/2/14
*
* Highcharts implementation of plotting functionality. Includes several highcharts specific plots
*
* I rely on the fact that an implicit method defined in an object takes precedence over one
* defined in a trait to have Iterable[T] with PartialFunction[Int, T] resolve to this method
*/

object Highcharts extends IterablePairLowerPriorityImplicits with HighchartsStyles {

  implicit def mkIterableIterable[A: Numeric, B: Numeric](ab: (Iterable[A], Iterable[B])) = new IterableIterable(ab._1, ab._2)
  implicit def mkIterableIterable[A: Numeric, B: Numeric](ab: (Iterable[(A, B)])) = new IterableIterable(ab.map(_._1), ab.map(_._2))
  implicit def mkIterableIterable[B: Numeric](b: (Iterable[B])) = new IterableIterable((0 until b.size), b)

  def area[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val (xr, yr) = xy.toIterables
    xyToSeries(xr, yr, SeriesType.area)
  }

  def areaspline[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val (xr, yr) = xy.toIterables
    xyToSeries(xr, yr, SeriesType.areaspline)
  }

  def bar[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val (xr, yr) = xy.toIterables
    xyToSeries(xr, yr, SeriesType.bar)
  }

  def column[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val (xr, yr) = xy.toIterables
    xyToSeries(xr, yr, SeriesType.column)
  }

  def line[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val (xr, yr) = xy.toIterables
    xyToSeries(xr, yr, SeriesType.line)
  }

  def pie[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val (xr, yr) = xy.toIterables
    xyToSeries(xr, yr, SeriesType.pie)
  }

  def regression[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    def numericToDouble[X](x: X)(implicit ev: Numeric[X]): Double = ev.toDouble(x)
    val (xr, yr) = xy.toIterables
    LeastSquareRegression.leastSquareRegression(xr.toSeq.map(numericToDouble(_)), yr.toSeq.map(numericToDouble(_)))
  }

  def scatter[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val (xr, yr) = xy.toIterables
    xyToSeries(xr, yr, SeriesType.scatter)
  }

  def spline[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val (xr, yr) = xy.toIterables
    xyToSeries(xr, yr, SeriesType.spline)
  }
}

