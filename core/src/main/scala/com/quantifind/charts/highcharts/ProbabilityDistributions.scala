package com.quantifind.charts.highcharts

import com.quantifind.charts.highcharts.Highchart._
import org.apache.commons.math3.util.ArithmeticUtils

/**
 * User: austin
 * Date: 2/2/15
 */
object ProbabilityDistributions {

  def bernoulli(p: Double, min: Int = Int.MaxValue, max: Int = Int.MinValue) = {
    binomial(1, p, min, max)
  }

  def binomial(n: Int, p: Double, min: Int = 0, max: Int = Int.MinValue) = {
    val stddev = math.sqrt(n*p*(1-p))
    val left = min
    val right = if(max != Int.MinValue) max else math.ceil(n*p + 3*stddev).toInt
    def binomialPoint(k: Int) = {
      (math.pow(p, k) * math.pow(1 - p, n-k)) * ArithmeticUtils.binomialCoefficient(n, k)
    }
    val data = (left to right).map(p => p -> binomialPoint(p))
    Highchart(Series(data, chart = SeriesType.scatter, name = s"binomial($n, $p)"))
  }

  // Hui convinced me that the convention is stddev for gaussian and variance for normal, but I don't know if that's true
  def gaussian(mean: Double, stddev: Double, min: Double = Double.MaxValue, max: Double = Double.MinValue, points: Int = 1000) = { // todo number of points? and center points at mean or 0?
  // todo check input
  val rootTwoPi = math.sqrt(2*math.Pi)
    val twoVariance = 2 * stddev * stddev
    def gaussianPoint(p: Double) = {
      (1 / (stddev*rootTwoPi)) * math.pow(math.E, - math.pow(p - mean, 2) / twoVariance)
    }
    val left = if(min != Double.MaxValue) min else mean - 3*stddev
    val right = if(max != Double.MinValue) max else mean + 3*stddev
    val stepSize = (right - left) / points
    val data = (left to right by stepSize).map(p => p -> gaussianPoint(p))
    Highchart(Series(data, chart = SeriesType.line, name = s"gaussian($mean, $stddev)"))
  }

  def normal(mean: Double, variance: Double, min: Double = Double.MaxValue, max: Double = Double.MinValue, points: Int = 1000) =
    gaussian(mean, math.sqrt(variance), min, max, points)

  def poisson(lambda: Int, min: Int = 0, max: Int = Int.MinValue): Highchart = {
    val stddev = math.sqrt(lambda)
    val eToTheNegativeLambda = math.pow(math.E, -lambda)
    val left = min
    val right = if(max != Int.MinValue) max else math.ceil(lambda + 3*stddev).toInt
    def poissonPoint(k: Int) = { // Double
      math.pow(lambda, k) * eToTheNegativeLambda / ArithmeticUtils.factorial(k)
    }
    val data = (left to right).map(k => k -> poissonPoint(k))
    Highchart(Series(data, chart = SeriesType.scatter, name = s"poisson($lambda)"))
  }

  def zipf(N: Int, s: Double) = {
    zipfMandelbrot(N, s, 0)
  }

  def zipfMandelbrot(N: Int, s: Double, q: Double) = {
    val left = 0
    val right = N
    val harmonicConstant = (left to right).map(n => math.pow(n+q, -s)).sum
    def zipfMandelbrotPoint(k: Int) = {
      1 / (harmonicConstant * math.pow(k+q, s))
    }
    val data = (left to right).map(x => x -> zipfMandelbrotPoint(x))
    Highchart(Series(data, chart = SeriesType.scatter, name = s"zipfMandelbrot($s, $N, $q)"))
  }
}
