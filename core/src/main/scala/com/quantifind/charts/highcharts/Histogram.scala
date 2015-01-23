package com.quantifind.charts.highcharts

/**
 * User: austin
 * Date: 12/20/14
 */
object Histogram {

  def histogram(data: Seq[Double], numBins: Int): Highchart = {

    val (min, max) = (data.min, data.max)
    val binWidth = ((max+1) - min) / numBins.toDouble

    // This strategy risks short-changing the last bin - perhaps there is a more fair way to do it?
    def toBin(d: Double) = ((d - min) / binWidth).toInt

    val binCounts = data.map(toBin).groupBy(identity).mapValues(_.size).toSeq.sortBy(_._1)

    // Does not rely on implicit imports - use import Highchart._ in an application!
    val series = Series(binCounts.map{case(bin, count) => Data(bin, count)}, chart = Some(SeriesType.column))
    val plotOptions = Some(PlotOptions(series = Some(PlotOptionKey(
      groupPadding = Some(0),
      pointPadding = Some(0)
    ))))

    val xAxis = Some(Array(Axis(labels = Some(AxisLabel(rotation = Some(-45))), categories = Some((min to max by binWidth).map { bin =>
      val end = bin+binWidth
      f"$bin%.2f-$end%.2f"
    }.toArray))))

    val hc = Highchart(series = Seq(series), plotOptions = plotOptions, xAxis = xAxis)

    hc
  }

}
