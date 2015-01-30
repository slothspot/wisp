package com.quantifind.charts.highcharts

/**
 * User: austin
 * Date: 12/12/14
 */
object PlotOptions {
  val name = "plotOptions"
}

// These are overlapping options, but are well over 22 options.
case class PlotOptions(
                        area: Option[PlotOptionKey] = None,
                        arearange: Option[PlotOptionKey] = None,
                        areaspline: Option[PlotOptionKey] = None,
                        areasplinerange: Option[PlotOptionKey] = None,
                        bar: Option[PlotOptionKey] = None,
                        boxplot: Option[PlotOptionKey] = None,
                        bubble: Option[PlotOptionKey] = None,
                        column: Option[PlotOptionKey] = None,
                        columnrange: Option[PlotOptionKey] = None,
                        errorbar: Option[PlotOptionKey] = None,
                        funnel: Option[PlotOptionKey] = None,
                        gauge: Option[PlotOptionKey] = None,
                        heatmap: Option[PlotOptionKey] = None,
                        line: Option[PlotOptionKey] = None,
                        pie: Option[PlotOptionKey] = None,
                        pyramid: Option[PlotOptionKey] = None,
                        scatter: Option[PlotOptionKey] = None,
                        series: Option[PlotOptionKey] = None,
                        solidgauge: Option[PlotOptionKey] = None,
                        spline: Option[PlotOptionKey] = None,
                        waterfall: Option[PlotOptionKey] = None,
                        var __name: String = PlotOptions.name
                        ) extends HighchartKey(__name) {

  def toServiceFormat: Map[String, Any] = {
    Map(
      "area" -> area,
      "arearange" -> arearange,
      "areaspline" -> areaspline,
      "areasplinerange" -> areasplinerange,
      "bar" -> bar,
      "boxplot" -> boxplot,
      "bubble" -> bubble,
      "column" -> column,
      "columnrange" -> columnrange,
      "errorbar" -> errorbar,
      "funnel" -> funnel,
      "gauge" -> gauge,
      "heatmap" -> heatmap,
      "line" -> line,
      "pie" -> pie,
      "pyramid" -> pyramid,
      "scatter" -> scatter,
      "series" -> series,
      "solidgauge" -> solidgauge,
      "spline" -> spline,
      "waterfall" -> waterfall
    ).flatMap(HighchartKey.flatten)
  }
}

case class PlotOptionKey( // todo - many more fields
                         borderWidth: Option[Int] = None,
                         groupPadding: Option[Int] = None,
                         pointPadding: Option[Int] = None,
                         stacking: Option[Stacking.Type] = None
                          ) {
  def toServiceFormat = Map(
    "borderWidth" -> borderWidth,
    "groupPadding" -> groupPadding,
    "pointPadding" -> pointPadding,
    "stacking" -> stacking
  ).flatMap(HighchartKey.flatten)
}
