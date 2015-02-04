package com.quantifind.charts.highcharts

/**
 * User: austin
 * Date: 12/12/14
 */
object PlotOptions {
  val name = "plotOptions"
}

object BoxplotPlotOptions {
  def apply(
     allowPointSelect: Option[Boolean] = None,
     color: Option[Color.Type] = None,
     colorByPoint: Option[Boolean] = None,
     colors: Option[Array[Color.Type]] = None,
     cursor: Option[Boolean] = None,
     enableMouseTracking: Option[Boolean] = None,
     events: Option[Events] = None,
     fillColor: Option[Color.Type] = None,
     groupPadding: Option[Double] = None,
     lineWidth: Option[Int] = None,
     linkedTo: Option[String] = None,
     mediaWidth: Option[Int] = None,
     medianColor: Option[Color.Type] = None,
     point: Option[Point] = None,
     pointInterval: Option[Double] = None,
     pointRange: Option[Int] = None,
     pointWidth: Option[Int] = None,
     selected: Option[Boolean] = None,
     showCheckbox: Option[Boolean] = None,
     showInLegend: Option[Boolean] = None,
     stemColor: Option[Color.Type] = None,
     stemDashStyle: Option[String] = None,
     stemWidth: Option[Int] = None,
     stickyTracking: Option[Boolean] = None,
     tooltip: Option[ToolTip] = None,
     visible: Option[Boolean] = None,
     whiskerColor: Option[Color.Type] = None,
     whiskerLength: Option[Double] = None,
     whiskerWidth: Option[Int] = None
  ): BoxplotPlotOptions = {
    val bpo = new BoxplotPlotOptions()
    bpo.allowPointSelect = allowPointSelect
    bpo.color = color
    bpo.colorByPoint = colorByPoint
    bpo.colors = colors
    bpo.cursor = cursor
    bpo.enableMouseTracking = enableMouseTracking
    bpo.events = events
    bpo.fillColor = fillColor
    bpo.groupPadding = groupPadding
    bpo.lineWidth = lineWidth
    bpo.linkedTo = linkedTo
    bpo.mediaWidth = mediaWidth
    bpo.medianColor = medianColor
    bpo.point = point
    bpo.pointInterval = pointInterval
    bpo.pointRange = pointRange
    bpo.pointWidth = pointWidth
    bpo.selected = selected
    bpo.showCheckbox = showCheckbox
    bpo.showInLegend = showInLegend
    bpo.stemColor = stemColor
    bpo.stemDashStyle = stemDashStyle
    bpo.stemWidth = stemWidth
    bpo.stickyTracking = stickyTracking
    bpo.tooltip = tooltip
    bpo.visible = visible
    bpo.whiskerColor = whiskerColor
    bpo.whiskerLength = whiskerLength
    bpo.whiskerWidth = whiskerWidth
    bpo
  }
}

// These are overlapping options, but are well over 22 options.
case class PlotOptions(
                        area: Option[PlotOptionKey] = None,
                        arearange: Option[PlotOptionKey] = None,
                        areaspline: Option[PlotOptionKey] = None,
                        areasplinerange: Option[PlotOptionKey] = None,
                        bar: Option[PlotOptionKey] = None,
                        boxplot: Option[BoxplotPlotOptions] = None,
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

class BoxplotPlotOptions(
                          var color: Option[Color.Type] = None,
                          var colorByPoint: Option[Boolean] = None,
                          var colors: Option[Array[Color.Type]] = None,
                          // depth
                          // edgeColor
                          // edgeWidth
                          var fillColor: Option[Color.Type] = None,
                          var groupPadding: Option[Double] = None,
                          // groupZPadding
                          // grouping
                          var lineWidth: Option[Int] = None,
                          var medianColor: Option[Color.Type] = None,
                          var mediaWidth: Option[Int] = None,
                          // negativeColor
                          var pointInterval: Option[Double] = None,
//                          val pointPlacement: Option[Any] = None // can be null, "on", or "between", or numeric
                          var pointRange: Option[Int] = None,
                          var pointWidth: Option[Int] = None,
                          var selected: Option[Boolean] = None,
                          var showCheckbox: Option[Boolean] = None,
                          var stemColor: Option[Color.Type] = None,
                          var stemDashStyle: Option[String] = None, // Solid, ...?
                          var stemWidth: Option[Int] = None, // can it be a decimal?
                          // turboThreshold
                          var whiskerColor: Option[Color.Type] = None,
                          var whiskerLength: Option[Double] = None, // percentage, can it be a pixel integer?
                          var whiskerWidth: Option[Int] = None // pixel, can it be a percentage?
                          ) extends PlotOptionsBase {
  override def toServiceFormat = {
    super.toServiceFormat ++ Map()
  }
}

trait PlotOptionsBase {
  var allowPointSelect: Option[Boolean] = None
  var cursor: Option[Boolean] = None // actually 'pointer'
  var enableMouseTracking: Option[Boolean] = None
  var events: Option[Events] = None
  var linkedTo: Option[String] = None // link to another series by the series id (<-- not the same as name but we can make it name, probably)
  var point: Option[Point] = None
//  val selected: Option[Boolean] = None // related to showCheckbox for selecting a series, not sure it's a default
  var showInLegend: Option[Boolean] = None
  var stickyTracking: Option[Boolean] = None // If events are defined, whether to maintain event after mouse leaves plot area
  var tooltip: Option[ToolTip] = None // tooltip object
  var visible: Option[Boolean] = None

  def toServiceFormat: Map[String, Any] = Map(
    "allowPointSelect" -> allowPointSelect,
    "cursor" -> cursor,
    "enableMouseTrackinge" -> enableMouseTracking,
    "linkedTo" -> linkedTo,
    "showInLegend" -> showInLegend,
    "stickyTracking" -> stickyTracking,
    "visible" -> visible
  ) ++
    Seq(events, point, tooltip).flatMap(HighchartKey.optionToServiceFormat)
}

case class Point(
                  events: Option[Events] = None
                  ) extends HighchartKey("point") {
  def toServiceFormat = HighchartKey.optionToServiceFormat(events)
}

// TODO: these are all javascript functions.
// Can we do better than embedding javascript in a String?
// events and point.events do not have the same options, but
// it is still folded into a single object
case class Events(
                   afterAnimate: Option[String] = None,
                   checkboxClick: Option[String] = None,
                   click: Option[String] = None,
                   hide: Option[String] = None,
                   legendItemClick: Option[String] = None,
                   mouseOut: Option[String] = None,
                   mouseOver: Option[String] = None,
                   remove: Option[String] = None,
                   select: Option[String] = None,
                   show: Option[String] = None,
                   unselect: Option[String] = None,
                   update: Option[String] = None
                   ) extends HighchartKey("events") {
  def toServiceFormat = Map(
    "afterAnimate" -> afterAnimate,
    "checkboxClick" -> checkboxClick,
    "click" -> click,
    "hide" -> hide,
    "legendItemClick" -> legendItemClick,
    "mouseOut" -> mouseOut,
    "mouseOver" -> mouseOver,
    "remove" -> remove,
    "select" -> select,
    "show" -> show,
    "unselect" -> unselect,
    "update" -> update
  ).flatMap(HighchartKey.flatten)
}

//case class ToolTip(
//                    dateTimeLabelFormats: Option[String] = None,
//                    followPointer: Option[Boolean] = None,
//                    followTouchMove: Option[Boolean] = None,
//                    footerFormat: Option[String] = None,
//                    headerFormat: Option[String] = None,
//                    hideDelay: Option[Int] = None,
//                    pointFormat: Option[String] = None,
//                    shape: Option[Any] = None, // square or callout
//                    valueDecimals: Option[Int] = None,
//                    valuePrefix: Option[String] = None,
//                    valueSuffix: Option[String] = None,
//                    xDateFormat: Option[String] = None
//                    ) extends HighchartKey("tooltip") {
//  def toServiceFormat = Map(
//    "dateTimeLabelFormats" -> dateTimeLabelFormats,
//    "followPointer" -> followPointer,
//    "followTouchMove" -> followTouchMove,
//    "footerFormat" -> footerFormat,
//    "headerFormat" -> headerFormat,
//    "hideDelay" -> hideDelay,
//    "pointFormat" -> pointFormat,
//    "shape" -> shape,
//    "valueDecimals" -> valueDecimals,
//    "valuePrefix" -> valuePrefix,
//    "valueSuffix" -> valueSuffix,
//    "xDateFormat" -> xDateFormat
//  ).flatMap(HighchartKey.flatten)
//}

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
