package com.qf.charts

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import com.qf.charts.highcharts._
import Highchart._

/**
 * User: austin
 * Date: 10/4/13
 */
class HighchartTest extends FunSuite with ShouldMatchers {

  test("Single point Highchart to json") {
    val hc = Highchart(Seq(Series(Seq(Data(1, 2)))), chart = Chart(zoomType = Zoom.xy), yAxis = None).toServiceFormat

//    hc._2("x")should be (0)

    hc should be ("highcharts",
      Map(
        "series" -> List(Map("data" -> List(Map("x" -> 1, "y" -> 2)))),
        "chart" -> Map("zoomType" -> "xy"),
        "exporting" -> Map("filename" -> "chart"),
        "plotOptions" -> Map(
          "line" -> Map("turboThreshold" -> "Infinity--")
        ),
        "credits" -> Map(
          "href" -> "",
          "text" -> ""
        ),
        "title" -> Map("text" -> "")
      )
    )
  }
}
