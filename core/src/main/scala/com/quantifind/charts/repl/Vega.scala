package com.quantifind.charts.repl

import java.io.{PrintWriter, File}

import com.quantifind.charts.vega._
import com.quantifind.charts.vega.Vega._

import scala.concurrent.Promise

/**
 * User: austin
 * Date: 12/12/14
 */
object Vega extends IterablePairLowerPriorityImplicits with VegaStyles {

  implicit def mkIterableIterable[A: Numeric, B: Numeric](ab: (Iterable[A], Iterable[B])) = new IterableIterable(ab._1, ab._2)
  implicit def mkIterableIterable[A: Numeric, B: Numeric](ab: (Iterable[(A, B)])) = new IterableIterable(ab.map(_._1), ab.map(_._2))
  implicit def mkIterableIterable[B: Numeric](b: (Iterable[B])) = new IterableIterable((0 until b.size), b)

  // TODO, a lot
  // - bar v column
//  def bar
  def bar[A, B, C: Numeric, D: Numeric](xy: IterablePair[A, B, C, D]) = {
    val (xr, yr) = xy.toIterables


    // TODO - the typing problem
    implicit def intToSomeString(num: Int) = Some(num.toString)

    val dataName = "table"

    val vega = com.quantifind.charts.vega.Vega(
      data = Array(
        Data(
          name = dataName,
          values = Some(xr.zip(yr).map{case(x, y) => XYPair(x, y)}.toArray)
        )
      ),
      scales = Array(
        Scale(
          name = "x",
          scaleType = ScaleType.ordinal,
          range = ScaleRangeLiteral.width,
          domain = DataRef(dataName, "data.x") // can we do better than a string here?
        ),
        Scale(
          name = "y",
          range = ScaleRangeLiteral.height,
          //            range = Array(10, 20, 30),
          domain = DataRef(dataName, "data.y"),
          nice = true
        )
      ),
      axes = Array(
        Axis(
          axisType = AxisType.x,
          scale = "x"
        ),
        Axis(
          axisType = AxisType.y,
          scale = "y"
        )
      ),
      marks = Array(
        Mark(
          markType = MarkType.rect,
          from = DataRef(data = dataName),
          properties = Properties(
            enter = Property(
              x = ValueRef(scale = "x", field = "data.x"),
              width = ValueRef(scale = "x", band = true, offset = -1),
              y = ValueRef(scale = "y", field = "data.y"),
              y2 = ValueRef(scale = "y", `value` = 0)
            ),
            update = Property(
              fill = ValueRef(`value` = "steelblue")
            ),
            hover = Property(
              fill = ValueRef(`value` = "red")
            )
          )
        )
      )
    )

    plot(vega)

  }
}

trait VegaStyles extends WebPlotVega {
  override def plot(t: Vega): Vega = {
    val newPlot = t
//      if(isHeld && plots.nonEmpty) {
//        val oldplot = plots.head
//        plots = plots.tail
//        // Throws away things from t besides the series!
//        oldplot.copy(series = oldplot.series ++ t.series)
//      } else t
    super.plot(newPlot)
  }
}

trait WebPlotVega extends WebPlot[Vega] {

  def plotAll(): Unit = {

    val temp = File.createTempFile("vega", ".html")
    val pw = new PrintWriter(temp)

//    val js =
//      s"""
//    |<html>
//    |  <head>
//    |    <title>Vega</title>
//    |    <script src="http://d3js.org/d3.v3.min.js"></script>
//    |    <script src="http://trifacta.github.io/vega/vega.js"></script>
//    |    <script type="text/javascript">${reloadJs}</script>
//    |  </head>
//    |  <body>
//    |    <div id="view"></div>
//    |  </body>
//    |  <script type="text/javascript">
//    |   vg.parse.spec(${plots.head.toJson}, function(chart) {
//    |    var view = chart({el:"#view"})
//    |      .renderer("svg")
//    |      .update();
//    |  });
//    |
//    |  </script>
//    |</html>
//  """.stripMargin

    // TODO look at jquery placement
    val jsHeader =
      s"""
      |<html>
      |  <head>
      |    <title>Vega</title>
      |    <script type="text/javascript" src="http://code.jquery.com/jquery-1.8.2.min.js"></script>
      |    <script src="http://d3js.org/d3.v3.min.js"></script>
      |    <script src="http://trifacta.github.io/vega/vega.js"></script>
      |    <script type="text/javascript">${reloadJs}</script>
      |  </head>
      |  <body>
      """.stripMargin

    def containerDivs(index: Int): String = """<div id="view%s"></div>""".format(index)

    def endBody = "</body>"

    def vegaContainer(json: String, index: Int): String =
      s"""
      |    <script type="text/javascript">
      |   vg.parse.spec($json, function(chart) {
      |    var view = chart({el:"#view%s"})
      |      .renderer("svg")
      |      .update();
      |  });
      |
      |  </script>
      """.stripMargin.format(index.toString)

    val jsFooter =  "</html>"


    pw.print(jsHeader)
    (0 until plots.size).foreach(index => pw.print(containerDivs(index)))
    pw.print(endBody)
    plots.zipWithIndex.foreach{case(plot, index) => pw.print(vegaContainer(plot.toJson, index))}
    pw.print(jsFooter)
    pw.flush()
    pw.close()

    plotServer.foreach{ps =>
      ps.p.success()
      ps.p = Promise[Unit]()
    }

    def link =
      if (serverMode) {
        temp.renameTo(serverRootFile)
        s"http://${java.net.InetAddress.getLocalHost.getCanonicalHostName}:${port}"
      }
      else s"file://$temp"

    openFirstWindow(link)

    // This should be specific to the instance
    println(s"Output written to $link (CMD + Click link in Mac OSX).")
  }

  override def plot(v: Vega): Vega = {
    super.plot(v)
    plots = v +: plots
//    undoStack.push(plots)
    plotAll()
    v
  }

  // TODO major DRY between this and highcharts, also compatability. Establishing POC first.
  def reloadJs =
  //    if (serverMode)
    "$.ajax({url: '/check', dataType: 'jsonp', complete: function(){location.reload()}})"
  //    else _reloadJs

}
