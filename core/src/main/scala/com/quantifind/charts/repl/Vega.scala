package com.quantifind.charts.repl

import java.io.{PrintWriter, File}

import com.quantifind.charts.vega.Vega

import scala.concurrent.Promise

/**
 * User: austin
 * Date: 12/12/14
 */
object Vega extends IterablePairLowerPriorityImplicits with WebPlotVega {

  implicit def mkIterableIterable[A: Numeric, B: Numeric](ab: (Iterable[A], Iterable[B])) = new IterableIterable(ab._1, ab._2)
  implicit def mkIterableIterable[A: Numeric, B: Numeric](ab: (Iterable[(A, B)])) = new IterableIterable(ab.map(_._1), ab.map(_._2))
  implicit def mkIterableIterable[B: Numeric](b: (Iterable[B])) = new IterableIterable((0 until b.size), b)

//  def bar
}

trait WebPlotVega extends WebPlot[Vega] {

  def plotAll(): Unit = {
    val temp = File.createTempFile("vega", ".html")
    val pw = new PrintWriter(temp)
    pw.print(js)

    val spec = new PrintWriter(new File(temp.getParentFile, "vegaspec.json"))

    spec.print(plots.head.toJson)

//    plots.zipWithIndex.foreach { case(plot, index) =>
//      pw.println(vegaContainer(plot.toJson, index))
//    }
//
//    pw.print(reload)
//    (0 until plots.size).foreach(index => pw.println(containerDivs(index)))
//    pw.print(jsFooter)
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

  val js =
  s"""
    |<html>
    |  <head>
    |    <title>Vega</title>
    |    <script src="http://d3js.org/d3.v3.min.js"></script>
    |    <script src="http://trifacta.github.io/vega/vega.js"></script>
    |    <script type="text/javascript">${reloadJs}</script>
    |  </head>
    |  <body>
    |    <div id="view" class="view"></div>
    |  </body>
    |  <script type="text/javascript">
    |   vg.parse.spec("vegaspec.json", function(chart) {
    |    var view = chart({el:"#view"})
    |      .renderer("svg")
    |      .update();
    |  });
    |
    |  </script>
    |</html>
  """.stripMargin

}
