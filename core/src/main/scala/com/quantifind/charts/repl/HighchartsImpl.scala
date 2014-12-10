package com.qf.charts.repl

import java.io.{PrintWriter, File}

import com.qf.charts.highcharts.{Series, SeriesType, Highchart}
import scala.concurrent.Promise

/**
 * User: austin
 * Date: 12/3/14
 */
trait WebPlotHighcharts extends WebPlot[Highchart] {

  def plotAll(): Unit = {
    val temp = File.createTempFile("highcharts", ".html")
    val pw = new PrintWriter(temp)
    pw.print(jsHeader)

    plots.zipWithIndex.foreach { case(plot, index) =>
      pw.println(highchartsContainer(plot.toJson, index))
    }

    pw.print(reload)
    (0 until plots.size).foreach(index => pw.println(containerDivs(index)))
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

  override def plot(t: Highchart): Highchart = {
    super.plot(t)
    plots.push(t)
    plotAll()
    t
  }

//  val _reloadJs = scala.io.Source.fromFile(new File(getClass().getResource("/nathan-reloader.js").getPath))
  def reloadJs =
//    if (serverMode)
      "$.ajax({url: '/check', dataType: 'jsonp', complete: function(){location.reload()}})"
//    else _reloadJs

  val jsHeader =
    """
      |<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
      |<html>
      |  <head>
      |    <title>
      |      Chart
      |    </title>
      |    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
      |    <script type="text/javascript" src="http://code.jquery.com/jquery-1.8.2.min.js"></script>
      |    <script type="text/javascript" src="http://code.highcharts.com/3.0.6/highcharts.js"></script>
      |    <script type="text/javascript" src="http://code.highcharts.com/3.0.6/modules/exporting.js"></script>
      |
    """.stripMargin

  def highchartsContainer(json: String, index: Int): String =
    """
      |    <script type="text/javascript">
      |        $(function() {
      |            $('#container%s').highcharts(
    """.stripMargin.format(index.toString) +
      """
        |                %s
        |            );
        |        });
        |    </script>
        |
      """.stripMargin.format(json)

  val reload =
    s"""
        |  <script type="text/javascript">${reloadJs}</script>
        |  </head>
        |  <body>
      """.stripMargin

  def containerDivs(index: Int) =
    s"""
        |    <div id="container%s" style="min-width: 400px; height: 400px; margin: 0 auto"></div>
      """.stripMargin.format(index.toString)

  val jsFooter =
    """
      |</body>
      |</html>
    """.stripMargin
}

trait MatlabStyleHighcharts extends Matlab[Highchart] with WebPlotHighcharts {
  import Highchart._
  override def plot(t: Highchart): Highchart = {
    val newPlot =
      if(isHeld && plots.nonEmpty) {
        val oldplot = plots.pop()
        // Throws away things from t besides the series!
        oldplot.copy(series = oldplot.series ++ t.series)
      } else t
    super.plot(newPlot)
  }
  def xlabel(label: String): Unit = {
    val plot = plots.pop()
    val newplot = plot.copy(xAxis = label)
    plots.push(newplot)
  }
  def ylabel(label: String): Unit = {
    val plot = plots.pop()
    val newplot = plot.copy(yAxis = label)
    plots.push(newplot)
  }

  def xyToSeries[T1: Numeric, T2: Numeric](x: Iterable[T1], y: Iterable[T2], chartType: SeriesType.Type, format: String = "r") =
    plot(Highchart(Series(x.zip(y).toSeq, chart = chartType)))
}
