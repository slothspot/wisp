package com.quantifind.charts.repl

import java.io.{PrintWriter, File}

import com.quantifind.charts.highcharts.{Series, SeriesType, Highchart}
import com.quantifind.charts.highcharts._
import scala.concurrent.Promise

/**
 * User: austin
 * Date: 12/3/14
 */
trait WebPlotHighcharts extends WebPlot[Highchart] {

  /**
   * Iterates through the plots and builds the necessary javascript and html around them.
   */
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
      ps.p.success(())
      ps.p = Promise[Unit]()
    }

    val (serverRootFile, port, serverMode) = getWispServerInfo()

    def link =
      if (serverMode) {
        println("ENTERED SERVER MODE")
        temp.renameTo(serverRootFile)
        s"http://${java.net.InetAddress.getLocalHost.getCanonicalHostName}:${port}"
      }
      else s"file://$temp"

    openFirstWindow(link)

    println(s"Output written to $link (CMD + Click link in Mac OSX).")
  }

  override def plot(t: Highchart): Highchart = {
    super.plot(t)
    plots = t +: plots
    undoStack.push(plots)
    plotAll()
    t
  }

  def reloadJs =
      "$.ajax({url: '/check', dataType: 'jsonp', complete: function(){location.reload()}})"

  val jsHeader =
    """
      |<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
      |<html>
      |  <head>
      |    <title>
      |      Highchart
      |    </title>
      |    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
      |    <script type="text/javascript" src="http://code.jquery.com/jquery-1.8.2.min.js"></script>
      |    <script type="text/javascript" src="http://code.highcharts.com/4.0.4/highcharts.js"></script>
      |    <script type="text/javascript" src="http://code.highcharts.com/4.0.4/modules/exporting.js"></script>
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

/**
 * Defines auxiliary tools available to plots, such as adding a Title
 */
trait HighchartsStyles extends Hold[Highchart] with Labels[Highchart] with WebPlotHighcharts {
  import Highchart._
  override def plot(t: Highchart): Highchart = {
    val newPlot =
      if(isHeld && plots.nonEmpty) {
        val oldplot = plots.head
        plots = plots.tail
        // Throws away things from t besides the series!
        oldplot.copy(series = oldplot.series ++ t.series)
      } else t
    super.plot(newPlot)
  }
  def xAxis(label: String): Highchart = {
    val plot = plots.head
    plots = plots.tail
    val newPlot = plot.copy(xAxis = label)
    super.plot(newPlot)
  }
  def yAxis(label: String): Highchart = {
    val plot = plots.head
    plots = plots.tail
    val newPlot = plot.copy(yAxis = label)
    super.plot(newPlot)
  }
  def title(label: String): Highchart = {
    val plot = plots.head
    plots = plots.tail
    val newPlot = plot.copy(title = label)
    super.plot(newPlot)  
  }
  // Assign names to series, if mis-matched lengths use the shorter one as a cut-off
  def legend(labels: Iterable[String]): Highchart = {
    val labelArray = labels.toArray
    val plot = plots.head
    plots = plots.tail
    val newSeries = plot.series.toSeq.zipWithIndex.map{case(s, idx) => if(idx >= labels.size) s else s.copy(name = Some(labelArray(idx)))}
    val newPlot = plot.copy(series = newSeries)
    super.plot(newPlot)
  }
  def xyToSeries[T1: Numeric, T2: Numeric](x: Iterable[T1], y: Iterable[T2], chartType: SeriesType.Type) =
    plot(Highchart(Series(x.zip(y).toSeq, chart = chartType)))

  def stack(stackType: Stacking.Type = Stacking.normal): Highchart = {
    val plot = plots.head
    plots = plots.tail
    val newPlot = plot.copy(plotOptions = Some(PlotOptions(series = PlotOptionKey(stacking = stackType))))
    super.plot(newPlot)
  }
}
