package com.qf.charts.repl

import java.io.File
import unfiltered.util.Port
import unfiltered.jetty.Http
import com.qf.charts.PlotServer

import scala.collection.mutable

/**
 * User: austin
 * Date: 11/14/14
 *
 * Defines a plotting api for interoperability with PlotServer.
 * Stores the plots in a stack
 */
trait PlotLike[T] {
  def plot(t: T): T
}

trait Plottable[T] extends PlotLike[T] {
  var plots = List[T]()
}

trait WebPlot[T] extends Plottable[T] {

  val serverRootFileName = s"index-${System.currentTimeMillis()}.html"
  val port = Port.any
  var serverMode = false
  var firstOpenWindow = false

  var serverRootFile = new File(serverRootFileName)
  val localFile = java.io.File.createTempFile("scala-viz-", ".html")

  var http: Option[Http] = None
  var plotServer: Option[PlotServer] = None

  startServer()

  implicit def openWindow(link: String) = {
    try {
      import sys.process._
      s"open $link".!!
    } catch {
      case ex: Exception => s"Unable to open $link : in browser, ie trying to launch browser on a remote machine"
    }
  }

  def openFirstWindow(link: String) = {
    if(!firstOpenWindow) {
      val msg = openWindow(link)
      if (msg.nonEmpty) println("Error on opening window: " + msg)
      firstOpenWindow = true
    }
  }

  implicit def startServer(message: String = s"http://${java.net.InetAddress.getLocalHost.getCanonicalHostName}:${port}/${serverRootFileName}") {
    if (!serverMode) {
      serverMode = true
      val ps = new PlotServer
      val args = ps.parseArgs(Array("--altRoot", serverRootFile.getAbsolutePath, "--port", port.toString))
      val server = ps.get(args)
      server.start
      println("Server started: " + message)
      http = Some(server)
      plotServer = Some(ps)
    }
  }

  implicit def stopServer {
    if (serverMode) {
      serverRootFile.delete()
      // satisfy the promise, to avoid exception on close
      // TODO handle failure in the PlotServer
      plotServer.map(_.p.success())
      http.map(_.stop)
      http.map(_.destroy)
      serverMode = false
      plotServer = None
    }
  }

  def plot(t: T): T = {
    startServer()
    t
  }
}

/*
Chart augmenting functions:
 - hold (from Matlab syntax) - until unhold is called, graphs will be plotted on the same plot, ie the series will be merged into one Highchart
 - unhold: stop holding. Next graph will be on its own plot
 - xlabel: assigns a label to the x-axis of the most recent plot
 - ylabel: assign a label to the y-axis of the most recent plot
*/
trait Hold[T] extends PlotLike[T] {
  var isHeld: Boolean = false
  def hold(): Unit = {
    isHeld = true
  }
  def unhold(): Unit = {
    isHeld = false
  }
}

trait Labels[T] extends PlotLike[T] {
  def xAxis(label: String): T
  def yAxis(label: String): T
  def title(label: String): T
  def legend(labels: Iterable[String]): T
}