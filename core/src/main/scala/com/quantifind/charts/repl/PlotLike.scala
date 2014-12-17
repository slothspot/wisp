package com.quantifind.charts.repl

import java.io.File
import unfiltered.util.Port
import unfiltered.jetty.Http

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

  def plotAll()

  // Heavy handed approach to undo / redo - maintain entire state in stack
  protected val undoStack = new mutable.Stack[List[T]]()
  protected val redoStack = new mutable.Stack[List[T]]()

  def undo() = {
    if(undoStack.nonEmpty) {
      redoStack.push(undoStack.pop())
      plots = if(undoStack.nonEmpty) undoStack.head else List[T]()
      plotAll()
    }
  }

  def redo = {
    if(redoStack.nonEmpty) {
      undoStack.push(redoStack.pop())
      plots = undoStack.head
      plotAll()
    }
  }

  def delete() = {
    if(plots.nonEmpty) {
      undoStack.push(plots)
      plots = plots.tail
      plotAll()
    }
  }

  def deleteAll() = {
    undoStack.push(plots)
    plots = List[T]()
    plotAll()
  }
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

  def openWindow(link: String) = {
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

  def startServer(message: String = s"http://${java.net.InetAddress.getLocalHost.getCanonicalHostName}:${port}/${serverRootFileName}") {
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

  def stopServer {
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