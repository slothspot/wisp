package com.quantifind.charts.vega


/**
 * User: austin
 * Date: 8/20/14
 */
object Color {
  type Type = String
  implicit def javaColorToHex(color: java.awt.Color): Type = "#"+Integer.toHexString(color.getRGB()).substring(2)
  implicit def javaColorToHexOption(color: java.awt.Color): Option[Type] = Some("#"+Integer.toHexString(color.getRGB()).substring(2))
}

object PaddingLiteral {
  type Type = String
  val (auto, strict) = ("auto", "strict")
  def values = Set(auto, strict)
}

case class PaddingObject(
                          bottom: Option[Int],
                          left: Option[Int],
                          right: Option[Int],
                          top: Option[Int]
                          ) {

  def toMap =
    Map(
      "bottom" -> bottom,
      "left" -> left,
      "right" -> right,
      "top" -> top
    ).flatMap(VegaKey.flatten)
}

object FormatType {
  type Type = String
  val (csv, json, topojson, treejson, tsv) = ("csv", "json", "topojson", "treejson", "tsv")
  def values = Set(csv, json, topojson, treejson, tsv)
}

object ScaleType {
  type Type = String
  val (linear, log, ordinal, pow, quantile, quantize, sqrt, threshold, time, utc) = ("linear", "log", "ordinal", "pow", "quantile", "quantize", "sqrt", "threshold", "time", "utc")
  def values = Set(linear, log, ordinal, pow, quantile, quantize, sqrt, threshold, time, utc)
}

trait ScaleRange
object ScaleRange {
  type Type = Any // String or Array or DataRef
}

object ScaleRangeLiteral extends ScaleRange {
  val (category10, category20, height, shapes, width) = ("category10", "category20", "height", "shapes", "width")
  def values = Set(category10, category20, height, shapes, width)
}

trait ScaleDomain
object ScaleDomain {
  type Type = Any // Array or DataRef
}

object AxisType {
  type Type = String
  val (x, y) = ("x", "y")
  def values = Set(x, y)
}

object AxisOrient {
  type Type = String
  val (bottom, left, right, top) = ("bottom", "left", "right", "top")
  def values = Set(bottom, left, right, top)
}

object AxisLayer {
  type Type = String
  val (back, front) = ("back", "front")
  def values = Set(back, front)
}

object LegendOrient {
  type Type = String
  val (left, right) = ("left", "right")
  def values = Set(left, right)
}

object MarkType {
  type Type = String
  val (arc, area, group, image, line, path, rect, symbol, text) = ("arc", "area", "group", "image", "line", "path", "rect", "symbol", "text")
  def values = Set(arc, area, group, image, line, path, rect, symbol, text)
}

/*
  val mods = Set("in", "out", "in-out", "out-in")
  val base = Set("bounce", "circle", "cubic", "exp", "linear", "quad", "sin")
mods.map(m => "val = (" + base.map(b => s"${b}_${m}").toSeq.sorted.mkString(", ") + ") = " + "(\"" + base.map(b => s"${b}-${m.replaceAllLiterally("_", "-")}").toSeq.sorted.mkString("\", \"") +"\")").foreach(println))
 */
object MarkEase {
  type Type = String
  val (bounce, circle, cubic, exp, linear, quad, sin) = ("bounce", "circle", "cubic", "exp", "linear", "quad", "sin")
  val (bounce_in, circle_in, cubic_in, exp_in, linear_in, quad_in, sin_in) = ("bounce-in", "circle-in", "cubic-in", "exp-in", "linear-in", "quad-in", "sin-in")
  val (bounce_out, circle_out, cubic_out, exp_out, linear_out, quad_out, sin_out) = ("bounce-out", "circle-out", "cubic-out", "exp-out", "linear-out", "quad-out", "sin-out")
  val (bounce_in_out, circle_in_out, cubic_in_out, exp_in_out, linear_in_out, quad_in_out, sin_in_out) = ("bounce-in-out", "circle-in-out", "cubic-in-out", "exp-in-out", "linear-in-out", "quad-in-out", "sin-in-out")
  val (bounce_out_in, circle_out_in, cubic_out_in, exp_out_in, linear_out_in, quad_out_in, sin_out_in) = ("bounce-out-in", "circle-out-in", "cubic-out-in", "exp-out-in", "linear-out-in", "quad-out-in", "sin-out-in")
  def values = Set(bounce, circle, cubic, exp, linear, quad, sin)
}