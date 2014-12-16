package com.quantifind.charts.vega

import com.quantifind.json.ScalaJsonFactory

/**
 * User: austin
 * Date: 8/19/14
 */

object Vega {
  implicit def optionWrap[T](value: T): Option[T] = Option(value)
  implicit def traversableToSomeArray(t: Traversable[Any]) = Some(t.toArray) // for axes
  implicit def scaleToName(s: Scale): Option[String] = s.name
  implicit def scaleOptionToName(s: Option[Scale]): Option[String] = s.map(_.name).flatten

  // Resolve types
  implicit def intToPadding(num: Int) = Some(Padding(numeric = Some(num)))
  implicit def stringToPadding(str: PaddingLiteral.Type) = Some(Padding(literal = Some(str)))
  implicit def objectToPadding(obj: PaddingObject) = Some(Padding(json = Some(obj)))
}

abstract class VegaKey(var _key: String) {
  def toMap: Map[String, Any]
}

object VegaKey {
  def flatten(o: (String, Option[Any])) = o._2 match {
    case None => None
    case Some(v) => Some(o._1, v)
  }

  def vegaTraversableToServiceFormat(t: Traversable[VegaKey]): Map[String, Any] = Map(t.head._key -> t.map(_.toMap))

}

// Called a Visualization by Vega
case class Vega(
                 name: Option[String] = None,
                 width: Option[Int] = None,
                 height: Option[Int] = None,
                 viewport: Option[Array[Int]] = None,
                 padding: Option[Padding] = None,
                 data: Option[Array[Data]] = None,
                 scales: Option[Array[Scale]] = None,
                 axes: Option[Array[Axis]] = None,
                 legends: Option[Array[Legend]] = None,
                 marks: Option[Array[Mark]] = None
                 ) {

  def toMap: Map[String, Any] = {
    val padMap = if(padding.nonEmpty) Seq(padding.map(_.toMap)).flatMap(x => x).reduce(_ ++ _) else Map()
    Map(
      "name" -> name,
      "width" -> width,
      "height" -> height,
      "viewport" -> viewport
    ).flatMap(VegaKey.flatten) ++
      padMap ++
      Map(
        "data" -> data,
        "scales" -> scales,
        "axes" -> axes,
        "legends" -> legends,
        "marks" -> marks
      ).flatMap{case(name, element) => element.map(e => name -> e.map(_.toMap))}
  }

  def toJson: String = {
    ScalaJsonFactory.serialize(this.toMap)
  }
}

case class Padding(numeric: Option[Int] = None,
                   literal: Option[PaddingLiteral.Type] = None,
                   json: Option[PaddingObject] = None,
                   var __key: String = "data") extends VegaKey(__key) {

  def toMap = {

    val output = (numeric, literal, json) match {
      case (Some(num), None, None) => num
      case (None, Some(literal), None) => literal
      case (None, None, Some(json)) => json.toMap
      case _ => throw new Exception("Only pass one type of padding: Number, String, Object")
    }

    Map("padding" -> output)
  }
}

case class Data(
                 name: Option[String] = None,
                 format: Option[Object] = None, // json, csv, tsv, topojson, treejson
                 values: Option[Array[XYPair[_, _]]] = None, // JSON of type format.type, TODO!
                 source: Option[String] = None, // name of dataset to use
                 url: Option[String] = None, // url to load dataset
                 transform: Option[Object] = None, // transformation to load on dataset. Not going to use.
                 var __key: String = "data"
                 ) extends VegaKey(__key) {

  def toMap =
    Map(
      "values" -> values.getOrElse(Array()).map(_.toMap)
    ) ++
      Map(
        "name" -> name,
        "source" -> source,
        "url" -> url
      ).flatMap(VegaKey.flatten)
}

// property, feature, mesh, children only apply to certain formatTypes
case class Format(
                   formatType: Option[FormatType.Type] = None,
                   parse: Option[Map[String, Any]] = None,
                   property: Option[String] = None,
                   feature: Option[String] = None,
                   mesh: Option[String] = None,
                   children: Option[String] = None,
                   var __key: String = "format"
                   ) extends VegaKey(__key) {

  def toMap =
    Map(
      "type" -> formatType,
      "property" -> property,
      "feature" -> feature,
      "mesh" -> mesh,
      "children" -> children
    ).flatMap(VegaKey.flatten) ++
      Map(
        "parse" -> parse
      )

}

case class XYPair[X: Numeric, Y: Numeric](
                                           x: X,
                                           y: Y
                                           ) {
  def toMap =
    Map(
      "x" -> x,
      "y" -> y
    )
}

case class Scale(
                  name: Option[String] = None,
                  scaleType: Option[ScaleType.Type] = None,
                  domain: Option[ScaleDomain.Type] = None, // DataRef
                  domainMin: Option[Int] = None, // DataRef
                  domainMax: Option[Int] = None, // DataRef
                  range: Option[ScaleRange.Type] = None, // Array, String, DataRef. String == width, height, shapes, category10, category20
                  rangeMin: Option[Int] = None, // *
                  rangeMax: Option[Int] = None, // *
                  reverse: Option[Boolean] = None,
                  round: Option[Boolean] = None,
                  nice: Option[Boolean] = None,
                  // Todo, Ordinal, Time, Quantitative, Domains, Range Literals
                  var __key: String = "scale"
                  ) extends VegaKey(__key) {
  def toMap =
    Map(
      "name" -> name,
      "type" -> scaleType,
      "domain" -> domain,
      "range" -> range,
      "rangeMin" -> rangeMin,
      "rangeMax" -> rangeMax,
      "reverse" -> reverse,
      "round" -> round,
      "nice" -> nice
    ).flatMap(VegaKey.flatten)
}

case class DataRef(
                    data: Option[String] = None,
                    field: Option[String] = None // Field, Array<Field>, Object, Array<Object> (ie, anything). To make this something other than String is a large undertaking (maybe?)
                    ) {
  def toMap =
    Map(
      "data" -> data,
      "field" -> field
    ).flatMap(VegaKey.flatten)
}

case class Axis(
                 axisType: Option[AxisType.Type] = None,
                 scale: Option[String] = None, // point at scale object and implicitly convert to String through name field
                 orient: Option[AxisOrient.Type] = None,
                 title: Option[String] = None,
                 titleOffset: Option[Int] = None,
                 format: Option[String] = None,
                 ticks: Option[Int] = None,
                 values: Option[Array[Any]] = None,
                 subdivide: Option[Int] = None,
                 tickPadding: Option[Int] = None,
                 tickSize: Option[Int] = None,
                 tickSizeMajor: Option[Int] = None,
                 tickSizeMinor: Option[Int] = None,
                 tickSizeEnd: Option[Int] = None,
                 offset: Option[Int] = None, // Object
                 layer: Option[AxisLayer.Type] = None,
                 grid: Option[Boolean] = None,
                 properties: Option[Any] = None, // custom axis styling - don't implement
                 var __key: String = "axis"
                 ) extends VegaKey(__key) {
  def toMap =
    Map (
      "type" -> axisType,
      "scale" -> scale,
      "orient" -> orient,
      "title" -> title,
      "titleOffset" -> titleOffset,
      "format" -> format,
      "ticks" -> ticks,
      //      "values" -> values
      "subdivide" -> subdivide,
      "tickPadding" -> tickPadding,
      "tickSize" -> tickSize,
      "tickSizeMajor" -> tickSizeMajor,
      "tickSizeMinor" -> tickSizeMinor,
      "tickSizeEnd" -> tickSizeEnd,
      //      "offset" -> offset,
      "layer" -> layer,
      "grid" -> grid
      //      "properties" -> properties
    ).flatMap (VegaKey.flatten)
}

case class Legend(
                   size: Option[String] = None, // what are the options?
                   shape: Option[String] = None, // what are the options?
                   fill: Option[Color.Type] = None,
                   stroke: Option[Color.Type] = None,
                   orient: Option[LegendOrient.Type] = None,
                   title: Option[String] = None,
                   format: Option[String] = None, // d3 format pattern
                   values: Option[Array[Any]] = None, //
                   properties: Option[Any] = None, // custom legend styling - don't implement
                   var __key: String = "legend"
                   ) extends VegaKey(__key) {
  def toMap =
    Map(
      "size" -> size,
      "shape" -> shape,
      "fill" -> fill,
      "stroke" -> stroke,
      "orient" -> orient,
      "title" -> title,
      "format" -> format,
      "values" -> values
      //      "properties" -> properties
    ).flatMap(VegaKey.flatten)
}

case class Mark(
                 markType: Option[MarkType.Type] = None,
                 interactive: Option[Boolean] = None,
                 name: Option[String] = None,
                 description: Option[String] = None,
                 from: Option[DataRef] = None, // transform and data fields
                 properties: Option[Properties] = None, // ?
                 key: Option[String] = None,
                 delay: Option[Int] = None, // Int?
                 ease: Option[MarkEase.Type] = None,
                 var __key: String = "mark"
                 ) extends VegaKey(__key) {
  def toMap =
    Map(
      "type" -> markType,
      "interactive" -> interactive,
      "name" -> name,
      "description" -> description,
      "key" -> key,
      "delay" -> delay,
      "ease" -> ease
    ).flatMap(VegaKey.flatten) ++
      Map(
        "properties" -> properties
      ).flatMap{case(name, e) => e.map(name -> _.toMap)} ++
      Map(
        "from" -> from
      ).flatMap{case(name, e) => e.map(name -> _.toMap)}
}

case class Properties(
                       enter: Option[Property] = None,
                       update: Option[Property] = None,
                       exit: Option[Property] = None,
                       hover: Option[Property] = None
                       ) {
  def toMap =
    Map(
      "enter" -> enter,
      "update" -> update,
      "exit" -> exit,
      "hover" -> hover
    ).flatMap{case(name, e) => e.map(name -> _.toMap)}
}

case class ValueRef (
                      `value`: Option[String] = None,  // change name?
                      field: Option[String] = None, // String Object
                      group: Option[String] = None, // String Boolean
                      scale: Option[String] = None, // String Object
                      mult: Option[Int] = None,   // Numeric
                      offset: Option[Int] = None, // Numeric
                      band: Option[Boolean] = None,
                      fill: Option[Fill] = None
                      ) {

  println("Initialized ValueRef with value: " + `value`)

//  val underlyingValue

  def toMap =
    Map(
      "value" -> `value`,
      "field" -> field,
      "group" -> group,
      "scale" -> scale,
      "mult" -> mult,
      "offest" -> offset,
      "band" -> band,
      "fill" -> fill
    ).flatMap(VegaKey.flatten)
}

case class Property(
                     x: Option[ValueRef] = None, // ValueRef -> Numeric
                     x2: Option[ValueRef] = None, // ValueRef -> Numeric
                     width: Option[ValueRef] = None, // ValueRef -> Numeric
                     y: Option[ValueRef] = None,
                     y2: Option[ValueRef] = None,
                     height: Option[ValueRef] = None,
                     opacity: Option[ValueRef] = None,
                     fill: Option[ValueRef] = None,
                     fillOpacity: Option[ValueRef] = None,
                     stroke: Option[ValueRef] = None, // ValueRef -> Color
                     strokeWidth: Option[ValueRef] = None,
                     strokeOpacity: Option[ValueRef] = None,
                     strokeDash: Option[ValueRef] = None,
                     strokeDashOffset: Option[ValueRef] = None
                     ) {

  def toMap =
    Map(
      "x" -> x,
      "x2" -> x2,
      "width" -> width,
      "y" -> y,
      "y2" -> y2,
      "height" -> height,
      "opacity" -> opacity,
      "fill" -> fill,
      "fillOpacity" -> fillOpacity,
      "stroke" -> stroke,
      "strokeWidth" -> strokeWidth,
      "strokeOpacity" -> strokeOpacity,
      "strokeDash" -> strokeDash,
      "strokeDashOffset" -> strokeDashOffset
    ).flatMap{case(name, e) => e.map(name -> _.toMap)}
}

// TODO: check proper triplet is used?
// rgb, hsl, lab, hcl
case class Fill(
                 r: Option[Property] = None,
                 g: Option[Property] = None,
                 b: Option[Property] = None,
                 h: Option[Property] = None,
                 s: Option[Property] = None,
                 l: Option[Property] = None,
                 a: Option[Property] = None,
                 c: Option[Property] = None
                 ) {
  def toMap =
    Map(
      "r" -> r,
      "g" -> g,
      "b" -> b,
      "h" -> h,
      "s" -> s,
      "l" -> l,
      "a" -> a,
      "c" -> c
    ).flatMap(VegaKey.flatten)
}

// ColorRef its own thing?
