package com.quantifind.charts.vega

/**
 * User: austin
 * Date: 8/27/14
 *
 * What's the difference between field and string - field references data?
 */
// name overload resolution
//case class Array(fields: Array[String]) {
//
//}

case class Copy(from: Option[String], fields: Option[Array[String]]) {

}

case class Cross(`with`: Option[String], diagonal: Option[Boolean]) {

}

case class Facet(keys: Option[Array[String]], sort: Option[String]) { // field v String

}

case class Filter(test: Option[String]) {

}

case class Fold(fields: Option[Array[String]]) {

}

case class Formula(field: Option[String], expr: Option[String]) {

}

case class Slice(by: Option[String], field: Option[String]) { //by -> number Array[Number] String

}

case class Sort(by: Option[String]) { // Array[String]

}

case class Stats(`value`: Option[String], median: Option[Boolean], assign: Option[Boolean]) {

}

case class Truncate(`value`: Option[String], output: Option[String], limit: Option[Int], position: Option[String], ellipsis: Option[String], wordbreak: Option[Boolean]) {

}

case class Unique(field: Option[String], as: Option[String]) {

}

case class Window(size: Option[Int], step: Option[Int]) {

}

case class Zip(`with`: Option[String], as: Option[String], key: Option[String], withKey: Option[String], default: Option[Any]) {

}

// TODO visual encoding