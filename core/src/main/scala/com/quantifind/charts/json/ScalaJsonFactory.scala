package com.quantifind.json

import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.core._
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.module.jsonSchema.JsonSchema
import com.fasterxml.jackson.module.jsonSchema.factories._
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import java.io.{ObjectInputStream, StringWriter}
import java.lang.reflect.{ParameterizedType, Type}
import scala.collection.JavaConverters._
import scala.collection._
import scala.collection.mutable.HashMap
import scala.reflect.ClassTag
import com.fasterxml.jackson.datatype.joda.JodaModule
import scala.util.control.NonFatal


/**
 * @author karthik@quantifind.com
 *         Date: 1/3/12
 *
 */

object ScalaJsonFactory extends java.io.Serializable {
  @transient private var _objectMapper: ObjectMapper = _
  @transient private var _prettyObjectMapper: ObjectMapper = _
  initMappers()

  lazy private val factory = new JsonFactory()

  def getObjectMapper(): ObjectMapper = _objectMapper

  def getPrettyObjectMapper: ObjectMapper = _prettyObjectMapper

  private def getObjectMapperInternal(): ObjectMapper = {

    val mapper = new ObjectMapper()

    // Don't write null map values
    mapper.configure(SerializationFeature.WRITE_NULL_MAP_VALUES, false)

    mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL)

    // Don't fail on serialization when there are null fields in the class
    mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false)

    // When there are unknown properties in the JSON (some unused fields), don't fail
    mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)

    //when there is a JSON_STRING instead of JSON_ARRAY and we want an array, just put it in an array
    mapper.configure(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY, true)

    // Scala specific. Register the scala module with the asl mapper
    mapper.registerModule(DefaultScalaModule)

    mapper.registerModule(new JodaModule())

    mapper
  }

  def deserializeToMap(json: String) = {

    val objMap = ScalaJsonFactory.getObjectMapper().readValue(json, classOf[Any]) match {
      case map: Map[_, _] => map.asInstanceOf[Map[String, Any]]
      case _ => Map.empty[String, Any]
    }

    objMap
  }

  def getTopLevelJsonMap(json: String): Map[String, String] = {

    val objMap = ScalaJsonFactory.getObjectMapper().readValue(json, classOf[Any]) match {
      case map: java.util.LinkedHashMap[_, _] => map.asScala.asInstanceOf[Map[String, Any]]
      case _ => Map.empty[String, Any]
    }

    val strMap = new HashMap[String, String]()

    objMap map {
      case (key, value) =>
        strMap += (key -> getObjectMapper().writeValueAsString(value))
    }

    strMap
  }

  def serialize(value: AnyRef): String = _objectMapper.writeValueAsString(value)

  def deserialize[T: Manifest](value: String): T =
    _objectMapper.readValue(value, typeReference[T])

  def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  /**
   * Generates a JSON schema object for the specified type.
   * Example usage:
   *
   * {{{
   *   // create schema object
   *   val jsonSchema = ScalaJsonFactory.mkSchema[MongoBean]
   *   // marshal it out
   *   ScalaJsonFactory.getObjectMapper.writeValueAsString(jsonSchema)
   * }}}
   */
  def mkSchema[T: ClassTag]: JsonSchema = {
    val mapper = getObjectMapper()
    val clazz = implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]
    val visitor = new SchemaFactoryWrapper()
    mapper.acceptJsonFormatVisitor(mapper.constructType(clazz), visitor)
    visitor.finalSchema()
  }

  private[this] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty) {
      m.runtimeClass
    }

    else new ParameterizedType {
      def getRawType = m.runtimeClass

      def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray

      def getOwnerType = null
    }
  }

  def getJson(topLevelStrMap: immutable.Map[String, String]): String = {

    val objMap = new HashMap[String, Any]()

    topLevelStrMap foreach {
      case (key, value) =>
        objMap.put(key, getObjectMapper().readValue(value, classOf[Any]))
    }

    getObjectMapper().writeValueAsString(objMap)

  }

  private def initMappers() {
    _objectMapper = getObjectMapperInternal()
    _prettyObjectMapper = {
      val mapper = getObjectMapperInternal()
      mapper.configure(SerializationFeature.INDENT_OUTPUT, true)
      mapper
    }
  }

  implicit  class JsonString(json: String) {
    def deserialize[T: Manifest]: T = ScalaJsonFactory.deserialize(json)
    def deserializeToMap: Map[String, Any] = ScalaJsonFactory.deserializeToMap(json)
    def jsonField(name: String) = ScalaJsonFactory.getFieldOnly(name, json)
  }

  implicit  class JsonOptString(jsonOpt: Option[String]) {
    def deserialize[T: Manifest]: Option[T] = jsonOpt.map(ScalaJsonFactory.deserialize(_))
    def deserializeToMap: Option[Map[String, Any]] = jsonOpt.map(ScalaJsonFactory.deserializeToMap)
    def jsonField(name: String) = jsonOpt.flatMap(ScalaJsonFactory.getFieldOnly(name, _))
  }


  /**
   * extract just one string value for a json string, this should be faster than deserializing a map
   * or a specific object mapping.  If the field contains an array or an object, it will return their json representation.
   * If the json is not valid, this might still succeed properly.
   * @param fieldName the name of the field to extract
   * @param json the json to look into
   * @return the value extracted from the json if it's found, otherwise None.
   */
  def getFieldOnly(fieldName: String, json: java.io.InputStream): Option[String] = {
    val parser = factory.createParser(json)
    getFieldOnlyFromParser(fieldName, parser)
  }

  /**
   * extract just one string value for a json string, this should be faster than deserializing a map
   * or a specific object mapping.  If the field contains an array or an object, it will return their json representation.
   * If the json is not valid, this might still succeed properly.
   * @param fieldName the name of the field to extract
   * @param json the json to look into
   * @return the value extracted from the json if it's found, otherwise None.
   */
  def getFieldOnly(fieldName: String, json: String): Option[String] = {
    val parser = factory.createParser(json)
    getFieldOnlyFromParser(fieldName, parser)
  }

  /**
   * get a field and deserializes it without deserializing the whole json
   * @param fieldName
   * @param json
   * @tparam T
   * @return
   */
  def getFieldOnlyAs[T: Manifest](fieldName: String, json: String): Option[T] = {
    getFieldOnly(fieldName=fieldName, json=json).map { str =>
      deserialize[T](str)
    }
  }

  /**
   * a generic method doing the work from the parser. So we can accept inputstream and String
   * @param fieldName
   * @param parser    will be closed in this method
   * @return
   */
  private def getFieldOnlyFromParser(fieldName: String, parser: JsonParser): Option[String] = {
    def readJson(tokenIt: Iterator[JsonToken], start: JsonToken): String = {
      var opens = 0
      import scala.util.control.Breaks._
      val until = start match {
        case JsonToken.START_ARRAY => JsonToken.END_ARRAY
        case JsonToken.START_OBJECT => JsonToken.END_OBJECT
        case _ => throw new UnsupportedOperationException(s"don' know how to process $start")
      }
      val str = new StringWriter()
      val gen = factory.createGenerator(str)

      def write(tok: JsonToken) {
        tok match {
          case JsonToken.START_ARRAY =>
            gen.writeStartArray()
            opens += 1
          case JsonToken.END_ARRAY =>
            gen.writeEndArray()
            opens -= 1
          case JsonToken.START_OBJECT =>
            gen.writeStartObject()
            opens += 1
          case JsonToken.END_OBJECT =>
            gen.writeEndObject()
            opens -= 1
          case JsonToken.FIELD_NAME => gen.writeFieldName(parser.getText)
          case JsonToken.VALUE_FALSE => gen.writeBoolean(false)
          case JsonToken.VALUE_TRUE => gen.writeBoolean(true)
          case JsonToken.VALUE_NULL => gen.writeNull()
          case JsonToken.VALUE_NUMBER_FLOAT => gen.writeNumber(parser.getFloatValue)
          case JsonToken.VALUE_NUMBER_INT =>
            try {
              gen.writeNumber(parser.getIntValue)
            } catch {
              case NonFatal(_) =>
                gen.writeNumber(parser.getLongValue)
            }
          case JsonToken.VALUE_STRING => gen.writeString(parser.getText)
          case _ => throw new UnsupportedOperationException(s"don' know how to process $tok")
        }
      }
      write(start)
      breakable {
        while (tokenIt.hasNext) {
          val tok = tokenIt.next()
          write(tok)
          if (opens == 0 && tok.equals(until)) {
            break
          }
        }
      }
      gen.close()
      str.toString
    }


    val parserStream = new Iterator[JsonToken] {
      def hasNext = !parser.isClosed

      def next() = parser.nextToken()
    }
    val afterDrop = parserStream.dropWhile {
      tok =>
        !(JsonToken.FIELD_NAME.equals(tok) && parser.getCurrentName.equals(fieldName))
    }
    if (afterDrop.hasNext) {
      afterDrop.next() //really move the iterator
      val tok = parser.nextValue()
      val t = if (tok.equals(JsonToken.START_OBJECT) || tok.equals(JsonToken.START_ARRAY)) {
        readJson(afterDrop, tok)
      } else parser.getText()
      parser.close()
      Some(t)
    } else None
  }

  /**
   * Used to reinitialize the transient fields on deserialization.
   */
  private def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    initMappers()
  }
}