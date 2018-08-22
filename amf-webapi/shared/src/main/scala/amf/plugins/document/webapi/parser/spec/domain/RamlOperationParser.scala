package amf.plugins.document.webapi.parser.spec.domain

import amf.core.metamodel.domain.DomainElementModel
import amf.core.model.domain.AmfArray
import amf.core.parser.{Annotations, _}
import amf.plugins.features.validation.ParserSideValidations._
import amf.plugins.document.webapi.contexts.RamlWebApiContext
import amf.plugins.document.webapi.parser.spec.common.{AnnotationParser, SpecParserOps}
import amf.plugins.document.webapi.parser.spec.declaration.OasCreativeWorkParser
import amf.plugins.domain.webapi.metamodel.OperationModel
import amf.plugins.domain.webapi.metamodel.OperationModel.Method
import amf.plugins.domain.webapi.models.{Operation, Response}
import org.yaml.model._
import amf.core.utils.Strings

import scala.collection.mutable

/**
  *
  */
case class RamlOperationParser(entry: YMapEntry, producer: String => Operation, parseOptional: Boolean = false)(
    implicit ctx: RamlWebApiContext)
    extends SpecParserOps {

  def parse(): Operation = {
    val method: String = entry.key.as[YScalar].text

    val operation = producer(method).add(Annotations(entry))
    operation.set(Method, ScalarNode(entry.key).string())

    if (parseOptional && method.endsWith("?")) {
      operation.set(OperationModel.Optional, value = true)
      operation.set(OperationModel.Method, method.stripSuffix("?"))
    }

    entry.value.tagType match {
      // Regular operation
      case YType.Map => parseMap(entry.value.as[YMap], operation)
      // Empty operation
      case _ if entry.value.toOption[YScalar].map(_.text).exists(s => s == "" || s == "null") => operation
      case _ =>
        ctx.violation(operation.id, s"Invalid node ${entry.value} for method $method", entry.value)
        operation
    }
  }

  protected def parseMap(map: YMap, operation: Operation): Operation = {

    val map = entry.value.as[YMap]
    ctx.closedShape(operation.id, map, "operation")

    map.key("displayName", OperationModel.Name in operation)
    map.key("oasDeprecated".asRamlAnnotation, OperationModel.Deprecated in operation)
    map.key("summary".asRamlAnnotation, OperationModel.Summary in operation)
    map.key("externalDocs".asRamlAnnotation,
            OperationModel.Documentation in operation using OasCreativeWorkParser.parse)
    map.key("protocols", (OperationModel.Schemes in operation).allowingSingleValue)
    map.key("consumes".asRamlAnnotation, OperationModel.Accepts in operation)
    map.key("produces".asRamlAnnotation, OperationModel.ContentType in operation)
    map.key("tags".asRamlAnnotation, OperationModel.Tags in operation)
    val DeclarationParser = ParametrizedDeclarationParser.parse(operation.withTrait) _
    map.key("is", (DomainElementModel.Extends in operation using DeclarationParser).allowingSingleValue.optional)

    ctx.factory
      .requestParser(map, () => operation.withRequest(), parseOptional)
      .parse()
      .foreach(operation.set(OperationModel.Request, _))

    val optionalMethod = if (parseOptional) "\\??" else ""

    map.key(
      "responses",
      entry => {
        entry.value.tagType match {
          case YType.Null => // ignore
          case _ =>
            val entries = entry.value
              .as[YMap]
              .regex(
                s"(\\d{3})$optionalMethod"
              )
            val keys = entries.map(_.key.as[YScalar].text)
            val keySet = keys.toSet
            if (keys.size > keySet.size) {
              ctx.violation(DuplicatedOperationStatusCodeSpecification.id, operation.id, None,"RAML Responses must not have duplicated status codes", entry.value)
            }

            if (entries.nonEmpty) {
              val responses = mutable.ListBuffer[Response]()
              entries.foreach(entry => {
                responses += ctx.factory
                  .responseParser(entry, (r: Response) => r.adopted(operation.id), parseOptional)
                  .parse()
              })
              operation.set(OperationModel.Responses,
                AmfArray(responses, Annotations(entry.value)),
                Annotations(entry))
            } else {
              ctx.violation(MissingOperationStatusCodeSpecification.id, operation.id, None,"RAML Responses must have a valid status code", entry.value)
            }
        }
      }
    )

    val SchemeParser = RamlParametrizedSecuritySchemeParser.parse(operation.withSecurity) _
    map.key("securedBy", (OperationModel.Security in operation using SchemeParser).allowingSingleValue)

    map.key("description", (OperationModel.Description in operation).allowingAnnotations)

    AnnotationParser(operation, map).parse()

    operation
  }
}
