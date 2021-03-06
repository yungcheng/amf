package amf.plugins.document.webapi.validation

import amf._
import amf.core.remote._
import amf.core.vocabulary.{Namespace, ValueType}

object AMFRawValidations {

  /**
    * @param uri URI of the validation, null to auto-generate
    * @param message Optional message for the validation
    * @param level Level: AMF, OpenAPI or RAML
    * @param owlClass Optional OWL class target of the validation
    * @param owlProperty Optional OWL property target of the validation
    * @param shape Type of SHACL shape for the validation
    * @param constraint URI of the constraint component
    * @param value Value for the constraint component
    */
  class AMFValidation(val uri: Option[String],
                      val message: Option[String],
                      val spec: String,
                      val level: String,
                      val owlClass: Option[String],
                      val owlProperty: Option[String],
                      val shape: String,
                      val target: String,
                      val constraint: String,
                      val value: String,
                      val ramlErrorMessage: String,
                      val openApiErrorMessage: String,
                      val severity: String)
  object AMFValidation {
    def apply(spec: String,
              level: String,
              owlClass: String,
              owlProperty: String,
              shape: String,
              target: String,
              constraint: String,
              value: String,
              ramlErrorMessage: String,
              openApiErrorMessage: String,
              severity: String): AMFValidation =
      apply(None,
            None,
            spec,
            level,
            Some(owlClass),
            Some(owlProperty),
            shape,
            target,
            constraint,
            value,
            ramlErrorMessage,
            openApiErrorMessage,
            severity)

    def apply(uri: String,
              message: String,
              spec: String,
              level: String,
              owlClass: String,
              owlProperty: String,
              shape: String,
              target: String,
              constraint: String,
              value: String,
              ramlErrorMessage: String,
              openApiErrorMessage: String,
              severity: String): AMFValidation =
      apply(Some(uri),
            Some(message),
            spec,
            level,
            Some(owlClass),
            Some(owlProperty),
            shape,
            target,
            constraint,
            value,
            ramlErrorMessage,
            openApiErrorMessage,
            severity)

    def apply(uri: Option[String],
              message: Option[String],
              spec: String,
              level: String,
              owlClass: Option[String],
              owlProperty: Option[String],
              shape: String,
              target: String,
              constraint: String,
              value: String,
              ramlErrorMessage: String,
              openApiErrorMessage: String,
              severity: String): AMFValidation =
      new AMFValidation(
        uri.map(Namespace.uri(_).iri()),
        message,
        spec,
        level,
        owlClass.map(Namespace.uri(_).iri()),
        owlProperty.map(Namespace.uri(_).iri()),
        shape,
        Namespace.uri(target).iri(),
        Namespace.uri(constraint).iri(),
        adaptValue(constraint, value),
        ramlErrorMessage,
        openApiErrorMessage,
        severity
      )

    // todo: change Validations instances for use this with types.
    def fromFields(uri: Option[ValueType],
                   message: Option[String],
                   spec: String,
                   level: String,
                   owlClass: Option[ValueType],
                   owlProperty: Option[ValueType],
                   shape: String,
                   target: ValueType,
                   constraint: ValueType,
                   value: String,
                   ramlErrorMessage: String,
                   openApiErrorMessage: String,
                   severity: String): AMFValidation =
      new AMFValidation(
        uri.map(_.iri()),
        message,
        spec,
        level,
        owlClass.map(_.iri()),
        owlProperty.map(_.iri()),
        shape,
        target.iri(),
        constraint.iri(),
        adaptValue(constraint.iri(), value),
        ramlErrorMessage,
        openApiErrorMessage,
        severity
      )

    private def adaptValue(constraint: String, value: String) =
      if (constraint.endsWith("pattern")) value
      else Namespace.uri(value).iri() // this might not be a URI, but trying to expand it is still safe
  }

  val schemaRequiredInParameter = AMFValidation(
    Amf.name,
    "Domain",
    "apiContract:Parameter",
    "raml-shapes:schema",
    "PropertyShape",
    "sh:path",
    "sh:minCount",
    "1",
    "RAML Type information is mandatory for parameters",
    "Schema/type information required for Parameter objects",
    "Violation"
  )

  // todo: Use valuetype instead of string uri.

  trait ProfileValidations {
    def validations(): Seq[AMFValidation]
  }

  trait AmfProfileValidations extends ProfileValidations {
    private lazy val result = Seq(
      AMFValidation(
        Amf.name,
        "Domain",
        "doc:DomainElement",
        "core:name",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Title and names must be string",
        "Names must be string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "doc:DomainElement",
        "core:description",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Descriptions must be strings",
        "Description must be strings",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:WebAPI",
        "core:name",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "API title is mandatory",
        "Info object 'title' must be a single value",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:WebAPI",
        "apiContract:scheme",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "API BaseUri scheme information must be a string",
        "Swagger object 'schemes' must be a string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:WebAPI",
        "apiContract:scheme",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "API BaseUri scheme information must be a string",
        "Swagger object 'schemes' must be a string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:WebAPI",
        "apiContract:accepts",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Default media types must contain strings",
        "Field 'consumes' must contain strings",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:WebAPI",
        "apiContract:accepts",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^(([-\\w]+|[*]{1})\\/([-+.\\w]+|[*]{1}))(\\s*;\\s*\\w+=[-+\\w.]+)*$",
        "Default media types must be valid",
        "Field 'produces' must be valid",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:WebAPI",
        "core:mediaType",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Default media types must be string",
        "Field 'produces' must contain strings",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:WebAPI",
        "core:version",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "API version must be a string",
        "Info object 'version' must be string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:WebAPI",
        "core:termsOfService",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "API terms of service must be a string",
        "Info object 'termsOfService' must be string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "core:Organization",
        "core:email",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "API provider email must be a string",
        "Contact object 'email' must be a string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:EndPoint",
        "apiContract:path",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Resource path must be a string",
        "PathItem object path must be a string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Operation",
        "apiContract:guiSummary",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Methods' summary information must be a string",
        "Methods' summary information must be a string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Operation",
        "doc:deprecated",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:boolean",
        "Methods' deprecated must be a boolean",
        "Methods' deprecated must be a boolean",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Operation",
        "apiContract:scheme",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Protocols must contain strings",
        "Schemes must contain strings",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Operation",
        "apiContract:accepts",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Method default media types consumed must be strings",
        "Operation object 'consumes' must be strings",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Response",
        "apiContract:statusCode",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Status code for a Response must be a string",
        "Status code for a Response object must be a string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Parameter",
        "core:name",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "Parameter information must have a name",
        "Parameter object must have a name property",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Parameter",
        "apiContract:required",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:boolean",
        "Information about required parameters must be a boolean value",
        "Required property of a Parameter object must be boolean",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Parameter",
        "apiContract:binding",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Information about the binding of the parameter is mandatory",
        "'in' property of a Parameter object must be a string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Parameter",
        "apiContract:binding",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "Binding information for a parameter is mandatory",
        "'in' property of a Parameter object is mandatory",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Payload",
        "core:mediaType",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Method default media types must be strings",
        "Operation object 'produces' must be strings",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:xml-wrapped-scalar",
        "XML property 'wrapped' must be false for scalar types",
        Raml.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:xmlSerialization",
        "PropertyShape", // Not useful
        "sh:path", // Not useful
        "raml-shapes:xmlWrappedScalar",
        "0",
        "XML property 'wrapped' must be false for scalar types",
        "XML property 'wrapped' must be false for scalar types",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:xml-non-scalar-attribute",
        "XML property 'attribute' must be false for non-scalar types",
        Raml.name,
        "Domain",
        "raml-shapes:Shape",
        "sh:xmlSerialization",
        "PropertyShape", // Not useful
        "sh:path", // Not useful
        "raml-shapes:xmlNonScalarAttribute",
        "0",
        "XML property 'attribute' must be false for non-scalar types",
        "XML property 'attribute' must be false for non-scalar types",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:XMLSerializer",
        "raml-shapes:xmlAtribute",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:boolean",
        "XML attribute serialisation info must be boolean",
        "XML attribute serialisation info must be boolean",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:XMLSerializer",
        "raml-shapes:xmlWrapped",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:boolean",
        "XML wraping serialisation info must be boolean",
        "XML wrapping serialisation info must be boolean",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:XMLSerializer",
        "raml-shapes:xmlName",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "XML name serialisation info must be string",
        "XML name serialisation info must be string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:XMLSerializer",
        "raml-shapes:xmlNamespace",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "XML namespace serialisation info must be string",
        "XML namespace serialisation info must be string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:XMLSerializer",
        "raml-shapes:xmlPrefix",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "XML prefix serialisation info must be string",
        "XML prefix serialisation info must be string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ObjectShape",
        "raml-shapes:minProperties",
        "PropertyShape",
        "sh:path",
        "sh:minInclusive",
        "0",
        "minProperties for a RAML Object type cannot be negative",
        "minProperties for a Schema object cannot be negative",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ObjectShape",
        "raml-shapes:minProperties",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:integer",
        "minProperties for a RAML Object type must be an integer",
        "minProperties for a Schema object must be an integer",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ObjectShape",
        "raml-shapes:maxProperties",
        "PropertyShape",
        "sh:path",
        "sh:minInclusive",
        "0",
        "maxProperties for a RAML Object type cannot be negative",
        "maxProperties for a Schema object cannot be negative",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ObjectShape",
        "raml-shapes:maxProperties",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:integer",
        "maxProperties for a RAML Object type must be an integer",
        "maxProperties for a Schema object must be an integer",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ObjectShape",
        "sh:closed",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:boolean",
        "additionalProperties for a RAML Object type must be a boolean",
        "additionalProperties for a Schema object must be a boolean",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ObjectShape",
        "raml-shapes:discriminator",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "discriminator for RAML Object type must be a string value",
        "discriminator for a Schema object must be a string value",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ObjectShape",
        "raml-shapes:discriminatorValue",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "x-discriminatorValue for RAML Object type must be a string value",
        "discriminatorValue for a Schema object must be a string value",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ObjectShape",
        "raml-shapes:readOnly ",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:boolean",
        "(readOnly) for a RAML Object type must be a boolean",
        "readOnly for a Schema object must be a boolean",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ArrayShape",
        "sh:minCount",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:integer",
        "minItems for a RAML Array type must be an integer",
        "minItems of a Schema object of type 'array' must be an integer",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ArrayShape",
        "sh:minCount",
        "PropertyShape",
        "sh:path",
        "sh:minInclusive ",
        "0",
        "maxItems for a RAML Array type must be greater than 0",
        "maxItems of a Schema object of type 'array' must be greater than 0",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ArrayShape",
        "sh:maxCount",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:integer",
        "maxItems for a RAML Array type must be an integer",
        "maxItems of a Schema object of type 'array' must be an integer",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ArrayShape",
        "sh:minCount",
        "PropertyShape",
        "sh:path",
        "sh:minInclusive",
        "0",
        "minItems for a RAML Array type must be greater than 0",
        "minItems of a Schema object of type 'array' must be greater than 0",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ArrayShape",
        "sh:maxCount",
        "PropertyShape",
        "sh:path",
        "sh:minInclusive",
        "0",
        "maxItems for a RAML Array type must be greater than 0",
        "maxItems of a Schema object of type 'array' must be greater than 0",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ArrayShape",
        "raml-shapes:uniqueItems",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:boolean",
        "uniqueItems for a RAML Array type must be a boolean",
        "uniqueItems of a Schema object of type 'array' must be a boolean",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:pattern",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "pattern facet for a RAML scalar type must be a string",
        "pattern for scalar Schema object of scalar type must be a string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:minLength",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:integer",
        "minLength facet for a RAML scalar type must be a integer",
        "minLength for scalar Schema object of scalar type must be a integer",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:maxLength",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:integer",
        "maxLength facet for a RAML scalar type must be a integer",
        "maxLength for scalar Schema object of scalar type must be a integer",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:minInclusive",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:double",
        "minimum facet for a RAML scalar type must be a number",
        "minimum for scalar Schema object of scalar type must be a integer",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:maxInclusive",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:double",
        "maximum facet for a RAML scalar type must be a number",
        "maximum for scalar Schema object of scalar type must be a integer",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:minExclusive",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:boolean",
        "x-exclusiveMinimum facet for a RAML scalar type must be a boolean",
        "exclusiveMinimum for scalar Schema object of scalar type must be a boolean",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:maxExclusive",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:boolean",
        "x-exclusiveMaximum facet for a RAML scalar type must be a boolean",
        "exclusiveMaximum for scalar Schema object of scalar type must be a boolean",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:minLength",
        "PropertyShape",
        "sh:path",
        "sh:minInclusive",
        "0",
        "Min length facet should be greater or equal than 0",
        "Min length facet should be greater or equal than 0",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:maxLength",
        "PropertyShape",
        "sh:path",
        "sh:minInclusive",
        "0",
        "Max length facet should be greater or equal than 0",
        "Max length facet should be greater or equal than 0",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:FileShape",
        "sh:minLength",
        "PropertyShape",
        "sh:path",
        "sh:minInclusive",
        "0",
        "Min length facet should be greater or equal than 0",
        "Min length facet should be greater or equal than 0",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:FileShape",
        "sh:maxLength",
        "PropertyShape",
        "sh:path",
        "sh:minInclusive",
        "0",
        "Max length facet should be greater or equal than 0",
        "Max length facet should be greater or equal than 0",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "raml-shapes:format",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "format facet for a RAML scalar type must be a string",
        "format for scalar Schema object of scalar type must be a string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "raml-shapes:multipleOf",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:double",
        "multipleOf facet for a RAML scalar type must be a number",
        "multipleOf for scalar Schema object of scalar type must be a number",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "raml-shapes:multipleOf",
        "PropertyShape",
        "sh:path",
        "sh:minExclusive",
        "0",
        "multipleOf facet for a RAML scalar type must be greater than 0",
        "multipleOf for scalar Schema object of scalar type must be greater than 0",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:datatype",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "type information for a RAML scalar is required",
        "type information fo a Schema object of scalar type is required",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Tag",
        "core:name",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "Tag must have a name",
        "Tag object must have a name property",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Server",
        "core:urlTemplate",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "API baseUri host information must be a string",
        "Swagger object 'host' and 'basePath' must be a string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Server",
        "core:description",
        "PropertyShape",
        "sh:path",
        "sh:datatype",
        "xsd:string",
        "Server 'description' property must be a string",
        "Server 'description' property must be a string",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Server",
        "core:urlTemplate",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "Server must have an 'url' property",
        "Server must have an 'url' property",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "security:SecurityScheme",
        "security:type",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "Security scheme type is mandatory",
        "Security scheme type is mandatory",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "security:SecurityScheme",
        "security:type",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^OAuth\\s1.0|OAuth\\s2.0|Basic\\sAuthentication|Digest\\sAuthentication|Pass\\sThrough|Api\\sKey|http|openIdConnect|x-.+$",
        "Security scheme type should be one of the supported ones",
        "Security scheme type should be one of the supported ones",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "security:SecurityScheme",
        "security:type",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "Type is mandatory in a Security Scheme Object",
        "Type is mandatory in a Security Scheme Object",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:strict-url-strinzgs",
        "URLs in values mapped to core:url must be valid",
        Amf.name,
        "Domain",
        "doc:DomainElement",
        "core:url",
        "NodeShape",
        "sh:targetObjectsOf",
        "sh:nodeKind",
        "sh:IRI",
        "URLs must be valid",
        "URLs must be valid",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:pattern-validation",
        "Pattern is not valid",
        Amf.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:pattern",
        "PropertyShape",
        "sh:path",
        "raml-shapes:patternValidation",
        "0",
        "Pattern is not valid",
        "Pattern is not valid",
        "Violation"
      )
    )
    override def validations(): Seq[AMFValidation] = result
  }

  object AmfValidations extends AmfProfileValidations

  trait RamlAndOasValidations extends AmfProfileValidations {
    private lazy val result = super.validations() ++ Seq(
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Parameter",
        "apiContract:binding",
        "PropertyShape",
        "sh:path",
        "sh:in",
        "query,path,header,uri,cookie",
        "Binding information for a parameter with an invalid value",
        "'in' property of a parameter with an invalid value",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:EndPoint",
        "apiContract:path",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^/",
        "Resource path must start with a '/'",
        "PathItem path must start with a '/'",
        "Violation"
      ),
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Operation",
        "apiContract:method",
        "PropertyShape",
        "sh:path",
        "sh:in",
        "get,put,post,delete,options,head,patch,connect,trace",
        "Unknown method type",
        "Unknown Operation method",
        "Violation"
      )
    )
    override def validations(): Seq[AMFValidation] = result
  }

  trait RamlValidations extends RamlAndOasValidations {
    private lazy val result = super.validations() ++ Seq(
      AMFValidation(
        Raml.name,
        "Domain",
        "apiContract:WebAPI",
        "core:name",
        "PropertyShape",
        "sh:path",
        "sh:minLength",
        "1",
        "Info object 'title' must not be empty",
        "API name must not be an empty string",
        "Violation"
      ),
      AMFValidation(
        Raml.name,
        "Domain",
        "core:CreativeWork",
        "core:title",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "API documentation title is mandatory",
        "Documentation object 'x-title' is mandatory",
        "Violation"
      ),
      AMFValidation(
        Raml.name,
        "Domain",
        "core:CreativeWork",
        "core:description",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "API documentation content is mandatory",
        "Documentation object 'description' is mandatory",
        "Violation"
      ),
      AMFValidation(
        Raml.name,
        "Domain",
        "doc:DomainProperty",
        "raml-shapes:schema",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "type is mandatory for a RAML annotationType",
        "schema is mandatory for an extension type",
        "Violation"
      ),
      AMFValidation(
        Raml.name,
        "Domain",
        "security:Settings",
        "security:authorizationGrant",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^authorization_code|password|client_credentials|implicit|(\\w+:(\\/?\\/?)[^\\s]+)$",
        "Invalid authorization grant. The options are: authorization_code, password, client_credentials, implicit or any valid absolut URI",
        "Invalid authorization grant. The options are: authorization_code, password, client_credentials, implicit or any valid absolut URI",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:raml-root-schemes-values",
        "Protocols property must be http or https",
        Raml.name,
        "Domain",
        "apiContract:WebAPI",
        "apiContract:scheme",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^(H|h)(T|t)(T|t)(P|p)(S|s)?$",
        "Protocols must have a case insensitive value matching http or https",
        "Swagger object 'schemes' property must have a case insensitive value matching http or https",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:raml-operation-schemes-values",
        "Protocols property must be http or https",
        Raml.name,
        "Domain",
        "apiContract:Operation",
        "apiContract:scheme",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^(H|h)(T|t)(T|t)(P|p)(S|s)?$",
        "Protocols must have a case insensitive value matching http or https",
        "Swagger object 'schemes' property must have a case insensitive value matching http or https",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:raml-root-schemes-non-empty-array",
        "Protocols must be a non-empty array of case-insensitive strings with values 'http' and/or 'https'",
        Raml.name,
        "Domain",
        "apiContract:WebAPI",
        "apiContract:scheme",
        "PropertyShape",
        "sh:path",
        "raml-shapes:nonEmptyListOfProtocols",
        "0",
        "Protocols must be a non-empty array of case-insensitive strings with values 'http' and/or 'https'",
        "Protocols must be a non-empty array of case-insensitive strings with values 'http' and/or 'https'",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:raml-operation-schemes-non-empty-array",
        "Protocols must be a non-empty array of case-insensitive strings with values 'http' and/or 'https'",
        Raml.name,
        "Domain",
        "apiContract:Operation",
        "apiContract:scheme",
        "PropertyShape",
        "sh:path",
        "raml-shapes:nonEmptyListOfProtocols",
        "0",
        "Protocols must be a non-empty array of case-insensitive strings with values 'http' and/or 'https'",
        "Protocols must be a non-empty array of case-insensitive strings with values 'http' and/or 'https'",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:min-max-inclusive",
        "Maximum must be greater than or equal to minimum",
        Raml.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:minInclusive",
        "PropertyShape",
        "sh:path",
        "raml-shapes:minimumMaximumValidation",
        "0",
        "Maximum must be greater than or equal to minimum",
        "Maximum must be greater than or equal to minimum",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:min-max-items",
        "MaxItems must be greater than or equal to minItems",
        Raml.name,
        "Domain",
        "raml-shapes:ArrayShape",
        "sh:minCount",
        "PropertyShape",
        "sh:path",
        "raml-shapes:minMaxItemsValidation",
        "0",
        "MaxItems must be greater than or equal to minItems",
        "MaxItems must be greater than or equal to minItems",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:min-max-length",
        "MaxLength must be greater than or equal to minLength",
        Raml.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:minLength",
        "PropertyShape",
        "sh:path",
        "raml-shapes:minMaxLengthValidation",
        "0",
        "MaxLength must be greater than or equal to minLength",
        "MaxLength must be greater than or equal to minLength",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:min-max-length",
        "MaxLength must be greater than or equal to minLength",
        Raml.name,
        "Domain",
        "raml-shapes:FileShape",
        "sh:minLength",
        "PropertyShape",
        "sh:path",
        "raml-shapes:minMaxLengthValidation",
        "0",
        "MaxLength must be greater than or equal to minLength",
        "MaxLength must be greater than or equal to minLength",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:min-max-properties",
        "MaxProperties must be greater than or equal to minProperties",
        Raml.name,
        "Domain",
        "sh:NodeShape",
        "raml-shapes:minProperties",
        "PropertyShape",
        "sh:path",
        "raml-shapes:minMaxPropertiesValidation",
        "0",
        "MaxProperties must be greater than or equal to minProperties",
        "MaxProperties must be greater than or equal to minProperties",
        "Violation"
      ),
      AMFValidation(
        Raml.name,
        "Domain",
        "apiContract:Payload",
        "core:mediaType",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "Payload media type is mandatory",
        "",
        "Violation"
      ),
      AMFValidation(
        Raml08.name,
        "Domain",
        "security:Settings",
        "security:signature",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^HMAC-SHA1|RSA-SHA1|PLAINTEXT$",
        "Invalid OAuth 1.0 signature. The options are: HMAC-SHA1, RSA-SHA1, or PLAINTEXT",
        "Invalid OAuth 1.0 signature. The options are: HMAC-SHA1, RSA-SHA1, or PLAINTEXT",
        "Violation"
      ),
      AMFValidation(
        Raml.name,
        "Domain",
        "apiContract:Response",
        "apiContract:statusCode",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^([1-5]{1}[0-9]{2})$|^(default)$",
        "Status code for a Response must be a value between 100 and 599",
        "Status code for a Response must be a value between 100 and 599 or 'default'",
        "Violation"
      ),
      schemaRequiredInParameter
      /*
      ,
      AMFValidation(
        Amf.name,
        "Domain",
        "apiContract:Payload",
        "core:mediaType",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^(([-\\w]+|[*]{1})\\/([-+.\\w]+|[*]{1}))(\\s*;\\s*\\w+=[-+\\w.]+)*$",
        "Invalid media type for method",
        "Swagger Operation object 'produces' must be a valid media type",
        "Violation"
      )
     */
    )
    override def validations(): Seq[AMFValidation] = result
  }

  object Raml10Validations extends RamlValidations {
    private lazy val result = super.validations() ++ Seq(
      )
    override def validations(): Seq[AMFValidation] = result
  }

  object Raml08Validations extends RamlValidations {
    private lazy val result = super.validations() ++ Seq(
      AMFValidation(
        Raml08.name,
        "Domain",
        "security:Settings",
        "security:authorizationGrant",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^code|token|owner|credentials$",
        "Invalid authorization grant. The options are: code, token, owner or credentials",
        "Invalid authorization grant. The options are: code, token, owner or credentials",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:raml-schemes",
        "Protocols property must be http or https",
        Raml08.name,
        "Domain",
        "apiContract:WebAPI",
        "apiContract:scheme",
        "PropertyShape",
        "sh:path",
        "sh:in",
        "http,https,HTTP,HTTPS",
        "Protocols must have a case insensitive value matching http or https",
        "Swagger object 'schemes' property must have a case insensitive value matching http or https",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:min-max-inclusive",
        "Maximum must be greater than or equal to minimum",
        Raml08.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:minInclusive",
        "PropertyShape",
        "sh:path",
        "raml-shapes:minimumMaximumValidation",
        "0",
        "Maximum must be greater than or equal to minimum",
        "Maximum must be greater than or equal to minimum",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:min-max-items",
        "MaxItems must be greater than or equal to minItems",
        Raml08.name,
        "Domain",
        "raml-shapes:ArrayShape",
        "sh:minCount",
        "PropertyShape",
        "sh:path",
        "raml-shapes:minMaxItemsValidation",
        "0",
        "MaxItems must be greater than or equal to minItems",
        "MaxItems must be greater than or equal to minItems",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:min-max-length",
        "MaxLength must be greater than or equal to minLength",
        Raml08.name,
        "Domain",
        "raml-shapes:ScalarShape",
        "sh:minLength",
        "PropertyShape",
        "sh:path",
        "raml-shapes:minMaxLengthValidation",
        "0",
        "MaxLength must be greater than or equal to minLength",
        "MaxLength must be greater than or equal to minLength",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:min-max-properties",
        "MaxProperties must be greater than or equal to minProperties",
        Raml08.name,
        "Domain",
        "sh:NodeShape",
        "raml-shapes:minProperties",
        "PropertyShape",
        "sh:path",
        "raml-shapes:minMaxPropertiesValidation",
        "0",
        "MaxProperties must be greater than or equal to minProperties",
        "MaxProperties must be greater than or equal to minProperties",
        "Violation"
      )
    )
    override def validations(): Seq[AMFValidation] = result
  }

  trait OasValidations extends RamlAndOasValidations {
    private lazy val result = super.validations() ++ Seq(
      AMFValidation(
        "amf-parser:mandatory-api-version",
        "Missing madatory Swagger / info / version",
        Oas.name,
        "Domain",
        "apiContract:WebAPI",
        "core:version",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "API Version is Mandatory",
        "Version is mandatory in Info object",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:openapi-schemes",
        "Protocols property must be http,https,ws,wss",
        Oas.name,
        "Domain",
        "apiContract:WebAPI",
        "apiContract:scheme",
        "PropertyShape",
        "sh:path",
        "sh:in",
        "http,https,ws,wss",
        "Protocols must match a value http, https, ws or wss",
        "Swagger object 'schemes' property must have a value matching http, https, ws or wss",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:mandatory-external-doc-url",
        "Swagger external-doc element without URL",
        Oas.name,
        "Domain",
        "core:CreativeWork",
        "core:url",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "Documentation URL is mandatory in API external documentation",
        "URL is mandatory in External Documentation object",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:mandatory-license-name",
        "Swagger License node without name",
        Oas.name,
        "Domain",
        "apiContract:License",
        "core:name",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "License name is mandatory if license information is included",
        "Name is mandatory in License object",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:empty-responses",
        "No responses declared",
        Oas.name,
        "Domain",
        "apiContract:Operation",
        "apiContract:returns",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "Responses array cannot be empty",
        "Responses cannot be empty",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:empty-enum",
        "Enum in types cannot be empty",
        Oas.name,
        "Domain",
        "raml-shapes:Shape",
        "sh:in",
        "PropertyShape",
        "sh:path",
        "sh:node",
        "amf-parser:NonEmptyList",
        "Property 'enum' must have at least one value",
        "Property 'enum' for a Schema object must have at least one value",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:array-shape-items-mandatory",
        "Declaration of the type of the items for an array is required",
        Oas.name,
        "Domain",
        "raml-shapes:ArrayShape",
        "raml-shapes:items",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "items facet of RAML Array type is required",
        "items property of Schema objects of type 'array' is required",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:path-parameter-required",
        "Path parameters must have the required property set to true",
        Oas.name,
        "Domain",
        "apiContract:Parameter",
        "apiContract:binding",
        "PropertyShape",
        "sh:path",
        "raml-shapes:pathParameterRequiredProperty",
        "0",
        "Path parameters must have the required property set to true",
        "Path parameters must have the required property set to true",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:file-parameter-in-form-data",
        "Parameter of type file must set property 'in' to formData",
        Oas.name,
        "Domain",
        "apiContract:Parameter",
        "raml-shapes:schema",
        "PropertyShape",
        "sh:path",
        "raml-shapes:fileParameterMustBeInFormData",
        "0",
        "Parameter of type file must set property 'in' to formData",
        "Parameter of type file must set property 'in' to formData",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:description-is-required-in-response",
        "Description must be defined in a response",
        Oas.name,
        "Domain",
        "apiContract:Response",
        "core:description",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "",
        "Response must have a 'description' field",
        "Violation"
      ),
      AMFValidation(
        Oas.name,
        "Domain",
        "core:Organization",
        "core:email",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".stripMargin,
        "",
        "Field 'email' must be in the format of an email address",
        "Violation"
      ),
      urlValidation(Oas.name, "core:License", "core:url"),
      urlValidation(Oas.name, "core:Organization", "core:url"),
      urlValidation(Oas.name, "core:CreativeWork", "core:url")
    )
    override def validations(): Seq[AMFValidation] = result
    def urlValidation(spec: String, owlClass: String, owlProperty: String) =
      AMFValidation(
        spec,
        "Domain",
        owlClass,
        owlProperty,
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        """^((https?|ftp|file)://)?[-a-zA-Z0-9()+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9()+&@#/%=~_|]$""".stripMargin,
        "Must be in the format of a URL",
        "Must be in the format of a URL",
        "Violation"
      )
  }

  object Oas20Validations extends OasValidations {
    private lazy val result = super.validations() ++ Seq(
      AMFValidation(
        Oas20.name,
        "Domain",
        "apiContract:Response",
        "apiContract:statusCode",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^([1-5]{1}[0-9]{2})$|^(default)$",
        "",
        "Status code for a Response must be a value between 100 and 599 or 'default'",
        "Violation"
      ),
      AMFValidation(
        Oas20.name,
        "Domain",
        "security:OAuth2Flow",
        "security:flow",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^(implicit|password|application|accessCode)$",
        "Invalid flow. The options are: implicit, password, application or accessCode",
        "Invalid flow. The options are: implicit, password, application or accessCode",
        "Violation"
      ),
      AMFValidation(
        Oas20.name,
        "Domain",
        "security:Settings",
        "security:in",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^(query|header)$",
        "Invalid 'in' value. The options are: query or header",
        "Invalid 'in' value. The options are: query or header",
        "Violation"
      ),
      schemaRequiredInParameter
    )
    override def validations(): Seq[AMFValidation] = result
  }

  object Async20Validations extends AmfProfileValidations {
    private lazy val result = super.validations() ++ Seq(
      AMFValidation(
        AsyncApi20.name,
        "Domain",
        "apiBinding:WebSocketsChannelBinding",
        "apiBinding:method",
        "PropertyShape",
        "sh:path",
        "sh:in",
        "GET,POST",
        "",
        "'method' for channel binding object must be one of 'GET' or 'POST'",
        "Violation"
      ),
      AMFValidation(
        AsyncApi20.name,
        "Domain",
        "apiBinding:MqttOperationBinding",
        "apiBinding:qos",
        "PropertyShape",
        "sh:path",
        "sh:in",
        "0,1,2",
        "",
        "'qos' for mqtt operation binding object must be one of 0, 1 or 2",
        "Violation"
      ),
      AMFValidation(
        AsyncApi20.name,
        "Domain",
        "apiBinding:Amqp091ChannelBinding",
        "apiBinding:is",
        "PropertyShape",
        "sh:path",
        "sh:in",
        "routingKey,queue",
        "",
        "'is' for amqp 0.9.1 channel binding object must be one of 'queue' or 'routingKey'",
        "Violation"
      ),
      AMFValidation(
        AsyncApi20.name,
        "Domain",
        "apiBinding:Amqp091ChannelExchange",
        "apiBinding:type",
        "PropertyShape",
        "sh:path",
        "sh:in",
        "topic,direct,fanout,default,headers",
        "",
        "'type' for amqp 0.9.1 channel exchange object must be one of 'topic', 'direct', 'fanout', 'default' or 'headers'",
        "Violation"
      ),
      AMFValidation(
        AsyncApi20.name,
        "Domain",
        "apiBinding:HttpOperationBinding",
        "apiBinding:method",
        "PropertyShape",
        "sh:path",
        "sh:in",
        "GET,POST,PUT,PATCH,DELETE,HEAD,OPTIONS,CONNECT,TRACE",
        "",
        "'method' for http operation binding object must be one of 'GET','POST','PUT','PATCH','DELETE','HEAD','OPTIONS','CONNECT','TRACE'",
        "Violation"
      ),
      AMFValidation(
        AsyncApi20.name,
        "Domain",
        "apiBinding:HttpOperationBinding",
        "apiBinding:type",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "",
        "'type' for http operation binding is required",
        "Violation"
      ),
      AMFValidation(
        AsyncApi20.name,
        "Domain",
        "apiBinding:MqttServerBinding",
        "apiBinding:keepAlive",
        "PropertyShape",
        "sh:path",
        "sh:minInclusive ",
        "0",
        "",
        "'keepAlive' must be greater than 0",
        "Violation"
      ),
      AMFValidation(
        AsyncApi20.name,
        "Domain",
        "apiBinding:MqttServerLastWill",
        "apiBinding:qos",
        "PropertyShape",
        "sh:path",
        "sh:in",
        "0,1,2",
        "",
        "'qos' for mqtt server binding last will object must be one of 0, 1 or 2",
        "Violation"
      ),
      AMFValidation(
        AsyncApi20.name,
        "Domain",
        "apiBinding:Amqp091OperationBinding",
        "apiBinding:deliveryMode",
        "PropertyShape",
        "sh:path",
        "sh:in",
        "1,2",
        "",
        "'deliveryMode' for amqp 0.9.1 operation binding object must be one of 1 or 2",
        "Violation"
      ),
      AMFValidation(
        AsyncApi20.name,
        "Domain",
        "apiBinding:MqttServerBinding",
        "apiBinding:expiration",
        "PropertyShape",
        "sh:path",
        "sh:minInclusive ",
        "0",
        "",
        "'expiration' must be greater than 0",
        "Violation"
      )
    )

    override def validations(): Seq[AMFValidation] = result
  }

  object Oas30Validations extends OasValidations {
    private lazy val result = super.validations() ++ Seq(
      AMFValidation(
        Oas30.name,
        "Domain",
        "apiContract:Response",
        "apiContract:statusCode",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^([1-5]{1}(([0-9]{2})|XX))$|^(default)$",
        "",
        "Status code for a Response must be a value between 100 and 599, a [1-5]XX wildcard, or 'default'",
        "Violation"
      ),
      AMFValidation(
        Oas30.name,
        "Domain",
        "security:OAuth2Flow",
        "security:flow",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^(implicit|password|clientCredentials|authorizationCode)$",
        "Invalid flow. The options are: implicit, password, clientCredentials or authorizationCode",
        "Invalid flow. The options are: implicit, password, clientCredentials or authorizationCode",
        "Violation"
      ),
      AMFValidation(
        Oas30.name,
        "Domain",
        "security:Settings",
        "security:in",
        "PropertyShape",
        "sh:path",
        "sh:pattern",
        "^(query|header|cookie)$",
        "Invalid 'in' value. The options are: query, header or cookie",
        "Invalid 'in' value. The options are: query, header or cookie",
        "Violation"
      ),
      AMFValidation(
        "amf-parser:example-mutually-exclusive-fields",
        "Example 'value' and 'externalValue' fields are mutually exclusive",
        Oas30.name,
        "Domain",
        "apiContract:Example",
        "doc:externalValue",
        "PropertyShape",
        "sh:path",
        "raml-shapes:exampleMutuallyExclusiveFields",
        "0",
        "",
        "Example 'value' and 'externalValue' fields are mutually exclusive",
        "Violation"
      ),
      AMFValidation(
        Oas30.name,
        "Domain",
        "apiContract:Parameter",
        "apiContract:payload",
        "PropertyShape",
        "sh:path",
        "sh:maxCount",
        "1",
        "",
        "Parameters 'content' field must only have one entry",
        "Violation"
      ),
      urlValidation(Oas30.name, "apiContract:WebAPI", "core:termsOfService"),
      AMFValidation(
        Oas30.name,
        "Domain",
        "security:HttpSettings",
        "security:scheme",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "'scheme' field is mandatory in http security scheme",
        "'scheme' field is mandatory in http security scheme",
        "Violation"
      ),
      AMFValidation(
        Oas30.name,
        "Domain",
        "security:ApiKeySettings",
        "core:name",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "'name' field is mandatory in apiKey security scheme",
        "'name' field is mandatory in apiKey security scheme",
        "Violation"
      ),
      AMFValidation(
        Oas30.name,
        "Domain",
        "security:ApiKeySettings",
        "security:in",
        "PropertyShape",
        "sh:path",
        "sh:minCount",
        "1",
        "'in' field is mandatory in apiKey security scheme",
        "'in' field is mandatory in apiKey security scheme",
        "Violation"
      ),
      AMFValidation(
        Oas30.name,
        "Domain",
        "security:SecurityScheme",
        "security:settings",
        "PropertyShape",
        "sh:path",
        "raml-shapes:requiredOpenIdConnectUrl",
        "0",
        "'openIdConnectUrl' field is mandatory in openIdConnect security scheme",
        "'openIdConnectUrl' field is mandatory in openIdConnect security scheme",
        "Violation"
      ),
      AMFValidation(
        Oas30.name,
        "Domain",
        "security:SecurityScheme",
        "security:settings",
        "PropertyShape",
        "sh:path",
        "raml-shapes:requiredFlowsInOAuth2",
        "0",
        "'flows' field is mandatory in OAuth2 security scheme",
        "'flows' field is mandatory in OAuth2 security scheme",
        "Violation"
      ),
      AMFValidation(
        Oas30.name,
        "Domain",
        "apiContract:Callback",
        "apiContract:expression",
        "PropertyShape",
        "sh:path",
        "raml-shapes:validCallbackExpression",
        "0",
        "Does not comply with runtime expression ABNF syntax",
        "Does not comply with runtime expression ABNF syntax",
        "Violation"
      ),
      AMFValidation(
        Oas30.name,
        "Domain",
        "apiContract:TemplatedLink",
        "apiContract:requestBody",
        "PropertyShape",
        "sh:path",
        "raml-shapes:validLinkRequestBody",
        "0",
        "Does not comply with runtime expression ABNF syntax",
        "Does not comply with runtime expression ABNF syntax",
        "Violation"
      ),
      AMFValidation(
        Oas30.name,
        "Domain",
        "apiContract:TemplatedLink",
        "apiContract:mapping",
        "PropertyShape",
        "sh:path",
        "raml-shapes:validLinkParameterExpressions",
        "0",
        "Does not comply with runtime expression ABNF syntax",
        "Does not comply with runtime expression ABNF syntax",
        "Violation"
      )
    )
    override def validations(): Seq[AMFValidation] = result
  }

  val map: Map[ProfileName, Seq[AMFValidation]] = Map(
    AmfProfile     -> forProfile(AmfProfile),
    Raml10Profile  -> forProfile(Raml10Profile),
    Raml08Profile  -> forProfile(Raml08Profile),
    RamlProfile    -> forProfile(RamlProfile), // ???
    Oas20Profile   -> forProfile(Oas20Profile),
    Oas30Profile   -> forProfile(Oas30Profile),
    OasProfile     -> forProfile(OasProfile),
    Async20Profile -> forProfile(Async20Profile)
  )

  private def forProfile(p: ProfileName): Seq[AMFValidation] = {
    p match {
      case Raml10Profile | RamlProfile => Raml10Validations.validations()
      case Raml08Profile               => Raml08Validations.validations()
      case OasProfile | Oas20Profile   => Oas20Validations.validations()
      case Oas30Profile                => Oas30Validations.validations()
      case Async20Profile              => Async20Validations.validations()
      case AmfProfile                  => AmfValidations.validations()
      case _                           => Nil
    }
  }
}
