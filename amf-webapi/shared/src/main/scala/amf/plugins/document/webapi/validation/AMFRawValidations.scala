// auto-generated class from ValidationsImporter.toScala
package amf.plugins.document.webapi.validation

// scalastyle:off line.contains.tab
object AMFRawValidations {
  val raw = List(
    "amf-parser:mandatory-api-version	Missing madatory Swagger / info / version	OpenAPI	Domain	schema-org:WebAPI	schema-org:version	PropertyShape	sh:path	sh:minCount	1	API Version is Mandatory	Version is mandatory in Info object",
    "amf-parser:raml-schemes	Protocols property must be http or https	RAML	Domain	schema-org:WebAPI	raml-http:scheme	PropertyShape	sh:path	sh:in	http,https,HTTP,HTTPS	Protocols must have a case insensitive value matching http or https	Swagger object 'schemes' property must have a case insensitive value matching http or https",
    "amf-parser:raml-schemes	Protocols property must be http or https	RAML08	Domain	schema-org:WebAPI	raml-http:scheme	PropertyShape	sh:path	sh:in	http,https,HTTP,HTTPS	Protocols must have a case insensitive value matching http or https	Swagger object 'schemes' property must have a case insensitive value matching http or https",
    "amf-parser:openapi-schemes	Protocols property must be http,https,ws,wss	OpenAPI	Domain	schema-org:WebAPI	raml-http:scheme	PropertyShape	sh:path	sh:in	http,https,ws,wss	Protocols must match a value http, https, ws or wss	Swagger object 'schemes' property must have a value matching http, https, ws or wss",
    "amf-parser:mandatory-external-doc-url	Swagger external-doc element without URL	OpenAPI	Domain	schema-org:CreativeWork	schema-org:url	PropertyShape	sh:path	sh:minCount	1	Documentation URL is mandatory in API external documentation	URL is mandatory in External Documentation object",
    "amf-parser:mandatory-license-name	Swagger License node without name	OpenAPI	Domain	raml-http:License	schema-org:name	PropertyShape	sh:path	sh:minCount	1	License name is mandatory if license information is included	Name is mandatory in License object",
    "amf-parser:strict-url-strings	URLs in values mapped to schema-org:url must be valid	AMF	Domain	raml-doc:DomainElement	schema-org:url	NodeShape	sh:targetObjectsOf	sh:nodeKind	sh:IRI	URLs must be valid	URLs must be valid",
    "amf-parser:host-valid-domain	Domains must be valid domain names	OpenAPI	Domain	schema-org:WebAPI	raml-http:host	PropertyShape	sh:path	sh:pattern	^[^{}\\/ :\\\\]+(?::\\d+)?$	BaseUris must be valid and not templates	Host information must be valid host",
    "amf-parser:empty-responses	No responses declared	OpenAPI	Domain	hydra:Operation	hydra:returns	PropertyShape	sh:path	sh:minCount	1	Responses array cannot be empty	Responses cannot be empty",
    "amf-parser:empty-enum	Enum in types cannot be empty	OpenAPI	Domain	raml-shapes:Shape	sh:in	PropertyShape	sh:path	sh:node	amf-parser:NonEmptyList	Property 'enum' must have at least one value	Property 'enum' for a Schema object must have at least one value",
    "amf-parser:raml-status-code	Status code must match a valid numeric status code	RAML	Domain	raml-http:Response	hydra:statusCode	PropertyShape	sh:path	sh:pattern	^([0-9]{3})$	Status code must be numeric	Status code must be numeric",
    "amf-parser:raml-status-code	Status code must match a valid numeric status code	RAML08	Domain	raml-http:Response	hydra:statusCode	PropertyShape	sh:path	sh:pattern	^([0-9]{3})$	Status code must be numeric	Status code must be numeric",
    "amf-parser:array-shape-items-mandatory	Declaration of the type of the items for an array is required	OpenAPI	Domain	raml-shapes:ArrayShape	raml-shapes:item	PropertyShape	sh:path	sh:minCount	1	items facet of RAML Array type is required	items property of Schame objects of type 'array' is required",
    "amf-parser:array-shape-items-optional	Declaration of the type of the items for an array is optional	RAML	Domain	raml-shapes:ArrayShape	raml-shapes:item	PropertyShape	sh:path	sh:minCount	0	items facet of RAML Array type is required	items property of Schame objects of type 'array' is required",
    "amf-parser:array-shape-items-optional	Declaration of the type of the items for an array is optional	RAML08	Domain	raml-shapes:ArrayShape	raml-shapes:item	PropertyShape	sh:path	sh:minCount	0	items facet of RAML Array type is required	items property of Schame objects of type 'array' is required",
    "amf-parser:multiple-of	Multiple of should be greater than 0	RAML	Domain	sh:ScalarShape	raml-shapes:multipleOf	PropertyShape	sh:path	raml-shapes:multipleOfValidation	0	Multiple of facet should be greater than 0	Multiple of facet should be greater than 0",
    "amf-parser:multiple-of	Multiple of should be greater than 0	RAML08	Domain	sh:ScalarShape	raml-shapes:multipleOf	PropertyShape	sh:path	raml-shapes:multipleOfValidation	0	Multiple of facet should be greater than 0	Multiple of facet should be greater than 0",
    "amf-parser:max-length	Max length should be greater or equal than 0	RAML	Domain	sh:ScalarShape	sh:maxLength	PropertyShape	sh:path	raml-shapes:maxLengthValidation	0	Max length facet should be greater or equal than 0	Max length facet should be greater or equal than 0",
    "amf-parser:max-length	Max length should be greater or equal than 0	RAML08	Domain	sh:ScalarShape	sh:maxLength	PropertyShape	sh:path	raml-shapes:maxLengthValidation	0	Max length facet should be greater or equal than 0	Max length facet should be greater or equal than 0",
    "amf-parser:min-length	Min length should be greater or equal than 0	RAML	Domain	sh:ScalarShape	sh:minLength	PropertyShape	sh:path	raml-shapes:minLengthValidation	0	Min length facet should be greater or equal than 0	Min length facet should be greater or equal than 0",
    "amf-parser:min-length	Min length should be greater or equal than 0	RAML08	Domain	sh:ScalarShape	sh:minLength	PropertyShape	sh:path	raml-shapes:minLengthValidation	0	Min length facet should be greater or equal than 0	Min length facet should be greater or equal than 0",
    "		AMF	Domain	raml-doc:DomainElement	schema-org:name	PropertyShape	sh:path	sh:datatype	xsd:string	Title and names must be string	Names must be string",
    "		AMF	Domain	raml-doc:DomainElement	schema-org:name	PropertyShape	sh:path	sh:maxCount	1	Titles and names must be single values	Names and titles must be single values",
    "		AMF	Domain	raml-doc:DomainElement	schema-org:description	PropertyShape	sh:path	sh:datatype	xsd:string	Descriptions must be strings	Description must be strings",
    "		AMF	Domain	raml-doc:DomainElement	schema-org:description	PropertyShape	sh:path	sh:maxCount	1	Descriptions must be single values	Descriptions must be single values",
    "		AMF	Domain	schema-org:WebAPI	schema-org:name	PropertyShape	sh:path	sh:minCount	1	API name must be a single value	Info object 'title' must be a single value",
    "		AMF	Domain	schema-org:WebAPI	raml-http:host	PropertyShape	sh:path	sh:datatype	xsd:string	API baseUri host information must be a string	Swagger object 'host' must be a string",
    "		AMF	Domain	schema-org:WebAPI	raml-http:host	PropertyShape	sh:path	sh:maxCount	1	API baseUri host information must be a single value	Swagger object 'host' must be a single value",
    "		AMF	Domain	schema-org:WebAPI	raml-http:scheme	PropertyShape	sh:path	sh:datatype	xsd:string	API BaseUri scheme information must be a string	Swagger object 'schemes' must be strings",
    "		AMF	Domain	schema-org:WebAPI	raml-http:basePath	PropertyShape	sh:path	sh:datatype	xsd:string	API baseUri path must be a string	Swagger object 'basePath' must be a string",
    "		AMF	Domain	schema-org:WebAPI	raml-http:basePath	PropertyShape	sh:path	sh:maxCount	1	API baseUri path must a single value	Swagger object 'basePath' must be a single value.",
    "		AMF	Domain	schema-org:WebAPI	raml-http:basePath	PropertyShape	sh:path	sh:pattern	^/	API baseUri path must start with a '/'	Swagger object 'basePath' must start with a '/'.",
    "		AMF	Domain	schema-org:WebAPI	raml-http:accepts	PropertyShape	sh:path	sh:datatype	xsd:string	API default media types consumed must be strings	Swagger object 'consumes' must be strings",
    "		AMF	Domain	schema-org:WebAPI	raml-http:mediaType	PropertyShape	sh:path	sh:datatype	xsd:string	API default media types produced must be strings	Swagger object 'produces' must be strings",
    "		AMF	Domain	schema-org:WebAPI	schema-org:version	PropertyShape	sh:path	sh:datatype	xsd:string	API version must be a string	Info object 'version' must be string",
    "		AMF	Domain	schema-org:WebAPI	schema-org:version	PropertyShape	sh:path	sh:maxCount	1	API version must be a single value	Info object 'version' must be a single value",
    "		AMF	Domain	schema-org:WebAPI	schema-org:termsOfService	PropertyShape	sh:path	sh:datatype	xsd:string	API terms of service must be a string	Info object 'termsOfService' must be string",
    "		AMF	Domain	schema-org:WebAPI	schema-org:termsOfService	PropertyShape	sh:path	sh:maxCount	1	API terms of service must be a single value	Info object 'termsOfService' must a single value",
    "		AMF	Domain	schema-org:WebAPI	schema-org:provider	PropertyShape	sh:path	sh:class	schema-org:Organization	API provider must hold Provider information	Info object 'contact' must be a Contact object",
    "		AMF	Domain	schema-org:WebAPI	schema-org:provider	PropertyShape	sh:path	sh:maxCount	1	API provider must be a single value	Info object 'contact' must be a single value",
    "		AMF	Domain	schema-org:WebAPI	raml-http:endpoint	PropertyShape	sh:path	sh:class	raml-http:EndPoint	API paths must link Resources	Paths object must link PathItem objects",
    "		AMF	Domain	schema-org:WebAPI	raml-http:parameter	PropertyShape	sh:path	sh:class	raml-http:Parameter	API parameters must be valid parameters	Swagger object 'parameters' must be valid Parameter objects",
    "		AMF	Domain	schema-org:Organization	schema-org:url	PropertyShape	sh:path	sh:maxCount	1	API provider URL must be a single value	Contact object 'url' must be a single value",
    "		AMF	Domain	schema-org:Organization	schema-org:email	PropertyShape	sh:path	sh:datatype	xsd:string	API provider email must be a string	Contact object 'email' must be a string",
    "		AMF	Domain	schema-org:Organization	schema-org:email	PropertyShape	sh:path	sh:maxCount	1	API provider must be a single value	Contact object 'email' must be a single value",
    "		AMF	Domain	raml-http:EndPoint	raml-http:path	PropertyShape	sh:path	sh:maxCount	1	Resource path must be unique	PathItem object must have a single path",
    "		AMF	Domain	raml-http:EndPoint	raml-http:path	PropertyShape	sh:path	sh:datatype	xsd:string	Resource path must be a string	PathItem object path must be a string",
    "		AMF	Domain	raml-http:EndPoint	raml-http:path	PropertyShape	sh:path	sh:pattern	^/	Resource path must start with a '/'	PathItem path must start with a '/'",
    "		AMF	Domain	raml-http:Endpoint	hydra:supportedOperation	PropertyShape	sh:path	sh:class	hydra:Operation	Resources must hold Methods operation	PathItems must hold Operations information",
    "		AMF	Domain	raml-http:Endpoint	raml-http:parameter	PropertyShape	sh:path	sh:class	raml-http:Parameter	Resources uriParameters must be valid parameters	PathItems 'parameters' property must be valid parameters",
    "		AMF	Domain	hydra:Operation	hydra:method	PropertyShape	sh:path	sh:in	get,put,post,delete,options,head,patch	Uknown method	Uknown Operation method",
    "		AMF	Domain	hydra:Operation	hydra:method	PropertyShape	sh:path	sh:maxCount	1	Methods can only have a single HTTP verbe	Operations can only have a single HTTP verb",
    "		AMF	Domain	hydra:Operation	hydra:returns	PropertyShape	sh:path	sh:class	raml-http:Response	Methods must hold Responses information	Operations must hold Responses information",
    "		AMF	Domain	hydra:Operation	raml-http:guiSummary	PropertyShape	sh:path	sh:datatype	xsd:string	Methods' summary information must be a string	Methods' summary information must be a string",
    "		AMF	Domain	hydra:Operation	raml-http:guiSummary	PropertyShape	sh:path	sh:maxCount	1	Methods can only have a single value for the summary information	Methods can only have a single value for summary information",
    "		AMF	Domain	hydra:Operation	raml-doc:deprecated	PropertyShape	sh:path	sh:datatype	xsd:boolean	Methods' deprecated must be a boolean	Methods' deprecated must be a boolean",
    "		AMF	Domain	hydra:Operation	raml-doc:deprecated	PropertyShape	sh:path	sh:maxCount	1	Methods' depcrecated must be a single value	Methods' deprecated must be a single value",
    "		AMF	Domain	hydra:Operation	raml-http:scheme	PropertyShape	sh:path	sh:datatype	xsd:string	protocols must be strings	schemes must be strings",
    "		AMF	Domain	hydra:Operation	raml-http:accepts	PropertyShape	sh:path	sh:datatype	xsd:string	Method default media types consumed must be strings	Operation object 'consumes' must be strings",
    "		AMF	Domain	hydra:Operation	hydra:returns	PropertyShape	sh:path	sh:class	raml-http:Response	An operation must have valid response information	Operation object must return a valid Response object",
    "		AMF	Domain	raml-http:Request	raml-http:header	PropertyShape	sh:path	sh:class	raml-http:Parameter	Header information must be a valid header	Header information must be a valid header",
    "		AMF	Domain	raml-http:Request	raml-http:payload	PropertyShape	sh:path	sh:class	raml-http:Payload	Payload information for a Method request info must be valid	Payload information for an Operation object request info must be valid",
    "		AMF	Domain	raml-http:Request	raml-http:parameter	PropertyShape	sh:path	sh:class	raml-http:Parameter	Parameter information for a Method must be valid	Parameter information for an Operation object must be valid",
    "		AMF	Domain	raml-http:Response	hydra:statusCode	PropertyShape	sh:path	sh:datatype	xsd:string	Status code for a Response must be a string	Status code for a Response object must be a string",
    "		AMF	Domain	raml-http:Response	hydra:statusCode	PropertyShape	sh:path	sh:pattern	^([0-9]{3})$|^(default)$	Status code for a Response must match '^([0-9]{3})$|^(default)$'	Status code for a Response must match '^([0-9]{3})$|^(default)$'",
    "		AMF	Domain	raml-http:Response	raml-http:header	PropertyShape	sh:path	sh:class	raml-http:Parameter	Header information must be a valid header	Header information must be a valid header",
    "		AMF	Domain	raml-http:Response	raml-http:payload	PropertyShape	sh:path	sh:class	raml-http:Payload	Payload information for a Response must be valid	Payload information for a response must be valid",
    "		AMF	Domain	raml-http:Parameter	schema-org:name	PropertyShape	sh:path	sh:minCount	1	Parameter information must have a name	Parameter object must have a name property",
    "		AMF	Domain	raml-http:Parameter	hydra:required	PropertyShape	sh:path	sh:datatype	xsd:boolean	Information about required parameters must be a boolean value	Required property of a Parameter object must be boolean",
    "		AMF	Domain	raml-http:Parameter	hydra:required	PropertyShape	sh:path	sh:minCount	1	Required information about a parameter is mandatory	Required property of a Parameter object is mandatory",
    "		AMF	Domain	raml-http:Parameter	hydra:required	PropertyShape	sh:path	sh:maxCount	1	Only one value is allowed for required information of a parameter	Required property of a Parameter object must be a single value",
    "		AMF	Domain	raml-http:Parameter	raml-http:binding	PropertyShape	sh:path	sh:datatype	xsd:string	Information about the binding of the parameter is mandatory	in' property of a Parameter object must be a string",
    "		AMF	Domain	raml-http:Parameter	raml-http:binding	PropertyShape	sh:path	sh:minCount	1	Binding information for a parameter is mandatory	in' property of a Parameter object is mandatory",
    "		AMF	Domain	raml-http:Parameter	raml-http:binding	PropertyShape	sh:path	sh:maxCount	1	Only one binding is allowed per parameter	in' property of a Parameter object must be a single value",
    "		AMF	Domain	raml-http:Parameter	raml-http:binding	PropertyShape	sh:path	sh:in	query,path,header,uri	Binding information for a parameter with an invalid value	in' property of a parameter with an invalid value",
    "		AMF	Domain	raml-http:Parameter	raml-http:schema	PropertyShape	sh:path	sh:class	sh:Shape	Expected RAML Type not found	Expected Schema object not found",
    "		AMF	Domain	raml-http:Parameter	raml-http:schema	PropertyShape	sh:path	sh:maxCount	1	Only one RAML type can be specified	Only one Schema object can be specified",
    "		AMF	Domain	raml-http:Parameter	raml-http:schema	PropertyShape	sh:path	sh:minCount	1	RAML Type information is mandatory for parameters	Schema/type information required for Parameter objects",
    "		AMF	Domain	raml-http:Payload	raml-http:mediaType	PropertyShape	sh:path	sh:datatype	xsd:string	Method default media types produced must be strings	Operation object 'produces' must be strings",
    "		AMF	Domain	raml-http:Payload	raml-http:mediaType	PropertyShape	sh:path	sh:maxCount	1	Media type maximum cardinality is 1 per payloa	Media type maximum cardinality is 1 per payload",
    "		AMF	Domain	raml-http:Payload	raml-http:schema	PropertyShape	sh:path	sh:class	sh:Shape	Expected RAML Type not found	Expected Schema object not found",
    "		AMF	Domain	raml-http:Payload	raml-http:schema	PropertyShape	sh:path	sh:maxCount	1	Only one RAML type can be specified	Only one Schema object can be specified",
    "		AMF	Domain	raml-shapes:Shape	raml-shapes:xmlSerialization	PropertyShape	sh:path	sh:class	raml-shapes:XMLSerializer	Property 'xml' of a RAML type must have as a value a valid XML Serialization	Property 'xml' of a Schema object must have as a value a valid XML object",
    "		AMF	Domain	raml-shapes:Shape	raml-shapes:xmlSerialization	PropertyShape	sh:path	sh:maxCount	1	XML serialisation object must be a single value	XML Object for the 'xml' property of a Schema object must be  single value",
    "		AMF	Domain	raml-shapes:Shape	sh:in	PropertyShape	sh:path	sh:maxCount	1	Property 'enum'  must have a single list of values	Property 'enum' for a Schema object must have a single list of values",
    "		AMF	Domain	raml-shapes:Shape	sh:in	PropertyShape	sh:path	sh:node	amf-parser:NonEmptyList	Only 1 array can be specified in RAML type enum	Ony 1 array can be specified in Schame object enum property",
    "		AMF	Domain	raml-shapes:XMLSerializer	raml-shapes:xmlAtribute	PropertyShape	sh:path	sh:datatype	xsd:boolean	XML attribute serialisation info must be boolean	XML attribute serialisation info must be boolean",
    "		AMF	Domain	raml-shapes:XMLSerializer	raml-shapes:xmlAtribute	PropertyShape	sh:path	sh:maxCount	1	property 'attribute' of the XML serialisation mut be a single vlaue	property 'attribute' of the XML serialisation mut be a single vlaue",
    "		AMF	Domain	raml-shapes:XMLSerializer	raml-shapes:xmlWrapped	PropertyShape	sh:path	sh:datatype	xsd:boolean	XML wraping serialisation info must be boolean	XML wrapping serialisation info must be boolean",
    "		AMF	Domain	raml-shapes:XMLSerializer	raml-shapes:xmlWrapped	PropertyShape	sh:path	sh:maxCount	1	property 'wrapped' of the XML serialisation mut be a single vlaue	property 'wrapped' of the XML serialisation mut be a single vlaue",
    "		AMF	Domain	raml-shapes:XMLSerializer	raml-shapes:xmlName	PropertyShape	sh:path	sh:datatype	xsd:string	XML name serialisation info must be string	XML name serialisation info must be string",
    "		AMF	Domain	raml-shapes:XMLSerializer	raml-shapes:xmlName	PropertyShape	sh:path	sh:maxCount	1	property 'name' of the XML serialisation mut be a single vlaue	property 'name' of the XML serialisation mut be a single vlaue",
    "		AMF	Domain	raml-shapes:XMLSerializer	raml-shapes:xmlNamespace	PropertyShape	sh:path	sh:datatype	xsd:string	XML namespace serialisation info must be string	XML namespace serialisation info must be string",
    "		AMF	Domain	raml-shapes:XMLSerializer	raml-shapes:xmlNamespace	PropertyShape	sh:path	sh:maxCount	1	property 'namespace' of the XML serialisation mut be a single vlaue	property 'namespace' of the XML serialisation mut be a single vlaue",
    "		AMF	Domain	raml-shapes:XMLSerializer	raml-shapes:xmlPrefix	PropertyShape	sh:path	sh:datatype	xsd:string	XML prefix serialisation info must be string	XML prefix serialisation info must be string",
    "		AMF	Domain	raml-shapes:XMLSerializer	raml-shapes:xmlPrefix	PropertyShape	sh:path	sh:maxCount	1	Property 'prefix' of the XML serialisation mut be a single vlaue	Property 'prefix' of the XML serialisation mut be a single vlaue",
    "		AMF	Domain	raml-shapes:Shape	sh:defaultValue	PropertyShape	sh:path	sh:maxCount	1	Default value for a RAML type must be unique	Default property for an Schema object must be unique",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:minProperties	PropertyShape	sh:path	sh:minInclusive	0	minProperties for a RAML Object type cannot be negative	minProperties for a Schema object cannot be negative",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:minProperties	PropertyShape	sh:path	sh:datatype	xsd:integer	minProperties for a RAML Object type must be an integer	minProperties for a Schema object must be an integer",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:minProperties	PropertyShape	sh:path	sh:maxCount	1	minProperties for a RAML Object type must be unique	minProperties for a Schema object must be unique",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:maxProperties	PropertyShape	sh:path	sh:minInclusive	0	maxProperties for a RAML Object type cannot be negative	maxProperties for a Schema object cannot be negative",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:maxProperties	PropertyShape	sh:path	sh:datatype	xsd:integer	maxProperties for a RAML Object type must be an integer	maxProperties for a Schema object must be an integer",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:maxProperties	PropertyShape	sh:path	sh:maxCount	1	maxProperties for a RAML Object type must be unique	maxProperties for a Schema object must be unique",
    "		AMF	Domain	raml-shapes:ObjectShape	sh:closed	PropertyShape	sh:path	sh:datatype	xsd:boolean	additionalProperties for a RAML Object type must be a boolean	additionalProperties for a Schema object must be a boolean",
    "		AMF	Domain	raml-shapes:ObjectShape	sh:closed	PropertyShape	sh:path	sh:maxCount	1	additionalProperties for a RAML Object must be unique	additionalProperties for a Schema object must be unique",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:discriminator	PropertyShape	sh:path	sh:datatype	xsd:string	discriminator for RAML Object type must be a string value	discriminator for a Schema object must be a string value",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:discriminator	PropertyShape	sh:path	sh:maxCount	1	discriminator for RAML Object type must be unique	discriminator for a Schema object must be unique",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:discriminatorValue	PropertyShape	sh:path	sh:datatype	xsd:string	x-discriminatorValue for RAML Object type must be a string value	discriminatorValue for a Schema object must be a string value",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:discriminatorValue	PropertyShape	sh:path	sh:maxCount	1	x-discriminatorValue for RAML Object type must be unique	discriminatorValue for a Schema object must be unique",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:readOnly 	PropertyShape	sh:path	sh:datatype	xsd:boolean	(readOnly) for a RAML Object type must be a boolean	readOnly for a Schema object must be a boolean",
    "		AMF	Domain	raml-shapes:ObjectShape	raml-shapes:readOnly 	PropertyShape	sh:path	sh:maxCount	1	(readOnly) for a RAML Object must be unique	readOnly for a Schema object must be unique",
    "		AMF	Domain	raml-shapes:ArrayShape	raml-shapes:item	PropertyShape	sh:path	sh:class	raml-shapes:Shape	value of the items facet must be a RAML Type 	value of the items property must be a Schema node",
    "		AMF	Domain	raml-shapes:ArrayShape	sh:minCount	PropertyShape	sh:path	sh:datatype	xsd:integer	minItems for a RAML Array type must be an integer	minItems of a Schema object of type 'array' must be an integer",
    "		AMF	Domain	raml-shapes:ArrayShape	sh:minCount	PropertyShape	sh:path	sh:maxCount	1	minItems for a RAML Array type must be unique	minItems of a Schema object of type 'array' must be unique",
    "		AMF	Domain	raml-shapes:ArrayShape	sh:minCount	PropertyShape	sh:path	sh:minInclusive 	0	maxItems for a RAML Array type must be greater than 0	maxItems of a Schema object of type 'array' must be greater than 0",
    "		AMF	Domain	raml-shapes:ArrayShape	sh:maxCount	PropertyShape	sh:path	sh:datatype	xsd:integer	maxItems for a RAML Array type must be an integer	maxItems of a Schema object of type 'array' must be an integer",
    "		AMF	Domain	raml-shapes:ArrayShape	sh:maxCount	PropertyShape	sh:path	sh:maxCount	1	maxItems for a RAML Array type must be unique	maxItems of a Schema object of type 'array' must be unique",
    "		AMF	Domain	raml-shapes:ArrayShape	sh:minCount	PropertyShape	sh:path	sh:minInclusive 	0	minItems for a RAML Array type must be greater than 0	minItems of a Schema object of type 'array' must be greater than 0",
    "		AMF	Domain	raml-shapes:ArrayShape	raml-shapes:uniqueItems	PropertyShape	sh:path	sh:datatype	xsd:boolean	uniqueItems for a RAML Array type must be a boolean	uniqueItems of a Schema object of type 'array' must be a boolean",
    "		AMF	Domain	raml-shapes:ArrayShape	raml-shapes:uniqueItems	PropertyShape	sh:path	sh:maxCount	1	uniqueItems for a RAML Array type must be unique	uniqueItems of a Schema object of type 'array' must be unique",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:pattern	PropertyShape	sh:path	sh:datatype	xsd:string	pattern facet for a RAML scalar type must be a string	pattern for scalar Schema object of scalar type must be a string",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:pattern	PropertyShape	sh:path	sh:maxCount	1	pattern facet for a RAML scalar type must be unique	pattern for scalar Schema object of scalar type must be unique",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:minLength	PropertyShape	sh:path	sh:datatype	xsd:integer	minLength facet for a RAML scalar type must be a integer	minLength for scalar Schema object of scalar type must be a integer",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:minLength	PropertyShape	sh:path	sh:maxCount	1	minLength facet for a RAML scalar type must be unique	minLength for scalar Schema object of scalar type must be unique",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:maxLength	PropertyShape	sh:path	sh:datatype	xsd:integer	maxLength facet for a RAML scalar type must be a integer	maxLength for scalar Schema object of scalar type must be a integer",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:maxLength	PropertyShape	sh:path	sh:maxCount	1	maxLength facet for a RAML scalar type must be unique	maxLength for scalar Schema object of scalar type must be unique",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:minInclusive	PropertyShape	sh:path	sh:datatype	xsd:integer	minimum facet for a RAML scalar type must be a integer	minimum for scalar Schema object of scalar type must be a integer",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:minInclusive	PropertyShape	sh:path	sh:maxCount	1	minimum facet for a RAML scalar type must be unique	minimum for scalar Schema object of scalar type must be unique",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:maxInclusive	PropertyShape	sh:path	sh:datatype	xsd:integer	maximum facet for a RAML scalar type must be a integer	maximum for scalar Schema object of scalar type must be a integer",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:maxInclusive	PropertyShape	sh:path	sh:maxCount	1	maximum facet for a RAML scalar type must be unique	maximum for scalar Schema object of scalar type must be unique",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:minExclusive	PropertyShape	sh:path	sh:datatype	xsd:boolean	x-exclusiveMinimum facet for a RAML scalar type must be a boolean	exclusiveMinimum for scalar Schema object of scalar type must be a boolean",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:minExclusive	PropertyShape	sh:path	sh:maxCount	1	x-exclusiveMinimum facet for a RAML scalar type must be unique	exclusiveMinimum for scalar Schema object of scalar type must be unique",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:maxExclusive	PropertyShape	sh:path	sh:datatype	xsd:boolean	x-exclusiveMaximum facet for a RAML scalar type must be a boolean	exclusiveMaximum for scalar Schema object of scalar type must be a boolean",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:maxExclusive	PropertyShape	sh:path	sh:maxCount	1	x-exclusiveMaximum facet for a RAML scalar type must be unique	exclusiveMaximum for scalar Schema object of scalar type must be unique",
    "		AMF	Domain	raml-shapes:ScalarShape	raml-shapes:format	PropertyShape	sh:path	sh:datatype	xsd:string	format facet for a RAML scalar type must be a string	format for scalar Schema object of scalar type must be a string",
    "		AMF	Domain	raml-shapes:ScalarShape	raml-shapes:format	PropertyShape	sh:path	sh:maxCount	1	format facet for a RAML scalar type must be unique	format for scalar Schema object of scalar type must be unique",
    "		AMF	Domain	raml-shapes:ScalarShape	raml-shapes:multipleOf	PropertyShape	sh:path	sh:datatype	xsd:integer	multipleOf facet for a RAML scalar type must be a integer	multipleOf for scalar Schema object of scalar type must be a integer",
    "		AMF	Domain	raml-shapes:ScalarShape	raml-shapes:multipleOf	PropertyShape	sh:path	sh:maxCount	1	multipleOf facet for a RAML scalar type must be unique	multipleOf for scalar Schema object of scalar type must be unique",
    "		AMF	Domain	raml-shapes:ScalarShape	raml-shapes:multipleOf	PropertyShape	sh:path	sh:minExclusive	0	multipleOf facet for a RAML scalar type must be greater than 0	multipleOf for scalar Schema object of scalar type must be greater than 0",
    "		AMF	Domain	raml-shapes:ScalarShape	sh:datatype	PropertyShape	sh:path	sh:minCount	1	type information for a RAML scalar is required	type information fo a Schema object of scalar type is required",
    "		AMF	Domain	raml-doc:DomainProperty	raml-shapes:schema	PropertyShape	sh:path	sh:minCount	1	type is mandatory for a RAML annotationType	schema is mandatory for an extension type",
    "		AMF	Domain	raml-doc:DomainProperty	raml-shapes:schema	PropertyShape	sh:path	sh:maxCount	1	type must be a single value for a RAML annotationType	schema must be a single value for an extension type",
    "		AMF	Domain	raml-doc:DomainProperty	raml-shapes:schema	PropertyShape	sh:path	sh:class	raml-shapes:Shape	type value must be a RAML Type	schema value must be a Schema object"
  )
}
