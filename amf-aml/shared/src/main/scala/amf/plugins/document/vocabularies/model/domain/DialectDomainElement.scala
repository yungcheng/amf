package amf.plugins.document.vocabularies.model.domain

import amf.core.metamodel.domain.{DomainElementModel, LinkableElementModel}
import amf.core.metamodel.{Field, Obj, Type}
import amf.core.model.{BoolField, StrField}
import amf.core.model.domain._
import amf.core.parser.{Annotations, Fields, Value}
import amf.core.vocabulary.{Namespace, ValueType}
import amf.plugins.document.vocabularies.metamodel.domain.DialectDomainElementModel
import org.mulesoft.common.time.SimpleDateTime
import org.yaml.model.{YMap, YNode}

import scala.collection.mutable

class UnknownMapKeyProperty(val id: String) extends Exception

case class DialectDomainElement(override val fields: Fields, annotations: Annotations)
    extends DynamicDomainElement
    with Linkable {

  // storage of a potential node parsed through a plugin, wrapped in this dialect domaine element
  val pluginProperties: mutable.Map[String, DomainElement]                      = mutable.HashMap()
  val pluginCollectionProperties: mutable.Map[String, Seq[DomainElement]]       = mutable.HashMap()
  // Storage for default dialect domain elements
  val literalProperties: mutable.Map[String, Any]                                = mutable.HashMap()
  val linkProperties: mutable.Map[String, Any]                                   = mutable.HashMap()
  val mapKeyProperties: mutable.Map[String, Any]                                 = mutable.HashMap()
  val objectProperties: mutable.Map[String, DialectDomainElement]                = mutable.HashMap()
  val objectCollectionProperties: mutable.Map[String, Seq[DialectDomainElement]] = mutable.HashMap()
    val propertyAnnotations: mutable.Map[String, Annotations]                      = mutable.HashMap()

  def isAbstract: BoolField = fields.field(meta.asInstanceOf[DialectDomainElementModel].Abstract)
  def withAbstract(isAbstract: Boolean): DialectDomainElement = {
    set(meta.asInstanceOf[DialectDomainElementModel].Abstract, isAbstract)
    this
  }

  def declarationName: StrField = fields.field(meta.asInstanceOf[DialectDomainElementModel].DeclarationName)
  def withDeclarationName(name: String): DialectDomainElement = {
    set(meta.asInstanceOf[DialectDomainElementModel].DeclarationName, name)
    this
  }

  // Types of the instance
  protected var instanceTypes: Seq[String] = Nil
  def withInstanceTypes(types: Seq[String]): DialectDomainElement = {
    instanceTypes = types
    this
  }

  // Dialect mapping defining the instance
  protected var instanceDefinedBy: Option[NodeMappable] = None
  def withDefinedBy(nodeMapping: NodeMappable): DialectDomainElement = {
    instanceDefinedBy = Some(nodeMapping)
    this
  }
  def definedBy: NodeMappable = instanceDefinedBy match {
    case Some(mapping) => mapping
    case None          => throw new Exception("NodeMapping for the instance not defined")
  }

  def localRefName: String = {
    if (isLink)
      linkTarget.map(_.id.split("#").last.split("/").last).getOrElse {
        throw new Exception(s"Cannot produce local reference without linked element at elem $id")
      } else id.split("#").last.split("/").last
  }

  def includeName: String = {
    if (isLink)
      linkLabel
        .option()
        .getOrElse(
          linkTarget
            .map(_.id.split("#").head)
            .getOrElse(throw new Exception(s"Cannot produce include reference without linked element at elem $id")))
    else
      throw new Exception(s"Cannot produce include reference without linked element at elem $id")
  }

  def iriToValue(iri: String) = ValueType(iri)

  def loadAnnotationsFromParsedFields(propertyId: String) = {
    fields.fields().find(_.field.value.iri() == propertyId) match {
      case Some(loadedField) =>
        findPropertyMappingByTermPropertyId(propertyId) match {
          case Some(propertyMapping) =>
            propertyAnnotations.update(propertyMapping.id, loadedField.value.annotations)
          case _ =>
          // ignore
        }
      case _ => //
    }
  }

  def dynamicFields: List[Field] = {
    /*
    val mapKeyFields = mapKeyProperties.keys map { propertyId =>
      loadAnnotationsFromParsedFields(propertyId)
      instanceDefinedBy.get.propertiesMapping().find(_.id == propertyId) match {
        case Some(propertyMapping) => propertyMapping.toField
        case _                     => throw new Exception(s"Cannot find properties mapping for property: $propertyId")
      }
    }
     */

    val defaultFields = instanceDefinedBy match {
      case Some(nodeMapping: NodeMapping) =>
        (mapKeyProperties.keys ++ literalProperties.keys ++ linkProperties.keys ++ objectProperties.keys ++ objectCollectionProperties.keys ++ pluginProperties.keys ++ pluginCollectionProperties.keys).flatMap {
          propertyId =>
            loadAnnotationsFromParsedFields(propertyId)
            nodeMapping.propertiesMapping().find(_.id == propertyId) match {
              case Some(propertyMapping) => propertyMapping.toField
              case _                     => throw new Exception(s"Cannot find properties mapping for property: $propertyId")
            }
        }.toList
      case _                        => // this is an plugin wrapper
        List()

    }
    defaultFields ++ fields
      .fields()
      .filter(f => f.field != LinkableElementModel.Target && f.field != DomainElementModel.CustomDomainProperties)
      .map { entry =>
        loadAnnotationsFromParsedFields(entry.field.value.iri())
        entry.field
      }
  }

  def findPropertyByTermPropertyId(termPropertyId: String): String =
    definedBy match {
      case nodeMapping: NodeMapping =>
        nodeMapping.propertiesMapping()
          .find(_.nodePropertyMapping().value() == termPropertyId)
          .map(_.id)
          .getOrElse(termPropertyId)
      case _                         =>
        termPropertyId
    }

  def findPropertyMappingByTermPropertyId(termPropertyId: String): Option[PropertyMapping] = {
    definedBy match {
      case nodeMapping: NodeMapping =>
        nodeMapping.propertiesMapping().find(_.nodePropertyMapping().value() == termPropertyId)
      case _                        =>
        None
    }
  }

  override def valueForField(f: Field): Option[Value] = {
    val termPropertyId = f.value.iri()

    val propertyId  = findPropertyByTermPropertyId(termPropertyId)
    val annotations = propertyAnnotations.getOrElse(propertyId, Annotations())

    // Warning, mapKey has the term property id, no the property mapping id because
    // there's no real propertyMapping for it
    mapKeyProperties.get(propertyId) map { stringValue =>
      AmfScalar(stringValue, annotations)
    } orElse objectProperties.get(propertyId) map { dialectDomainElement =>
      dialectDomainElement
    } orElse {
      objectCollectionProperties.get(propertyId) map { seqElements =>
        AmfArray(seqElements, annotations)
      }
    } orElse {
      literalProperties.get(propertyId) map {
        case vs: Seq[_] =>
          val scalars = vs.map { s =>
            AmfScalar(s)
          }
          AmfArray(scalars, annotations)
        case other =>
          AmfScalar(other, annotations)
      }
    } orElse {
      linkProperties.get(propertyId) map {
        case vs: Seq[_] =>
          val scalars = vs.map { s =>
            AmfScalar(s)
          }
          AmfArray(scalars, annotations)
        case other =>
          AmfScalar(other, annotations)
      }

    } orElse pluginProperties.get(propertyId) map { domainElement =>
      domainElement
    } orElse {
      pluginCollectionProperties.get(propertyId) map { domainElements =>
        AmfArray(domainElements, annotations)
      }
    } map { amfElement =>
      Value(amfElement, amfElement.annotations)
    } orElse {
      fields.fields().find(_.field == f).map(_.value)
    }
  }

  protected def propertyMappingForField(field: Field): Option[PropertyMapping] = {
    val iri = field.value.iri()
    definedBy match {
      case nodeMapping: NodeMapping => nodeMapping.propertiesMapping().find(_.nodePropertyMapping().value() == iri)
      case _                        => None
    }
  }

  def removeField(patchField: Field) = {
    propertyMappingForField(patchField) match {
      case Some(property) =>
        val id = property.id
        mapKeyProperties.remove(id)
        objectCollectionProperties.remove(id)
        objectProperties.remove(id)
        literalProperties.remove(id)
        linkProperties.remove(id)
        propertyAnnotations.remove(id)
        fields.remove(id)
      case _ => // ignore
    }
  }

  def containsProperty(property: PropertyMapping): Boolean = {
    mapKeyProperties.contains(property.id) ||
    objectCollectionProperties.contains(property.id) ||
    literalProperties.contains(property.id) ||
    linkProperties.contains(property.id)
  }

  def setObjectField(property: PropertyMapping, value: DomainElement, node: YNode): DialectDomainElement = {
    value match {
      case dialectElement: DialectDomainElement =>
        objectProperties.put(property.id, dialectElement)
      case domainElemet                         =>
        pluginProperties.put(property.id, domainElemet)
    }

    propertyAnnotations.put(property.id, Annotations(node))

    value match {
      case linkable: Linkable =>
        if (linkable.isUnresolved) {
          linkable.toFutureRef {
            case resolvedDialectDomainElement: DialectDomainElement =>
              val dialectElement = value.asInstanceOf[DialectDomainElement]
              objectProperties.put(
                property.id,
                resolveUnreferencedLink(dialectElement.refName,
                  dialectElement.annotations,
                  resolvedDialectDomainElement,
                  dialectElement.supportsRecursion.option().getOrElse(false))
                  .withId(value.id)
              )
            case resolvedDomainElement: DomainElement =>
              // TODO: Transform this into a proper name
              // TODO: Normalize supports recursion
              value.fields.fields().find(_.field.value.iri() == (Namespace.Schema + "name").iri()) match {
                case Some(entry) if entry.value.isInstanceOf[AmfScalar] =>
                  val name = entry.value.toString
                  pluginProperties.put(
                    property.id,
                    resolveUnreferencedLink(name,
                      value.annotations,
                      resolvedDomainElement,
                      true)
                      .withId(value.id)
                  )
                case _         => // ignore
              }
            case resolved =>
              throw new Exception(s"Cannot resolve reference with not dialect domain element value ${resolved.id}")
          }
        }
      case _                  => // ignore
    }
    this
  }

  def setObjectField(property: PropertyMapping, value: Seq[DomainElement], node: YNode): DialectDomainElement = {
    // set values
    value match {
      case dialectElements: Seq[DialectDomainElement] =>
        val dialectElements = value.asInstanceOf[Seq[DialectDomainElement]]
        objectCollectionProperties.put(property.id, dialectElements)
      case _                                          =>
        pluginCollectionProperties.put(property.id, value)
    }

    // annotations
    propertyAnnotations.put(property.id, Annotations(node))

    // resolve links
    propertyAnnotations.put(property.id, Annotations(node))
    value.foreach {
      case linkable: Linkable if linkable.isUnresolved =>
        linkable.toFutureRef((resolved) => {
          objectCollectionProperties.get(property.id) map { oldValues =>
            oldValues map { oldValue =>
              if (oldValue.id == resolved.id) {
                resolved
              } else {
                oldValue
              }
            }
          } foreach {
            case updatedValues: Seq[DialectDomainElement] =>
              objectCollectionProperties.put(property.id, updatedValues)
            case updatedValues: Seq[DomainElement] =>
              pluginCollectionProperties.put(property.id, updatedValues)
            case _ => // ignore
          }
        })
      case _ => // ignore
    }

    this
  }

  def patchLiteralField(field: Field, value: Any): DialectDomainElement = {
    propertyMappingForField(field) match {
      case Some(property) =>
        val id = property.id
        if (mapKeyProperties.contains(id)) {
          mapKeyProperties.put(id, value)
        } else if (linkProperties.contains(id)) {
          linkProperties.put(id, value.toString)
        } else {
          literalProperties.put(id, value)
        }
      case _ => // ignore
    }

    this
  }

  def patchObjectField(field: Field, value: DialectDomainElement): DialectDomainElement = {
    propertyMappingForField(field) match {
      case Some(property) =>
        val id = property.id
        if (mapKeyProperties.contains(id)) {
          mapKeyProperties.put(id, value)
        } else {
          objectProperties.put(id, value)
        }
      case _ => // ignore
    }

    this
  }

  def patchObjectField(field: Field, value: Seq[DialectDomainElement]): DialectDomainElement = {
    propertyMappingForField(field) match {
      case Some(property) =>
        val id = property.id
        objectCollectionProperties.put(id, value)
      case _ => // ignore
    }

    this
  }

  def setLinkField(property: PropertyMapping, value: String, node: YNode): DialectDomainElement = {
    linkProperties.put(property.id, value)
    propertyAnnotations.put(property.id, Annotations(node))
    this
  }

  def setLinkField(property: PropertyMapping, value: Seq[_], node: YNode): DialectDomainElement = {
    linkProperties.put(property.id, value)
    propertyAnnotations.put(property.id, Annotations(node))
    this
  }

  def setLiteralField(property: PropertyMapping, value: Int, node: YNode): DialectDomainElement = {
    literalProperties.put(property.id, value)
    propertyAnnotations.put(property.id, Annotations(node))
    this
  }

  def setLiteralField(property: PropertyMapping, value: Float, node: YNode): DialectDomainElement = {
    literalProperties.put(property.id, value)
    propertyAnnotations.put(property.id, Annotations(node))
    this
  }

  def setLiteralField(property: PropertyMapping, value: Double, node: YNode): DialectDomainElement = {
    literalProperties.put(property.id, value)
    propertyAnnotations.put(property.id, Annotations(node))
    this
  }

  def setLiteralField(property: PropertyMapping, value: Boolean, node: YNode): DialectDomainElement = {
    literalProperties.put(property.id, value)
    propertyAnnotations.put(property.id, Annotations(node))
    this
  }

  def setLiteralField(property: PropertyMapping, value: Seq[_], node: YNode): DialectDomainElement = {
    literalProperties.put(property.id, value)
    propertyAnnotations.put(property.id, Annotations(node))
    this
  }

  def setLiteralField(property: PropertyMapping, value: String, node: YNode): DialectDomainElement = {
    literalProperties.put(property.id, value)
    propertyAnnotations.put(property.id, Annotations(node))
    this
  }

  def setLiteralField(property: PropertyMapping, value: SimpleDateTime, node: YNode): DialectDomainElement = {
    literalProperties.put(property.id, value)
    propertyAnnotations.put(property.id, Annotations(node))
    this
  }

  def setMapKeyField(propertyId: String, value: String, node: YNode): DialectDomainElement = {
    findPropertyMappingByTermPropertyId(propertyId) match {
      case Some(nodeMapping) =>
        literalProperties.put(nodeMapping.id, value)
        mapKeyProperties.put(nodeMapping.id, value)
      case _ =>
        throw new UnknownMapKeyProperty(propertyId)
    }
    this
  }

  override def meta: Obj =
  /*
    if (instanceTypes.isEmpty) {
      DialectDomainElementModel()
    } else {
  */
    new DialectDomainElementModel(instanceTypes.distinct, dynamicFields, Some(definedBy))
   // }

  override def adopted(newId: String): DialectDomainElement.this.type =
    if (Option(this.id).isEmpty) simpleAdoption(newId) else this

  override def linkCopy(): Linkable =
    DialectDomainElement().withId(id).withDefinedBy(definedBy).withInstanceTypes(instanceTypes)

  override def resolveUnreferencedLink[T](label: String,
                                          annotations: Annotations,
                                          unresolved: T,
                                          supportsRecursion: Boolean): T = {
    val unresolvedNodeMapping = unresolved.asInstanceOf[DialectDomainElement]
    val linked: T             = unresolvedNodeMapping.link(label, annotations)
    if (supportsRecursion && linked.isInstanceOf[Linkable])
      linked.asInstanceOf[Linkable].withSupportsRecursion(supportsRecursion)
    linked.asInstanceOf[DialectDomainElement].asInstanceOf[T]
  }

  /** Value , path + field value that is used to compose the id when the object its adopted */
  override def componentId: String = ""

  /** apply method for create a new instance with fields and annotations. Aux method for copy */
  override protected def classConstructor: (Fields, Annotations) => Linkable with DomainElement =
    DialectDomainElement.apply
}

object DialectDomainElement {
  def apply(): DialectDomainElement = apply(Annotations())

  def apply(ast: YMap): DialectDomainElement = apply(Annotations(ast))

  def apply(annotations: Annotations): DialectDomainElement = DialectDomainElement(Fields(), annotations)
}
