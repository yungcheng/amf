package amf.plugins.document.webapi.validation.remote

import amf.core.emitter.RenderOptions
import amf.core.model.document.PayloadFragment
import amf.core.model.domain.{DataNode, ObjectNode, ScalarNode, Shape}
import amf.core.validation.core.ValidationProfile
import amf.core.validation.{AMFValidationReport, ValidationCandidate}
import amf.plugins.document.webapi.OAS20Plugin
import amf.plugins.document.webapi.metamodel.FragmentsTypesModels.DataTypeFragmentModel
import amf.plugins.document.webapi.model.DataTypeFragment
import amf.plugins.domain.shapes.models.{AnyShape, NodeShape}
import amf.plugins.syntax.SYamlSyntaxPlugin

import scala.concurrent.Future

class ExampleUnknownException(e: Throwable) extends RuntimeException(e)
class UnknownDiscriminator() extends RuntimeException

trait PlatformJsonSchemaValidator {

  def validate(validationCandidates: Seq[ValidationCandidate], profile: ValidationProfile): Future[AMFValidationReport]

  def findPolymorphicShape(anyShape: AnyShape, payload: DataNode): Shape = {
    val closure: Seq[Shape] = anyShape.effectiveStructuralShapes
    findPolymorphicEffectiveShape(closure, payload) match {
      case Some(shape: NodeShape) => shape
      case _ => // TODO: structurally can be a valid type, should we fail? By default RAML expects a failure
        /*
        val polymorphicUnion = UnionShape().withId(payload.id + "_polymorphic")
        polymorphicUnion.setArrayWithoutId(UnionShapeModel.AnyOf, closure)
        polymorphicUnion
        */
        throw new UnknownDiscriminator()
    }
  }

  def findPolymorphicEffectiveShape(polymorphicUnion: Seq[Shape], currentDataNode: DataNode): Option[Shape] = {
    polymorphicUnion.filter(_.isInstanceOf[NodeShape]).find {
      case nodeShape: NodeShape =>
        nodeShape.discriminator.option() match {
          case Some(discriminatorProp) =>
            val discriminatorValue = nodeShape.discriminatorValue.option().getOrElse(nodeShape.name.value())
            currentDataNode match {
              case obj: ObjectNode =>
                obj.properties.get(discriminatorProp) match {
                  case Some(v: ScalarNode) => {
                    v.value == discriminatorValue
                  }
                  case _ => false
                }
              case _ => false
            }
          case None => false
        }
    }
  }

  def computeJsonSchemaCandidates(validationCandidates: Seq[ValidationCandidate]): Seq[(PayloadFragment, String, String, Option[Throwable])] = validationCandidates.map { vc =>
    try {
      val fragmentShape = vc.shape match {
        case anyShape: AnyShape if anyShape.supportsInheritance => findPolymorphicShape(anyShape, vc.payload.encodes)
        case _ => vc.shape
      }
      val dataType = DataTypeFragment()
      dataType.fields.setWithoutId(DataTypeFragmentModel.Encodes, fragmentShape) // careful, we don't want to modify the ID

      OAS20Plugin.unparse(dataType, RenderOptions()) match {
        case Some(doc) =>
          SYamlSyntaxPlugin.unparse("application/json", doc) match {
            case Some(jsonSchema) =>
              Some((vc.payload, jsonSchema.replace("x-amf-union", "anyOf"), vc.shape.id, None))
            case _ =>
              None
          }
        case _ => None
      }
    } catch {
      case e: UnknownDiscriminator => Some((vc.payload, "", vc.shape.id, Some(e)))
    }
  } collect { case Some(s) => s }
}
