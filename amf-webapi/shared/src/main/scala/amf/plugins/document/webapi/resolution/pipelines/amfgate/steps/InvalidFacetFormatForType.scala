package amf.plugins.document.webapi.resolution.pipelines.amfgate.steps
import amf.client.model.DataTypes
import amf.core.model.document.BaseUnit
import amf.core.model.domain.{AmfScalar, DomainElement, Shape}
import amf.core.parser.ErrorHandler
import amf.core.validation.AMFValidationResult
import amf.plugins.document.webapi.resolution.pipelines.amfgate.FixerStage
import amf.plugins.domain.shapes.metamodel.ScalarShapeModel
import amf.plugins.domain.shapes.models.ScalarShape
import amf.plugins.features.validation.ParserSideValidations

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class InvalidFacetFormatForType(results: Seq[AMFValidationResult])(override implicit val errorHandler: ErrorHandler)
    extends FixerStage(results) {

  private val toFix: Seq[AMFValidationResult] =
    results
      .filter(_.validationId == ParserSideValidations.InvalidDecimalPoint.id)

  override def resolve(model: BaseUnit): Future[BaseUnit] = {
    val stringToResults: Map[String, Seq[AMFValidationResult]] = toFix.groupBy(_.targetNode)
    stringToResults.keys.flatMap(k => model.findById(k)).foreach { d =>
      fixType(d)
    }
    Future(model)
  }

  private def fixType(d: DomainElement) = {
    d match {
      case s: ScalarShape if s.dataType.value() == DataTypes.Integer =>
        s.set(ScalarShapeModel.DataType,
              AmfScalar(DataTypes.Number),
              d.fields.getValue(ScalarShapeModel.DataType).annotations)
      case _ => // ignore
    }
  }
}
