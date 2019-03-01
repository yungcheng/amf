package amf.plugins.document.webapi.resolution.pipelines.amfgate.steps

import amf.core.model.document.BaseUnit
import amf.core.parser.ErrorHandler
import amf.core.unsafe.PlatformSecrets
import amf.core.validation.AMFValidationResult
import amf.plugins.document.webapi.resolution.pipelines.amfgate.FixerStage
import amf.plugins.domain.shapes.models.AnyShape
import amf.plugins.domain.webapi.models.Payload

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MandatoryMediaTypeFixerStep(results: Seq[AMFValidationResult])(override implicit val errorHandler: ErrorHandler)
    extends FixerStage(results)
    with PlatformSecrets {

  private val toFix: Seq[AMFValidationResult] =
    results.filter(_.message == "Payload media type is mandatory")

  override def resolve(model: BaseUnit): Future[BaseUnit] = Future {
    toFix.foreach(fixMandatory(model, _))
    model
  }

  private def fixMandatory(model: BaseUnit, result: AMFValidationResult): Unit =
    model.findById(result.targetNode) match {
      case Some(payload: Payload) =>
        payload.schema match {
          case s: AnyShape =>
            if (s.examples.exists(_.raw.value().startsWith("<"))) payload.withMediaType("application/xml")
            else payload.withMediaType("application/json")
          case _ =>
        }
      case _ =>
    }
}
