package amf.plugins.document.webapi.resolution.pipelines.amfgate
import amf.core.model.document.BaseUnit
import amf.core.parser.ErrorHandler
import amf.core.validation.AMFValidationResult

import scala.concurrent.Future

abstract class FixerStage(results: Seq[AMFValidationResult])(implicit val errorHandler: ErrorHandler) {
  def resolve(model: BaseUnit): Future[BaseUnit]
}
