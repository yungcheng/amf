package amf.plugins.document.webapi.resolution.pipelines.amfgate.steps
import amf.core.model.document.BaseUnit
import amf.core.parser.ErrorHandler
import amf.core.utils.IdCounter
import amf.core.validation.AMFValidationResult
import amf.plugins.document.webapi.resolution.pipelines.amfgate.FixerStage
import amf.plugins.domain.webapi.models.Parameter
import amf.plugins.features.validation.ParserSideValidations

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ParameterNameCreation(results: Seq[AMFValidationResult])(override implicit val errorHandler: ErrorHandler)
    extends FixerStage(results) {

  private val toFix: Seq[AMFValidationResult] =
    results
      .filter(_.validationId == "http://a.ml/vocabularies/amf/parser#parameter-name-required")

  override def resolve(model: BaseUnit): Future[BaseUnit] = {
    val counter = new IdCounter()

    toFix
      .flatMap(r => model.findById(r.targetNode))
      .foreach {
        case p: Parameter if p.name.option().isEmpty || p.name.option().contains("default") =>
          val name = counter.genId("gen-name-")
          p.withName(name)
          p.withParameterName(name)
        case other =>
          other // ignore
      }
    Future(model)
  }
}
