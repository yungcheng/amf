package amf.plugins.document.webapi.resolution.pipelines.amfgate

import amf.core.model.document.BaseUnit
import amf.core.parser.ErrorHandler
import amf.core.validation.AMFValidationResult
import amf.plugins.document.webapi.resolution.pipelines.amfgate.steps.SyamlSyntaxFixerStep
import amf.{ProfileName, Raml10Profile}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Raml10FixerResolutionPipeline(results: Seq[AMFValidationResult])(implicit val eh: ErrorHandler) {
  def profileName: ProfileName = Raml10Profile
  val steps: Seq[FixerStage] = Seq(
    new SyamlSyntaxFixerStep(results)
  )

  final def resolve(model: BaseUnit): Future[BaseUnit] = steps.foldLeft(Future.successful(model)) {
    case (acc, stage) => acc.flatMap(stage.resolve)
  }

  protected def step(unit: BaseUnit, stage: FixerStage): Future[BaseUnit] = {
    val resolved = stage.resolve(unit)
    resolved
  }
}
