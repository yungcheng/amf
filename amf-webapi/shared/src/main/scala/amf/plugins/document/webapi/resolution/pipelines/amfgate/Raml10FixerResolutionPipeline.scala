package amf.plugins.document.webapi.resolution.pipelines.amfgate
import amf.ProfileName
import amf.core.parser.ErrorHandler
import amf.core.resolution.pipelines.ResolutionPipeline
import amf.core.resolution.stages.ResolutionStage
import amf.core.validation.AMFValidationResult

class Raml10FixerResolutionPipeline(override val eh: ErrorHandler, errors: Seq[AMFValidationResult])
    extends ResolutionPipeline(eh) {
  override def profileName: ProfileName    = ???
  override val steps: Seq[ResolutionStage] = Nil
}
