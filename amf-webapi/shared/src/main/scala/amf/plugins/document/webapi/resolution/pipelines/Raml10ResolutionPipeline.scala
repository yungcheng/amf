package amf.plugins.document.webapi.resolution.pipelines

import amf.core.errorhandling.ErrorHandler
import amf.{ProfileName, RamlProfile}

class Raml10ResolutionPipeline(override val eh: ErrorHandler) extends AmfResolutionPipeline(eh) {
  override def profileName: ProfileName = RamlProfile
  override def references               = new WebApiReferenceResolutionStage()
}
