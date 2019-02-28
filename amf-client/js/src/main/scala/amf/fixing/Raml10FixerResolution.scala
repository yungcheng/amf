package amf.fixing

import amf.client.convert.CoreClientConverters._
import amf.client.model.document.BaseUnit
import amf.client.validate.ValidationResult
import amf.core.parser.{ErrorHandler, UnhandledErrorHandler}
import amf.plugins.document.webapi.resolution.pipelines.amfgate.Raml10FixerResolutionPipeline

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExportAll
@JSExport("amf.fixing.Raml10FixerResolution")
class Raml10FixerResolution {

  def resolve(unit: BaseUnit, vendor: String, results: ClientList[ValidationResult]): ClientFuture[BaseUnit] = {
    implicit val eh: ErrorHandler = UnhandledErrorHandler
    new Raml10FixerResolutionPipeline(results.asInternal).resolve(unit).asClient
  }
}
