package amf.amfgate
import amf.Raml10Profile
import amf.core.emitter.RenderOptions
import amf.core.model.document.BaseUnit
import amf.core.parser.UnhandledErrorHandler
import amf.core.remote.{Raml10, RamlYamlHint}
import amf.core.services.RuntimeValidator
import amf.core.validation.SeverityLevels
import amf.emit.AMFRenderer
import amf.facades.{AMFCompiler, Validation}
import amf.io.FileAssertionTest
import amf.plugins.document.webapi.resolution.pipelines.amfgate.Raml10FixerResolutionPipeline
import org.scalatest._

import scala.concurrent.Future

class FixCycleTest extends AsyncFunSuite with FileAssertionTest with Matchers {

  private val basePath   = "file://amf-client/shared/src/test/resources/amfgate/"
  private val goldenPath = "file://amf-client/shared/src/test/resources/amfgate/goldens/"

  def run(api: String, goldenFix: String): Future[Assertion] = {

    for {
      v        <- Validation(platform)
      original <- AMFCompiler(basePath + api, platform, RamlYamlHint, v).build()
      errors   <- RuntimeValidator(original, Raml10Profile)
      fixedUnit <- Future(
        new Raml10FixerResolutionPipeline(UnhandledErrorHandler,
                                          errors.results.filter(_.level == SeverityLevels.VIOLATION))
          .resolve(original)) // todo: RuntimeResolve.fix ???
      fixedS <- AMFRenderer(fixedUnit, Raml10, RenderOptions()).renderToString
      diff <- {
        writeTemporaryFile(goldenPath + goldenFix)(fixedS)
          .flatMap(assertDifferences(_, goldenPath + goldenFix))
      }
    } yield diff
  }
}
