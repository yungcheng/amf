package amf.amfgate
import amf.Raml10Profile
import amf.core.emitter.RenderOptions
import amf.core.parser.UnhandledErrorHandler
import amf.core.remote.{Raml10, RamlYamlHint}
import amf.core.services.RuntimeValidator
import amf.emit.AMFRenderer
import amf.facades.{AMFCompiler, Validation}
import amf.io.FileAssertionTest
import amf.plugins.document.webapi.resolution.pipelines.amfgate.Raml10FixerResolutionPipeline
import org.scalatest._

import scala.concurrent.{ExecutionContext, Future}

class FixCycleTest extends AsyncFunSuite with FileAssertionTest with Matchers {

  private val basePath   = "file://amf-client/shared/src/test/resources/amfgate/"
  private val goldenPath = "amf-client/shared/src/test/resources/amfgate/goldens/"

  override implicit val executionContext = ExecutionContext.Implicits.global
  def run(api: String): Future[Assertion] = {

    for {
      v        <- Validation(platform)
      original <- AMFCompiler(basePath + api, platform, RamlYamlHint, v).build()
      errors   <- RuntimeValidator(original, Raml10Profile)
      fixedUnit <- new Raml10FixerResolutionPipeline(errors.results)(UnhandledErrorHandler)
        .resolve(original) // todo: RuntimeResolve.fix ???
      fixedS <- AMFRenderer(fixedUnit, Raml10, RenderOptions()).renderToString
      diff <- {
        writeTemporaryFile(goldenPath + api)(fixedS)
          .flatMap(assertDifferences(_, goldenPath + api))
      }
    } yield diff
  }

  test("Test expecting-bool-str-provided.raml") {
    run("expecting-bool-str-provided.raml")
  }

  test("Test expecting-str-int-provided.raml") {
    run("expecting-str-int-provided.raml")
  }

}
