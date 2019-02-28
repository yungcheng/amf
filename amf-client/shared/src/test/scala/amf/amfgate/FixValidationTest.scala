package amf.amfgate
import amf.Raml10Profile
import amf.core.parser.UnhandledErrorHandler
import amf.core.remote.RamlYamlHint
import amf.core.services.RuntimeValidator
import amf.core.validation.SeverityLevels
import amf.facades.{AMFCompiler, Validation}
import amf.io.FileAssertionTest
import amf.plugins.document.webapi.resolution.pipelines.amfgate.Raml10FixerResolutionPipeline
import org.scalatest.{Assertion, AsyncFunSuite, Matchers}
import scala.concurrent.Future

class FixValidationTest extends AsyncFunSuite with FileAssertionTest with Matchers {

  private val basePath = "file://amf-client/shared/src/test/resources/amfgate/"

  override implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global

  def run(api: String): Future[Assertion] = {
    for {
      v        <- Validation(platform)
      original <- AMFCompiler(basePath + api, platform, RamlYamlHint, v).build()
      errors   <- RuntimeValidator(original, Raml10Profile)
      fixedUnit <- new Raml10FixerResolutionPipeline(errors.results)(UnhandledErrorHandler)
        .resolve(original) // todo: RuntimeResolve.fix ???
      newReport <- RuntimeValidator(fixedUnit, Raml10Profile)
    } yield newReport.conforms shouldBe true
  }

//
  test("Expecting bool") {
    run("expecting-bool-str-provided.raml")
  }
}
