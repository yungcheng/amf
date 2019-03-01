package amf.amfgate
import amf.Raml10Profile
import amf.core.emitter.RenderOptions
import amf.core.model.document.BaseUnit
import amf.core.parser.UnhandledErrorHandler
import amf.core.remote.{Raml10, RamlYamlHint}
import amf.core.services.RuntimeValidator
import amf.core.validation.AMFValidationReport
import amf.emit.AMFRenderer
import amf.facades.{AMFCompiler, Validation}
import amf.io.FileAssertionTest
import amf.plugins.document.webapi.resolution.pipelines.amfgate.Raml10FixerResolutionPipeline
import org.scalatest.{Assertion, AsyncFreeSpec, AsyncFunSuite, Matchers}

import scala.concurrent.Future

class FixValidationTest extends AsyncFreeSpec with FileAssertionTest with Matchers {

  private val basePath   = "file://amf-client/shared/src/test/resources/amfgate/"
  private val goldenPath = "amf-client/shared/src/test/resources/amfgate/goldens/"

  override implicit val executionContext = scala.concurrent.ExecutionContext.Implicits.global

  case class UnitAndReport(bu: BaseUnit, report: AMFValidationReport)

  private def parseAndValidate(api: String, path: String = basePath): Future[UnitAndReport] = {
    for {
      v        <- Validation(platform)
      original <- AMFCompiler(path + api, platform, RamlYamlHint, v).build()
      errors   <- RuntimeValidator(original, Raml10Profile)
    } yield UnitAndReport(original, errors)
  }

  def fixValidation(api: String): Future[BaseUnit] = {
    for {
      unitAndReport <- parseAndValidate(api)
      fixedUnit <- new Raml10FixerResolutionPipeline(unitAndReport.report.results)(UnhandledErrorHandler)
        .resolve(unitAndReport.bu) // todo: RuntimeResolve.fix ???
    } yield fixedUnit
  }

  def assertValidation(api: String): Future[Assertion] =
    fixValidation(api).flatMap(RuntimeValidator(_, Raml10Profile)).map(_.conforms shouldBe true)

  def emitFixed(api: String): Future[Assertion] = {
    for {
      bu     <- fixValidation(api)
      fixedS <- AMFRenderer(bu, Raml10, RenderOptions()).renderToString
      diff <- {
        writeTemporaryFile(goldenPath + api)(fixedS)
          .flatMap(assertDifferences(_, goldenPath + api))
      }
    } yield diff
  }

  def validateEmited(api: String): Future[Assertion] = {
    parseAndValidate(api, "file://" + goldenPath).map(_.report.conforms shouldBe true)
  }

  case class Fixture(name: String, file: String)
  val fixture =
    Seq(
      Fixture("Expecting bool", "expecting-bool-str-provided.raml"),
      Fixture("Expecting str int provided", "expecting-str-int-provided.raml"),
      Fixture("Invalid Decimal Point", "invalid-decimal-point.raml")
    )

  fixture.foreach { c =>
    s"Test ${c.name}" - {
      "Test fix validation" in {
        assertValidation(c.file)
      }

      "Cycle fixed" - {
        s"Emit fixed ${c.name}" in {
          emitFixed(c.file)
        }
        s"Validate emited fixed ${c.name}" in {
          validateEmited(c.file)
        }
      }
    }
  }

}
