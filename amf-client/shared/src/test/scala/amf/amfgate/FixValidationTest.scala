package amf.amfgate

import amf.Raml10Profile
import amf.core.emitter.RenderOptions
import amf.core.model.document.BaseUnit
import amf.core.parser.UnhandledErrorHandler
import amf.core.remote._
import amf.core.services.RuntimeValidator
import amf.core.validation.AMFValidationReport
import amf.emit.AMFRenderer
import amf.facades.{AMFCompiler, Validation}
import amf.io.FileAssertionTest
import amf.plugins.document.webapi.resolution.pipelines.amfgate.{
  Oas20FixerResolutionPipeline,
  Raml10FixerResolutionPipeline
}
import org.scalatest.{Assertion, AsyncFreeSpec, Matchers}

import scala.concurrent.{ExecutionContext, Future}

class FixValidationTest extends AsyncFreeSpec with FileAssertionTest with Matchers {

  private val basePath   = "file://amf-client/shared/src/test/resources/amfgate/"
  private val goldenPath = "amf-client/shared/src/test/resources/amfgate/goldens/"

  override implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  case class UnitAndReport(bu: BaseUnit, report: AMFValidationReport)

  private def parseAndValidate(api: String, vendor: Vendor, path: String = basePath): Future[UnitAndReport] = {
    for {
      v        <- Validation(platform)
      original <- AMFCompiler(path + api, platform, if (vendor.isRaml) RamlYamlHint else OasJsonHint, v).build()

      errors <- RuntimeValidator(original, Raml10Profile)
    } yield UnitAndReport(original, errors)
  }

  def resolveFixing(unitAndReport: UnitAndReport, vendor: Vendor): Future[BaseUnit] = {
    (vendor match {
      case _: Raml =>
        new Raml10FixerResolutionPipeline(unitAndReport.report.results)(UnhandledErrorHandler)
          .resolve(unitAndReport.bu)
      case _ =>
        new Oas20FixerResolutionPipeline(unitAndReport.report.results)(UnhandledErrorHandler)
          .resolve(unitAndReport.bu)
    }).map { u =>
      u.parserRun = u.parserRun.map(_ + 1)
      u
    }
  }

  def fixValidation(api: String, vendor: Vendor): Future[BaseUnit] = {
    for {
      unitAndReport <- parseAndValidate(api, vendor)
      fixedUnit     <- resolveFixing(unitAndReport, vendor)
      // todo: RuntimeResolve.fix ???
    } yield fixedUnit
  }

  def assertValidation(api: String, vendor: Vendor): Future[Assertion] =
    fixValidation(api, vendor).flatMap(RuntimeValidator(_, Raml10Profile)).map(_.conforms shouldBe true)

  def emitFixed(api: String, vendor: Vendor): Future[Assertion] = {
    for {
      bu     <- fixValidation(api, vendor)
      fixedS <- AMFRenderer(bu, vendor, RenderOptions()).renderToString
      diff <- {
        writeTemporaryFile(goldenPath + api)(fixedS)
          .flatMap(assertDifferences(_, goldenPath + api))
      }
    } yield diff
  }

  def validateEmitted(api: String, vendor: Vendor): Future[Assertion] = {
    parseAndValidate(api, vendor, "file://" + goldenPath).map(_.report.conforms shouldBe true)
  }

  case class Fixture(name: String, file: String, vendor: Vendor = Raml10)
  private val fixture =
    Seq(
      Fixture("Expecting bool", "expecting-bool-str-provided.raml"),
      Fixture("Expecting str int provided", "expecting-str-int-provided.raml"),
      Fixture("Invalid Decimal Point", "invalid-decimal-point.raml"),
      Fixture("Mandatory payload mediaType", "payload-media-type-mandatory.raml"),
      Fixture("Invalid Example", "named-example.raml"),
      Fixture("Schema keyword is deprecated", "schema-deprecated.raml"),
      Fixture("Unnamed parameters", "name-parameter.json", Oas20)
    )

  fixture.foreach { c =>
    s"Test ${c.name}" - {
      s"Test fix validation ${c.name}" in {
        assertValidation(c.file, c.vendor)
      }

      "Cycle fixed" - {
        s"Emit fixed ${c.name}" in {
          emitFixed(c.file, c.vendor)
        }
        s"Validate emited fixed ${c.name}" in {
          validateEmitted(c.file, c.vendor)
        }
      }
    }
  }

}
