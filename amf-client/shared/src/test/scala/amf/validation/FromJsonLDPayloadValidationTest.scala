package amf.validation

import amf.core.unsafe.PlatformSecrets
import amf.facades.Validation
import amf.plugins.features.validation.PlatformValidator
import amf.plugins.features.validation.emitters.{JSLibraryEmitter, ValidationJSONLDEmitter}
import amf.{AMFProfile, OASProfile, RAMLProfile}
import org.scalatest.AsyncFunSuite

import scala.concurrent.ExecutionContext

class FromJsonLDPayloadValidationTest extends AsyncFunSuite with PlatformSecrets {
  override implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

  val path = "file://amf-client/shared/src/test/resources/validations/"

  val testValidations = Map(
    "bad_domain/valid.jsonld"                 -> ExpectedReport(conforms = true, 0, OASProfile),
    "endpoint/amf.jsonld"                     -> ExpectedReport(conforms = false, 1, AMFProfile),
    "endpoint/valid.jsonld"                   -> ExpectedReport(conforms = true, 0, AMFProfile),
    "operation/amf.jsonld"                    -> ExpectedReport(conforms = false, 1, AMFProfile),
    "operation/valid.jsonld"                  -> ExpectedReport(conforms = true, 0, AMFProfile),
    "parameters/amf_properties.jsonld"        -> ExpectedReport(conforms = false, 3, AMFProfile),
    "parameters/amf_empty.jsonld"             -> ExpectedReport(conforms = false, 3, AMFProfile),
    "parameters/amf_valid.jsonld"             -> ExpectedReport(conforms = true, 0, AMFProfile),
    "shapes/enum_amf.jsonld"                  -> ExpectedReport(conforms = false, 1, OASProfile),
    "shapes/enum_valid.jsonld"                -> ExpectedReport(conforms = true, 0, OASProfile),
    "webapi/amf.jsonld"                       -> ExpectedReport(conforms = false, 1, OASProfile),
    "webapi/valid.jsonld"                     -> ExpectedReport(conforms = false, 1, OASProfile),
    "webapi/valid.jsonld"                     -> ExpectedReport(conforms = true, 0, RAMLProfile),
    "webapi/bad_protocol.jsonld"              -> ExpectedReport(conforms = false, 1, RAMLProfile),
    "types/scalars/missing_type.jsonld"       -> ExpectedReport(conforms = false, 1, RAMLProfile),
    "types/scalars/missing_type_valid.jsonld" -> ExpectedReport(conforms = true, 0, RAMLProfile),
    "types/scalars/wrong_facet.jsonld"        -> ExpectedReport(conforms = false, 2, RAMLProfile),
    "types/scalars/valid_facet.jsonld"        -> ExpectedReport(conforms = true, 0, RAMLProfile),
    //   we commentated the range of items validation
    //    "types/arrays/wrong_items.jsonld"         -> ExpectedReport(conforms = false, 1, RAMLProfile),
    //    "types/arrays/right_items.jsonld"         -> ExpectedReport(conforms = true, 0, RAMLProfile),
    "types/arrays/empty_items.jsonld" -> ExpectedReport(conforms = true, 0, RAMLProfile),
    "types/arrays/empty_items.jsonld" -> ExpectedReport(conforms = false, 1, OASProfile),
    "annotationTypes/invalid.jsonld"  -> ExpectedReport(conforms = false, 1, RAMLProfile),
    "annotationTypes/valid.jsonld"    -> ExpectedReport(conforms = true, 0, RAMLProfile)
  )

  for {
    (file, expectedReport) <- testValidations
  } yield {
    test(s"SHACL Validator $file") {
      validate(file, expectedReport)
    }
  }

  private def validate(file: String, expectedReport: ExpectedReport) = {
    platform.resolve(path + file).flatMap { data =>
      val model = data.stream.toString
      Validation(platform).flatMap { validation =>
        val effectiveValidations = validation.computeValidations(expectedReport.profile)
        val shapes               = validation.shapesGraph(effectiveValidations)
        val jsLibrary            = new JSLibraryEmitter(None).emitJS(effectiveValidations.effective.values.toSeq)

        jsLibrary match {
          case Some(code) =>
            PlatformValidator.instance.registerLibrary(ValidationJSONLDEmitter.validationLibraryUrl, code)
          case _ => // ignore
        }
        PlatformValidator.instance.report(
          model,
          "application/ld+json",
          shapes,
          "application/ld+json"
        ) flatMap { report =>
          assert(expectedReport == ExpectedReport(report.conforms, report.results.length, expectedReport.profile))
        }
      }
    }
  }

}
