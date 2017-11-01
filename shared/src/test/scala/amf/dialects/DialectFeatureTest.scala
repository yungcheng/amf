package amf.dialects

import amf.client.GenerationOptions
import amf.common.Tests.checkDiff
import amf.compiler.AMFCompiler
import amf.dumper.AMFDumper
import amf.remote.Syntax.Yaml
import amf.remote.Syntax.Json
import amf.remote.{Raml, RamlYamlHint, Amf}
import amf.unsafe.PlatformSecrets
import amf.validation.Validation
import amf.validation.emitters.ValidationReportJSONLDEmitter
import org.scalatest.AsyncFunSuite

import scala.concurrent.ExecutionContext

case class ExpectedReport(conforms: Boolean, numErrors: Integer, profile: String)

class DialectFeatureTest extends AsyncFunSuite with PlatformSecrets {

  override implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

  val basePath         = "file://shared/src/test/resources/vocabularies/"
  val vocabulariesPath = "file://shared/src/test/resources/vocabularies/"
  val examplesPath     = "file://shared/src/test/resources/validations/"

  test("Default values") {
    val validation = platform.dialectsRegistry.registerDialect(basePath + "validation_dialect_defaults.raml")
    val expected =
      platform.resolve(basePath + "validation_profile_with_default_example.json", None).map(_.stream.toString)
    val actual = validation
      .flatMap(
        unit =>
          AMFCompiler(basePath + "validation_profile_example.raml",
                      platform,
                      RamlYamlHint,
                      None,
                      None,
                      platform.dialectsRegistry)
            .build())
    actual
      .flatMap({ unit =>
        AMFDumper(unit, Amf, Json, GenerationOptions()).dumpToString

      })
      .map(v => {
        // platform.write(basePath + "validation_profile_with_default_example.json",v);
        v
      })
      .zip(expected)
      .map(checkDiff)
  }

  test("Uses (in dialect definition)") {
    val validation = platform.dialectsRegistry.registerDialect(basePath + "validation_dialect_uses.raml")
    val expected =
      platform.resolve(basePath + "validation_profile_with_uses_example.json", None).map(_.stream.toString)
    val actual = validation
      .flatMap(
        unit =>
          AMFCompiler(basePath + "validation_profile_example.raml",
                      platform,
                      RamlYamlHint,
                      None,
                      None,
                      platform.dialectsRegistry)
            .build())
    actual
      .flatMap({ unit =>
        AMFDumper(unit, Amf, Json, GenerationOptions()).dumpToString

      })
      .map(v => {
        platform.write(basePath + "validation_profile_with_uses_example.json", v);
        v
      })
      .zip(expected)
      .map(checkDiff)
  }

//  test("Uses (in dialect)") {
//    val validation = platform.dialectsRegistry.registerDialect(basePath + "validation_dialect_unions.raml")
//    val expected =
//      platform.resolve(basePath + "validation_profile_example_uses_gold.raml", None).map(_.stream.toString)
//    val actual = validation
//      .flatMap(unit => {
//        val dl = new DialectRegistry();
//        dl.add(unit)
//        AMFCompiler(basePath + "validation_profile_example_uses.raml", platform, RamlYamlHint, None, None, dl)
//          .build()
//      })
//    actual
//      .flatMap({ unit =>
//        AMFDumper(unit, Raml, Yaml, GenerationOptions()).dumpToString
//
//      })
//      .map(v => {
//        // platform.write(basePath + "validation_profile_example_uses_gold.raml",v);
//        v
//      })
//      .zip(expected)
//      .map(checkDiff)
//  }

  test("Uses (in dialect (real library))") {
    val validation = platform.dialectsRegistry.registerDialect(basePath + "validation_dialect_unions.raml")
    val expected =
      platform.resolve(basePath + "validation_profile_example_uses_gold2.raml", None).map(_.stream.toString)
    val actual = validation
      .flatMap(unit => {
        val dl = new DialectRegistry()
        dl.add(unit)
        AMFCompiler(basePath + "validation_profile_example_uses2.raml", platform, RamlYamlHint, None, None, dl)
          .build()
      })
    actual
      .flatMap({ unit =>
        AMFDumper(unit, Raml, Yaml, GenerationOptions()).dumpToString

      })
      .map(v => {
        // platform.write(basePath + "validation_profile_example_uses_gold.raml",v);
        v
      })
      .zip(expected)
      .map(checkDiff)
  }

  test("Fragments ") {
    val validation = platform.dialectsRegistry.registerDialect(basePath + "validation_dialect_with_fragments.raml")
    val expected   = platform.resolve(basePath + "validationFragment.raml", None).map(_.stream.toString)
    val actual = validation
      .flatMap(unit => {
        val dl = new DialectRegistry();
        dl.add(unit)
        AMFCompiler(basePath + "validationFragment.raml", platform, RamlYamlHint, None, None, dl)
          .build()
      })
    actual
      .flatMap({ unit =>
        AMFDumper(unit, Raml, Yaml, GenerationOptions()).dumpToString

      })
      .map(v => {
        // platform.write(basePath + "validation_profile_example_uses_gold.raml",v);
        v
      })
      .zip(expected)
      .map(checkDiff)
  }

  test("Fragments in Dialects ") {
    val validation = platform.dialectsRegistry.registerDialect(basePath + "validation_dialect_using_fragments.raml")
    val expected   = platform.resolve(basePath + "validationFragment_simple.raml", None).map(_.stream.toString)
    val actual = validation
      .flatMap(unit => {
        val dl = new DialectRegistry();
        dl.add(unit)
        AMFCompiler(basePath + "validationFragment_simple.raml", platform, RamlYamlHint, None, None, dl)
          .build()
      })
    actual
      .flatMap({ unit =>
        AMFDumper(unit, Raml, Yaml, GenerationOptions()).dumpToString

      })
      .map(v => {
        // platform.write(basePath + "validation_profile_example_uses_gold.raml",v);
        v
      })
      .zip(expected)
      .map(checkDiff)
  }
  test("Fragments in Dialects + inplace") {
    val validation = platform.dialectsRegistry.registerDialect(basePath + "validation_dialect_using_fragments2.raml")
    val expected   = platform.resolve(basePath + "validationFragment_simple.raml", None).map(_.stream.toString)
    val actual = validation
      .flatMap(unit => {
        val dl = new DialectRegistry();
        dl.add(unit)
        AMFCompiler(basePath + "validationFragment_simple.raml", platform, RamlYamlHint, None, None, dl)
          .build()
      })
    actual
      .flatMap({ unit =>
        AMFDumper(unit, Raml, Yaml, GenerationOptions()).dumpToString

      })
      .map(v => {
        // platform.write(basePath + "validation_profile_example_uses_gold.raml",v);
        v
      })
      .zip(expected)
      .map(checkDiff)
  }
  test("Using library in dialect definition") {
    val validation = platform.dialectsRegistry.registerDialect(basePath + "validation_dialect_uses(dialect_lib).raml")
    val expected   = platform.resolve(basePath + "validationFragment.raml", None).map(_.stream.toString)
    val actual = validation
      .flatMap(unit => {
        val dl = new DialectRegistry();
        dl.add(unit)
        AMFCompiler(basePath + "validationFragment.raml", platform, RamlYamlHint, None, None, dl)
          .build()
      })
    actual
      .flatMap({ unit =>
        AMFDumper(unit, Raml, Yaml, GenerationOptions()).dumpToString

      })
      .map(v => {
        // platform.write(basePath + "validation_profile_example_uses_gold.raml",v);
        v
      })
      .zip(expected)
      .map(checkDiff)
  }
}
