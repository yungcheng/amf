package amf.plugins.document.webapi.resolution.pipelines.amfgate.steps

import amf.core.AMFCompiler
import amf.core.model.document.BaseUnit
import amf.core.parser.ErrorHandler
import amf.core.remote.{Cache, Raml10}
import amf.core.unsafe.PlatformSecrets
import amf.core.validation.AMFValidationResult
import amf.internal.environment.Environment
import amf.internal.resource.StringResourceLoader
import amf.plugins.document.webapi.resolution.pipelines.amfgate.FixerStage
import amf.plugins.features.validation.ParserSideValidations

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
class SyamlSyntaxFixerStep(results: Seq[AMFValidationResult])(override implicit val errorHandler: ErrorHandler)
    extends FixerStage(results)
    with PlatformSecrets {

  private val toFix: Seq[AMFValidationResult] =
    results
      .filter(_.validationId == ParserSideValidations.SyamlError.id)

  private val lines: ListBuffer[String] = ListBuffer()

  override def resolve(model: BaseUnit): Future[BaseUnit] = {
    if (toFix.nonEmpty) {
      val raw = model.raw.getOrElse(throw new Exception("Raw should be present"))
      lines ++= raw.split("\n")
      toFix.foreach(fixResult)
      build(model.id)
    } else Future(model)
  }

  private def build(id: String): Future[BaseUnit] =
    new AMFCompiler(id,
                    platform,
                    None,
                    None,
                    Some(Raml10.toString),
                    env = Environment().add(StringResourceLoader(id, lines.mkString("\n"))),
                    cache = Cache()).build()

  private def fixResult(result: AMFValidationResult): Unit = {
    result.message match {
      case "Expecting !!bool and !!str provided" => fixScalar(result.position.get.range, Bool)
      case "Expecting !!str and !!int provided"  => fixScalar(result.position.get.range, Int)
      case _                                     => // ignore
    }
  }

  private def fixScalar(range: amf.core.parser.Range, target: Target): Unit = {
    val line = lines(range.start.line - 1)

    // todo: in future we need to check if it's key or value to replace
    val text = line.split(":").last.trim
    val newText: String = target match {
      case Bool => fixBoolScalar(text)
      case Int  => fixStrScalar(text)
    }

    lines(range.start.line - 1) = lines(range.start.line - 1).substring(0, range.start.column) + newText
  }

  private def fixBoolScalar(text: String): String = {
    if (text.equals("\"false\"") || text.equals("0")) "false"
    else "true"
  }

  private def fixStrScalar(text: String): String = "\"" + text + "\""

}
sealed trait Target

object String extends Target
object Int    extends Target
object Bool   extends Target
