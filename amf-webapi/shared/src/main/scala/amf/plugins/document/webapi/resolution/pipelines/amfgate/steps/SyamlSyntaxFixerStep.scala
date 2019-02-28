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

class SyamlSyntaxFixerStep(results: Seq[AMFValidationResult])(override implicit val errorHandler: ErrorHandler)
    extends FixerStage(results)
    with PlatformSecrets {

  private val toFix: Seq[AMFValidationResult] =
    results
      .filter(_.validationId == ParserSideValidations.SyamlError.id)

  private val lines: ListBuffer[String] = ListBuffer()

  override def resolve(model: BaseUnit): Future[BaseUnit] = {

    val raw = model.raw.getOrElse(throw new Exception("Raw should be present"))
    lines ++= raw.split("\n")
    results.foreach(fixResult)
    build(model.id)
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
      case "Expecting !!bool and !!str provided" | "Expecting !!bool and !!int provided" =>
        fixBoolScalar(result.position.get.range)
      case _ => // ignore
    }
  }

  private def fixBoolScalar(range: amf.core.parser.Range): Unit = {
    val line = lines(range.start.line - 1)

    val text = line.split(":").last.trim
    if (text.equals("\"false\"") || line.equals("0"))
      lines(range.start.line - 1) = lines(range.start.line - 1).substring(0, range.start.column) + "false"
    else
      lines(range.start.line - 1) = lines(range.start.line - 1).substring(0, range.start.column) + "true"
  }
}
