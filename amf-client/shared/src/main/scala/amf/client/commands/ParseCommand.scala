package amf.client.commands

import amf.core.benchmark.ExecutionLog
import amf.core.client.{ExitCodes, ParserConfig}
import amf.core.remote.Platform

import scala.concurrent.Future
import scala.util.{Failure, Success}

class ParseCommand(override val platform: Platform) extends TranslateCommand(platform) {

  override def run(origConfig: ParserConfig): Future[Any] = {
    val config = origConfig.copy(outputFormat = Some("AMF Graph"), outputMediaType = Some("application/ld+json"))
    val res = for {
      _         <- ExecutionLog.withStage("AMF Initialisation") { AMFInit() }
      _         <- ExecutionLog.withStage("Processing dialects") { processDialects(config) }
      model     <- ExecutionLog.withStage("Parsing") { parseInput(config) }
      _         <- ExecutionLog.withStage("Validation") { checkValidation(config, model) }
      model     <- ExecutionLog.withStage("Resolution") { resolve(config, model) }
      generated <- ExecutionLog.withStage("Generation") { generateOutput(config, model) }
    } yield {
      generated
    }

    res.onComplete {

      case Failure(ex: Throwable) =>
        config.stderr.print(ex)
        config.proc.exit(ExitCodes.Exception)
      case Success(other) => other
    }

    res
  }

}

object ParseCommand {
  def apply(platform: Platform) = new ParseCommand(platform)
}
