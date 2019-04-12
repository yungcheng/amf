package amf.plugins.features

import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
object AMFValidation {
  def register() = {
    amf.Core.registerPlugin(AMFValidatorPlugin)
  }
}
