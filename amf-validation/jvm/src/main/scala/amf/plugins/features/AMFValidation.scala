package amf.plugins.features

object AMFValidation {
  def register() = {
    amf.Core.registerPlugin(AMFValidatorPlugin)
  }
}
