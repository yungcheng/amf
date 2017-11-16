package amf.resolution

import amf.remote._

class SecurityResolutionTest extends ResolutionTest {

  override val basePath = "shared/src/test/resources/resolution/security/"

  test("Security resolution raml to AMF") {
    cycle("security.raml", "security.raml.jsonld", RamlYamlHint, Amf)
  }

  test("Security resolution oas to AMF") {
    cycle("security.json", "security.json.jsonld", OasJsonHint, Amf)
  }
}
