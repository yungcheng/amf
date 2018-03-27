package amf.plugins.document.webapi.parser.spec.domain

import amf.core.annotations.{BasePathLexicalInformation, HostLexicalInformation, SynthesizedField}
import amf.core.model.domain.{AmfArray, AmfScalar}
import amf.core.utils.TemplateUri
import amf.plugins.document.webapi.contexts.{OasWebApiContext, RamlWebApiContext}
import amf.plugins.document.webapi.parser.spec.common.{AnnotationParser, RamlScalarNode, SpecParserOps}
import amf.plugins.domain.webapi.metamodel.{ServerModel, WebApiModel}
import amf.plugins.domain.webapi.models.{Parameter, Server, WebApi}
import org.yaml.model.{YMap, YMapEntry, YType}
import amf.core.parser.{Annotations, _}
import amf.plugins.document.webapi.parser.spec.{toOas, toRaml}

case class RamlServersParser(map: YMap, api: WebApi)(implicit val ctx: RamlWebApiContext) extends SpecParserOps {
  def parse(): Unit = {
    map.key("baseUri") match {
      case Some(entry) =>
        val node   = RamlScalarNode(entry.value)
        val value  = node.text().toString
        val server = api.withServer(value)

        (ServerModel.Url in server).allowingAnnotations(entry)

        if (!TemplateUri.isValid(value))
          ctx.violation(api.id, TemplateUri.invalidMsg(value), entry.value)

        map.key("(serverDescription)", ServerModel.Description in server)

        map.key(
          "baseUriParameters",
          entry => {
            entry.value.tagType match {
              case YType.Map =>
                val parameters =
                  RamlParametersParser(entry.value.as[YMap], server.withVariable)
                    .parse()
                    .map(_.withBinding("path"))
                server.set(ServerModel.Variables, AmfArray(parameters, Annotations(entry.value)), Annotations(entry))
              case YType.Null =>
              case _          => ctx.violation("Invalid node for baseUriParameters", entry.value)
            }
          }
        )

        api.set(WebApiModel.Servers,
                AmfArray(Seq(server.add(SynthesizedField())), Annotations(entry.value)),
                Annotations(entry))
      case None =>
        map
          .key("baseUriParameters")
          .foreach(ctx.violation(api.id, "'baseUri' not defined and 'baseUriParameters' defined.", _))
    }

    map.key("(servers)").foreach { entry =>
      entry.value.as[Seq[YMap]].map(OasServerParser(api.id, _)(toOas(ctx)).parse()).foreach { server =>
        api.add(WebApiModel.Servers, server)
      }
    }
  }
}

case class OasServersParser(map: YMap, api: WebApi)(implicit val ctx: OasWebApiContext) extends SpecParserOps {
  def parse(): Unit = {
    if (baseUriExists(map)) {
      var host     = ""
      var basePath = ""

      val annotations = Annotations()

      map.key("basePath").foreach { entry =>
        annotations += BasePathLexicalInformation(Range(entry.range))
        basePath = entry.value.as[String]

        if (!basePath.startsWith("/")) {
          ctx.violation(api.id, "'basePath' property must start with '/'", entry.value)
          basePath = "/" + basePath
        }
      }
      map.key("host").foreach { entry =>
        annotations += HostLexicalInformation(Range(entry.range))
        host = entry.value.as[String]
      }

      val server = Server().set(ServerModel.Url, AmfScalar(host + basePath), annotations)

      map.key("x-serverDescription", ServerModel.Description in server)

      map.key(
        "x-base-uri-parameters",
        entry => {
          val uriParameters =
            OasHeaderParametersParser(entry.value.as[YMap], server.withVariable).parse()
          server.set(ServerModel.Variables, AmfArray(uriParameters, Annotations(entry.value)), Annotations(entry))
        }
      )

      api.set(WebApiModel.Servers, AmfArray(Seq(server.add(SynthesizedField())), Annotations()))
    }

    map.key("x-servers").foreach { entry =>
      entry.value.as[Seq[YMap]].map(OasServerParser(api.id, _).parse()).foreach { server =>
        api.add(WebApiModel.Servers, server)
      }
    }
  }

  def baseUriExists(map: YMap): Boolean = map.key("host").orElse(map.key("basePath")).isDefined
}

private case class OasServerParser(parent: String, map: YMap)(implicit val ctx: OasWebApiContext)
    extends SpecParserOps {
  def parse(): Server = {
    val server = Server()

    map.key("url", ServerModel.Url in server)

    map.key("description", ServerModel.Description in server)

    map.key("variables").foreach { entry =>
      val variables = entry.value
        .as[YMap]
        .entries
        .map(Raml10ParameterParser(_, name => Parameter().withName(name).adopted(parent))(toRaml(ctx)).parse())

      server.set(ServerModel.Variables, AmfArray(variables, Annotations(entry.value)), Annotations(entry))
    }

    AnnotationParser(server, map).parse()

    server
  }
}