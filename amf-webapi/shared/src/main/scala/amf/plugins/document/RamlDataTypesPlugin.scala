package amf.plugins.document
import amf.client.plugins.{AMFDocumentPlugin, AMFPlugin}
import amf.core.Root
import amf.core.client.ParsingOptions
import amf.core.emitter.RenderOptions
import amf.core.metamodel.Obj
import amf.core.model.document.{BaseUnit, EncodesModel}
import amf.core.model.domain.AnnotationGraphLoader
import amf.core.parser.{ErrorHandler, ParserContext}
import amf.core.remote.{Platform, Raml10, Vendor}
import amf.core.unsafe.PlatformSecrets
import amf.plugins.document.webapi.RamlPlugin
import amf.plugins.document.webapi.contexts.{Raml10SpecEmitterContext, Raml10WebApiContext, RamlSpecEmitterContext, RamlWebApiContext}
import amf.plugins.document.webapi.model.DataTypeFragment
import amf.plugins.document.webapi.parser.RamlFragmentHeader.Raml10DataType
import amf.plugins.document.webapi.parser.spec.raml.{RamlFragmentEmitter, RamlFragmentParser}
import amf.plugins.document.webapi.parser.spec.{RamlWebApiDeclarations, WebApiDeclarations}
import amf.plugins.document.webapi.parser.{RamlFragmentHeader, RamlHeader}
import amf.plugins.domain.shapes.DataShapesDomainPlugin
import amf.plugins.domain.shapes.models.AnyShape
import amf.plugins.features.validation.ResolutionSideValidations
import amf.{ProfileName, RamlProfile}
import org.yaml.model.YDocument

object RamlDataTypesPlugin extends AMFDocumentPlugin with RamlPlugin with PlatformSecrets {

  override  val ID: String = "RAML 1.0 DataType"

  override  val allowRecursiveReferences: Boolean = true

  override  val vendors: Seq[String] = Seq(ID)

  override def modelEntities: Seq[Obj] = DataShapesDomainPlugin.modelEntities

  override def serializableAnnotations(): Map[String, AnnotationGraphLoader] = DataShapesDomainPlugin.serializableAnnotations()

  override def resolve(unit:  BaseUnit, errorHandler:  ErrorHandler, pipelineId:  String): BaseUnit = {
    errorHandler.violation(
      ResolutionSideValidations.UnsupportedPipeline,
      s"Unsupported '$pipelineId' on $ID plugin",
      unit.location().getOrElse(unit.id)
    )
    unit
  }

  override def documentSyntaxes: Seq[String] =  Seq(
    "application/raml",
    "application/raml+json",
    "application/raml+yaml",
    "text/yaml",
    "text/x-yaml",
    "application/yaml",
    "application/x-yaml",
    "text/vnd.yaml",
    "application/json",
    "application/yaml",
    "application/x-yaml",
    "text/vnd.yaml",
    "application/openapi+json",
    "application/swagger+json",
    "application/openapi+yaml",
    "application/swagger+yaml",
    "application/openapi",
    "application/swagger"
  )



  override def parse(root: Root,
                     parentContext: ParserContext,
                     platform: Platform,
                     options: ParsingOptions,
                     inlined: Boolean = false): Option[BaseUnit] = {

    val updated = context(parentContext, root)
    inlineExternalReferences(root, updated)
    val finalContext = if (inlined) {
      val clean = context(parentContext, root)
      clean.globalSpace = parentContext.globalSpace
      clean.reportDisambiguation = parentContext.reportDisambiguation
      clean
    } else {
      cleanContext(parentContext, root)
    }


    RamlFragmentParser(root, Raml10DataType)(finalContext).parseFragment()
  }


  override protected  def unparseAsYDocument(unit:  BaseUnit, renderOptions:  RenderOptions): Option[YDocument] = {
    unit match {
      case fragment: EncodesModel =>
        fragment.encodes match {
          case shape: AnyShape =>
            val dataTypeFragment = new DataTypeFragment(fragment.fields, fragment.annotations)
            implicit val context: RamlSpecEmitterContext = specContext(renderOptions)
            Some(new RamlFragmentEmitter(dataTypeFragment).emitFragment())
          case _ =>
            throw new UnsupportedOperationException(s"Unsupported fragment type: $fragment")
        }
      case _ =>
        throw new UnsupportedOperationException(s"Unsupported unit type: $unit")
    }
  }

  override def canParse(document:  Root): Boolean = {
    RamlHeader(document) match {
      case Some(header: RamlHeader) if header.text == RamlFragmentHeader.Raml10DataType.text => true
      case _                                                                                 => false
    }
  }

  override def canUnparse(unit:  BaseUnit): Boolean = {
    unit match {
      case fragment: EncodesModel =>
        modelEntities.contains(fragment.encodes.meta) // we check they are trying to unparse a fragment (any fragment) with one of the supported domain entities
      case _                      =>
        false
    }
  }

  override def dependencies(): Seq[AMFPlugin] = Seq(DataShapesDomainPlugin)

  override def context(wrapped: ParserContext, root: Root, ds: Option[WebApiDeclarations] = None): RamlWebApiContext =
    new Raml10WebApiContext(root.location,
      root.references ++ wrapped.refs,
      wrapped,
      ds.map(d => RamlWebApiDeclarations(d)))

  def specContext(options: RenderOptions): RamlSpecEmitterContext = new Raml10SpecEmitterContext(options.errorHandler)


  override protected  def vendor: Vendor = Raml10
  override  val validationProfile: ProfileName = RamlProfile
}
