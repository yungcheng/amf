package amf.plugins.document.vocabularies.model.domain
import amf.client.plugins.AMFDocumentPlugin
import amf.core.Root
import amf.core.client.ParsingOptions
import amf.core.metamodel.Obj
import amf.core.model.StrField
import amf.core.model.document.{EncodesModel, Fragment}
import amf.core.model.domain.{DomainElement, Linkable}
import amf.core.parser.{Annotations, Fields}
import amf.core.registries.AMFPluginsRegistry
import amf.core.unsafe.PlatformSecrets
import amf.core.utils._
import amf.plugins.document.vocabularies.metamodel.domain.PluginNodeMappingModel
import amf.plugins.document.vocabularies.metamodel.domain.PluginNodeMappingModel._
import amf.plugins.document.vocabularies.parser.instances.DialectInstanceContext
import org.yaml.builder.YDocumentBuilder
import org.yaml.model.YDocument.PartBuilder
import org.yaml.model.YMap

case class PluginNodeMapping(fields: Fields, annotations: Annotations) extends DomainElement with Linkable with NodeMappable with PlatformSecrets {

  def pluginFragment: StrField = fields.field(PluginFragment)
  def pluginVendor: StrField   = fields.field(PluginVendor)

  def withPluginFragment(fragment: String): PluginNodeMapping = set(PluginFragment, fragment)
  def withPluginVendor(vendor: String): PluginNodeMapping   = set(PluginVendor, vendor)

  override def meta: Obj = PluginNodeMappingModel
  override def linkCopy(): Linkable = PluginNodeMapping().withId(id)
  override protected  def classConstructor: (Fields, Annotations) => Linkable with DomainElement = PluginNodeMapping.apply
  override def componentId: String = "/" + name.value().urlComponentEncoded

  def findSyntaxPlugin: Seq[AMFDocumentPlugin] = {
    val vendor = pluginVendor.value().trim

    AMFPluginsRegistry.documentPluginForVendor(vendor)
  }

  def parse(root: Root, ctx: DialectInstanceContext): Option[DomainElement] = {
    findSyntaxPlugin.find(_.canParse(root)) match {
      case Some(domainPlugin) =>
        val newCtx = ctx.copyWithSonsReferences()
        domainPlugin.parse(root, newCtx, platform, ParsingOptions(), inlined = true) match {
          case Some(baseUnit: EncodesModel) =>
            val parsed = baseUnit.encodes
            Some(parsed)
          case _ =>
            None
        }
      case _                  =>
        None
    }
  }

  def emit(b: PartBuilder, domainElement: DomainElement) = {
    val wrapperFragment = new Fragment {
      override  val fields: Fields = Fields()
      override  val annotations: Annotations = Annotations()
    }
    wrapperFragment.withEncodes(domainElement)
    wrapperFragment.withLocation(domainElement.id + "_wrapper")
    findSyntaxPlugin.find(_.canUnparse(wrapperFragment)) match {
      case Some(documentPlugin) =>
        val docBuilder = new YDocumentBuilder()
        documentPlugin.emit(wrapperFragment, docBuilder)
        b += docBuilder.result.node
      case _                    =>
        // ignore
    }
  }

  override def declarationNameProperty: String = Name.value.iri()
}


object PluginNodeMapping {
  def apply(): PluginNodeMapping = apply(Annotations())
  def apply(ast: YMap): PluginNodeMapping = apply(Annotations(ast))
  def apply(annotations: Annotations): PluginNodeMapping = PluginNodeMapping(Fields(), annotations)
}