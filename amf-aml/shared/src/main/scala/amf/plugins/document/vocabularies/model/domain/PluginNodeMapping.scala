package amf.plugins.document.vocabularies.model.domain
import amf.core.metamodel.Obj
import amf.core.model.StrField
import amf.core.model.domain.{DomainElement, Linkable}
import amf.core.parser.{Annotations, Fields}
import amf.plugins.document.vocabularies.metamodel.domain.PluginNodeMappingModel
import amf.plugins.document.vocabularies.metamodel.domain.PluginNodeMappingModel._
import org.yaml.model.YMap
import amf.core.utils._

case class PluginNodeMapping(fields: Fields, annotations: Annotations) extends DomainElement with Linkable with NodeMappable {

  def pluginFragment: StrField = fields.field(PluginFragment)
  def pluginVendor: StrField   = fields.field(PluginVendor)

  def withPluginFragment(fragment: String): PluginNodeMapping = set(PluginFragment, fragment)
  def withPluginVendor(vendor: String): PluginNodeMapping   = set(PluginVendor, vendor)

  override def meta: Obj = PluginNodeMappingModel
  override def linkCopy(): Linkable = PluginNodeMapping().withId(id)
  override protected  def classConstructor: (Fields, Annotations) => Linkable with DomainElement = PluginNodeMapping.apply
  override def componentId: String = "/" + name.value().urlComponentEncoded

}


object PluginNodeMapping {
  def apply(): PluginNodeMapping = apply(Annotations())
  def apply(ast: YMap): PluginNodeMapping = apply(Annotations(ast))
  def apply(annotations: Annotations): PluginNodeMapping = PluginNodeMapping(Fields(), annotations)
}