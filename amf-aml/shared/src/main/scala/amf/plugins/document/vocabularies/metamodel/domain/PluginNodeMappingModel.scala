package amf.plugins.document.vocabularies.metamodel.domain
import amf.core.metamodel.Field
import amf.core.metamodel.Type.Str
import amf.core.metamodel.domain.{DomainElementModel, LinkableElementModel}
import amf.core.model.domain.AmfObject
import amf.core.vocabulary.{Namespace, ValueType}
import amf.plugins.document.vocabularies.model.domain.PluginNodeMapping

object PluginNodeMappingModel extends DomainElementModel with LinkableElementModel with NodeMappableModel {

  val PluginVendor    = Field(Str, Namespace.Meta + "pluginVendor")
  val PluginFragment  = Field(Str, Namespace.Meta + "pluginFragment")

  override def fields: List[Field] = PluginFragment :: PluginVendor :: Name :: DomainElementModel.fields

  override  val `type`: List[ValueType] = Namespace.Meta + "PluginNodeMapping" :: DomainElementModel.`type`

  override def modelInstance: AmfObject = PluginNodeMapping()
}
