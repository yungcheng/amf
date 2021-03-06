package amf.plugins.domain.webapi.models.bindings.mqtt
import amf.core.metamodel.{Field, Obj}
import amf.core.model.domain.{Linkable, DomainElement}
import amf.core.model.{BoolField, IntField}
import amf.core.parser.{Annotations, Fields}
import amf.plugins.domain.webapi.metamodel.bindings.MqttOperationBindingModel
import amf.plugins.domain.webapi.metamodel.bindings.MqttOperationBindingModel._
import amf.plugins.domain.webapi.models.bindings.{OperationBinding, BindingVersion}

class MqttOperationBinding(override val fields: Fields, override val annotations: Annotations)
    extends OperationBinding
    with BindingVersion {
  override protected def bindingVersionField: Field = BindingVersion
  override def meta: Obj                            = MqttOperationBindingModel

  def qos: IntField     = fields.field(Qos)
  def retain: BoolField = fields.field(Retain)

  def withQos(qos: Int): this.type           = set(Qos, qos)
  def withRetain(retain: Boolean): this.type = set(Retain, retain)

  override def componentId: String = "/mqtt-operation"
  override def linkCopy(): MqttOperationBinding = MqttOperationBinding().withId(id)

  override protected def classConstructor: (Fields, Annotations) => Linkable with DomainElement = MqttOperationBinding.apply
}

object MqttOperationBinding {

  def apply(): MqttOperationBinding = apply(Annotations())

  def apply(annotations: Annotations): MqttOperationBinding = apply(Fields(), annotations)

  def apply(fields: Fields, annotations: Annotations): MqttOperationBinding = new MqttOperationBinding(fields, annotations)
}