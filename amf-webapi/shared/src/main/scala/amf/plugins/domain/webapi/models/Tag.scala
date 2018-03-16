package amf.plugins.domain.webapi.models

import amf.core.model.domain.DomainElement
import amf.core.parser.{Annotations, Fields}
import amf.plugins.domain.shapes.models.CreativeWork
import amf.plugins.domain.webapi.metamodel.TagModel
import amf.plugins.domain.webapi.metamodel.TagModel._
import org.yaml.model.YMap

/**
  * Tag internal model
  */
case class Tag(fields: Fields, annotations: Annotations) extends DomainElement {

  def name: String                = fields(Name)
  def description: String         = fields(Description)
  def documentation: CreativeWork = fields(Documentation)

  def withName(name: String): this.type                         = set(Name, name)
  def withDescription(description: String): this.type           = set(Description, description)
  def withDocumentation(documentation: CreativeWork): this.type = set(Documentation, documentation)

  override def adopted(parent: String): this.type = withId(parent + "/tag")

  override def meta: TagModel.type = TagModel

}

object Tag {

  def apply(): Tag = apply(Annotations())

  def apply(ast: YMap): Tag = apply(Annotations(ast))

  def apply(annotations: Annotations): Tag = new Tag(Fields(), annotations)
}
