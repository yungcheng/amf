package amf.core.client

import amf.core.model.document.BaseUnit
import amf.core.resolution.pipelines.ResolutionPipeline
import amf.core.services.RuntimeResolver

abstract class PlatformResolver(vendor: String) {

  def resolve(unit: BaseUnit): BaseUnit           = RuntimeResolver.resolve(vendor, unit, ResolutionPipeline.DEFAULT_PIPELINE)
  def resolve(unit: BaseUnit, pipelineId: String) = RuntimeResolver.resolve(vendor, unit, pipelineId)
}