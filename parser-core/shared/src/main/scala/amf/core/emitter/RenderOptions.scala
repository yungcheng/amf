package amf.core.emitter

import amf.plugins.document.graph.parser.ScalarEmitter
import amf.client.render.{RenderOptions => ClientRenderOptions}

/**
  * Render options
  */
class RenderOptions {

  private var sources: Boolean                     = false
  private var customEmitter: Option[ScalarEmitter] = None
  private var compactUris: Boolean                 = false
  private var rawSourceMaps: Boolean               = false
  private var validating: Boolean                  = false

  /** Include source maps when rendering to graph. */
  def withSourceMaps: RenderOptions = {
    sources = true
    this
  }

  /** Include source maps when rendering to graph. */
  def withoutSourceMaps: RenderOptions = {
    sources = false
    this
  }

  def withCompactUris: RenderOptions = {
    compactUris = true
    this
  }

  def withoutCompactUris: RenderOptions = {
    compactUris = false
    this
  }

  def withRawSourceMaps: RenderOptions = {
    rawSourceMaps = true
    this
  }

  def withoutRawSourceMaps: RenderOptions = {
    rawSourceMaps = false
    this
  }

  def withCustomEmitter(emitter: ScalarEmitter): RenderOptions = {
    customEmitter = Some(emitter)
    this
  }

  def withValidation: RenderOptions = {
    validating = true
    this
  }

  def withoutValidation: RenderOptions = {
    validating = false
    this
  }

  def isCompactUris: Boolean                  = compactUris
  def isWithSourceMaps: Boolean               = sources
  def isWithRawSoureMaps: Boolean             = rawSourceMaps
  def getCustomEmitter: Option[ScalarEmitter] = customEmitter
  def isValidation: Boolean                   = customEmitter.isDefined || validating// I consider that if CustomEmitter is defined, is a validation
}

object RenderOptions {
  def apply(): RenderOptions = new RenderOptions()

  def apply(client: ClientRenderOptions): RenderOptions = {
    val opts = if (client.isWithSourceMaps)
      new RenderOptions().withSourceMaps
    else
      new RenderOptions().withoutSourceMaps
    if (client.isWithCompactUris)
      opts.withCompactUris
    else
      opts.withoutCompactUris
  }
}