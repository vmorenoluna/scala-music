package scalamusic.performance

import scalamusic.performance.Accent.{Accent, HEAVY_ACCENT, LIGHT_ACCENT, NO_ACCENT}
import scalamusic.performance.Performance.TickedTime
import spire.math.Rational

/**
 * An object with methods related to timing
 */
object Meter {

  val ticksPerQuarterNote = 96

  /**
   * Calculate the ticked duration of one whole note
   *
   * @param ticksQN how many ticks there are in a quarter note.
   * @return the ticked duration of one whole note
   */
  def tickedWholeNote(ticksQN: Int = ticksPerQuarterNote): Int =
    4 * ticksQN

  /**
   * Calculate the ticked duration of a single beat
   *
   * @param ticksQN how many ticks there are in a quarter note.
   * @return the ticked duration of one whole note
   */
  def tickedBeat(beatType: Rational, ticksQN: Int = ticksPerQuarterNote): TickedTime =
    4 * ticksQN * beatType
}

final case class TimeSignature(pulse: Pulse, beatType: Rational, startTime: TickedTime) {
  def getTickedBeat(): TickedTime =
    Meter.tickedBeat(beatType)
}

object Accent extends Enumeration {
  type Accent = Value
  val NO_ACCENT, LIGHT_ACCENT, HEAVY_ACCENT = Value
}

trait Pulse {
  def calculateAccent(t: TickedTime, tickedBeat: TickedTime, startTime: TickedTime): Accent
}

final case class NoPulse() extends Pulse {
  /**
   * Return the type of accent to use for marking the pulse accents.
   * NoPulse always has no accents.
   *
   * @param t           the ticked time of the note
   * @param tickedBeat  the ticked time of a beat
   * @param startTime   the ticked time at which this pulse started
   * @return  the accent type
   */
  override def calculateAccent(t: TickedTime, tickedBeat: TickedTime, startTime: TickedTime): Accent =
    NO_ACCENT
}

final case class DuplePulse() extends Pulse {
  /**
   * Return the type of accent to use for marking the pulse accents.
   * In duple pulse the first beat has an accent.
   *
   * @param t           the ticked time of the note
   * @param tickedBeat  the ticked time of a beat
   * @param startTime   the ticked time at which this pulse started
   * @return  the accent type
   */
  override def calculateAccent(t: TickedTime, tickedBeat: TickedTime, startTime: TickedTime): Accent = {
    val relativeTime: Int = (t - startTime).intValue
    val heavyAccentTick: Int = (2 * tickedBeat).intValue
    if(relativeTime % heavyAccentTick == 0)
      HEAVY_ACCENT
    else {
      NO_ACCENT
    }
  }
}

final case class QuadruplePulse() extends Pulse {
  /**
   * Return the type of accent to use for marking the pulse accents.
   * In quadruple pulse the first and the third beat have an accent, with the first beat having a
   * stronger accent than the one on the third beat.
   *
   * @param t           the ticked time of the note
   * @param tickedBeat  the ticked time of a beat
   * @param startTime   the ticked time at which this pulse started
   * @return  the accent type
   */
  override def calculateAccent(t: TickedTime, tickedBeat: TickedTime, startTime: TickedTime): Accent = {
    val relativeTime: Int = (t - startTime).intValue
    val heavyAccentTick: Int = (4 * tickedBeat).intValue
    val lightAccentTick: Int = (2 * tickedBeat).intValue
    if(relativeTime % heavyAccentTick == 0)
      HEAVY_ACCENT
    else if(relativeTime % lightAccentTick == 0)
      LIGHT_ACCENT
    else {
      NO_ACCENT
    }
  }
}

final case class TriplePulse() extends Pulse {
  /**
   * Return the type of accent to use for marking the pulse accents.
   * In triple pulse the first beat has an accent.
   *
   * @param t           the ticked time of the note
   * @param tickedBeat  the ticked time of a beat
   * @param startTime   the ticked time at which this pulse started
   * @return  the accent type
   */
  override def calculateAccent(t: TickedTime, tickedBeat: TickedTime, startTime: TickedTime): Accent = {
    val relativeTime: Int = (t - startTime).intValue
    val heavyAccentTick: Int = (3 * tickedBeat).intValue
    if(relativeTime % heavyAccentTick == 0)
      HEAVY_ACCENT
    else {
      NO_ACCENT
    }
  }
}
