package scalamusic.performance

import scalamusic.performance.Accent.{Accent, HEAVY_ACCENT, LIGHT_ACCENT, NO_ACCENT}
import scalamusic.performance.Metronome.ticksPerQuarterNote
import scalamusic.performance.Performance.TickedTime
import spire.math.Rational

/**
 * An object with methods related to timing
 */
object Metronome {

  val ticksPerQuarterNote = 96

  /**
   * Calculate the ticked duration of one whole note
   *
   * @param ticksQN how many ticks there are in a quarter note.
   * @return the ticked duration of one whole note
   */
  def tickedWholeNote(ticksQN: Int = ticksPerQuarterNote): Int =
    4 * ticksQN

}

final case class TimeSignature(pulse: Pulse, beatType: Rational, startTime: TickedTime)

object Accent extends Enumeration {
  type Accent = Value
  val NO_ACCENT, LIGHT_ACCENT, HEAVY_ACCENT = Value
}

trait Pulse {
  def calculateAccent(t: TickedTime, beatType: Rational, startTime: TickedTime, ticksQN: Int = ticksPerQuarterNote): Accent
}

final case class NoPulse() extends Pulse {
  override def calculateAccent(t: TickedTime, beatType: Rational, startTime: TickedTime, ticksQN: Int = ticksPerQuarterNote): Accent =
    NO_ACCENT
}

final case class DuplePulse() extends Pulse {
  /**
   * Return the type of accent to use for marking the pulse accents.
   * In duple pulse the first beat has an accent.
   *
   * @param t         the ticked time of the note
   * @param beatType  the type of the beat
   * @param startTime the ticked time at which this pulse started
   * @param ticksQN   optional number of ticks per quarter note
   * @return  the accent type
   */
  override def calculateAccent(t: TickedTime, beatType: Rational, startTime: TickedTime, ticksQN: Int = ticksPerQuarterNote): Accent = {
    val relativeTime: Int = (t - startTime).intValue
    val ticksPerBeat: Rational = 4 * ticksQN * beatType
    val heavyAccentTick: Int = (2 * ticksPerBeat).intValue
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
   * @param t         the ticked time of the note
   * @param beatType  the type of the beat
   * @param startTime the ticked time at which this pulse started
   * @param ticksQN   optional number of ticks per quarter note
   * @return  the accent type
   */
  override def calculateAccent(t: TickedTime, beatType: Rational, startTime: TickedTime, ticksQN: Int = ticksPerQuarterNote): Accent = {
    val relativeTime: Int = (t - startTime).intValue
    val ticksPerBeat: Rational = 4 * ticksQN * beatType
    val heavyAccentTick: Int = (4 * ticksPerBeat).intValue
    val lightAccentTick: Int = (2 * ticksPerBeat).intValue
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
   * @param t         the ticked time of the note
   * @param beatType  the type of the beat
   * @param startTime the ticked time at which this pulse started
   * @param ticksQN   optional number of ticks per quarter note
   * @return  the accent type
   */
  override def calculateAccent(t: TickedTime, beatType: Rational, startTime: TickedTime, ticksQN: Int = ticksPerQuarterNote): Accent = {
    val relativeTime: Int = (t - startTime).intValue
    val ticksPerBeat: Rational = 4 * ticksQN * beatType
    val heavyAccentTick: Int = (3 * ticksPerBeat).intValue
    if(relativeTime % heavyAccentTick == 0)
      HEAVY_ACCENT
    else {
      NO_ACCENT
    }
  }
}
