package scalamusic.performance

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

trait Pulse {
  protected val noAccent: Int = 0
  protected val lightAccent: Int = 10
  protected val heavyAccent: Int = 20

  def calculateAccent(t: TickedTime, beatType: Rational, startTime: TickedTime, ticksQN: Int = ticksPerQuarterNote): Int
}

final case class NoPulse() extends Pulse {
  override def calculateAccent(t: TickedTime, beatType: Rational, startTime: TickedTime, ticksQN: Int = ticksPerQuarterNote): Int =
    noAccent
}

final case class DuplePulse() extends Pulse {
  override def calculateAccent(t: TickedTime, beatType: Rational, startTime: TickedTime, ticksQN: Int = ticksPerQuarterNote): Int = ???
}

final case class QuadruplePulse() extends Pulse {
  /**
   * Return the amount by which the note volume should be increased to mark the pulse accents.
   * In quadruple pulse the first and the third beat have an accent, with the first beat having a
   * stronger accent than the one on the third beat.
   *
   * @param t         the ticked time of the note
   * @param beatType  the type of the beat
   * @param startTime the ticked time at which this pulse started
   * @param ticksQN   optional number of ticks per quarter note
   * @return
   */
  override def calculateAccent(t: TickedTime, beatType: Rational, startTime: TickedTime, ticksQN: Int = ticksPerQuarterNote): Int = {
    val relativeTime: Int = (t - startTime).intValue
    val ticksPerBeat: Rational = 4 * ticksQN * beatType
    val heavyAccentTick: Int = (ticksPerBeat / beatType).intValue
    val lightAccentTick: Int = (2 * ticksPerBeat).intValue
    // TODO let the player pass the accent values, if they are random from a range it could sound more natural (so that everytime the accent is slightly different)
    if(relativeTime % heavyAccentTick == 0) {
      heavyAccent
    }
    else if(relativeTime % lightAccentTick == 0) {
      lightAccent
    }
    else {
      noAccent
    }
  }

}

final case class TriplePulse() extends Pulse {
  override def calculateAccent(t: TickedTime, beatType: Rational, startTime: TickedTime, ticksQN: Int = ticksPerQuarterNote): Int = ???
}

final case class SkipplePulse() extends Pulse {
  override def calculateAccent(t: TickedTime, beatType: Rational, startTime: TickedTime, ticksQN: Int = ticksPerQuarterNote): Int = ???
}
