package scalamusic.performance

/**
 * An object with methods related to timing
 */
object Metronome {

  /**
   * Calculate the ticked duration of one whole note
   *
   * @param ticksPerQuarterNote   how many ticks there are in a quarter note.
   * @return the ticked duration of one whole note
   */
  def tickedWholeNote(ticksPerQuarterNote: Int): Int =
    4 * ticksPerQuarterNote

}
