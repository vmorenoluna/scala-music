package scalamusic.performance

/**
 * An object with methods related to timing
 */
object Metronome {

  /**
   * Calculate the ticked duration of one whole note
   *
   * @param ticksPerQuarterNote   the note type associated with one beat
   * @return the ticked duration of one whole note
   */
  def tickedWholeNote(ticksPerQuarterNote: Int): Int =
    4 * ticksPerQuarterNote

}
