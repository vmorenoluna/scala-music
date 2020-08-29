package performance

import music.Types.Duration
import performance.Performance.DurT

/**
 * An object with a handy method to compute context's cDur
 */
object Metronome {
  /**
   * Calculate the ticked duration of one whole note
   *
   * @param tempoInBPM is the tempo in BPM
   * @param dur        the note type associated with one beat
   * @return the ticked duration of one whole note
   */
  def metro(tempoInBPM: Int, dur: Duration): DurT = {
    val ticksPerBeat = 96
    val ticksPerSecond = ticksPerBeat * (tempoInBPM / 60.0)
    ticksPerSecond
  }
}
