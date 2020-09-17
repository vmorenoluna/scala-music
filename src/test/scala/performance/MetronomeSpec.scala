package performance

import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import Metronome.tickedWholeNote

class MetronomeSpec extends AnyFlatSpec with Matchers {

  val ticksPerQuarterNote = 96

  "tickedWholeNote" should "return the ticked duration of a whole note" in {
    tickedWholeNote(ticksPerQuarterNote) should equal(4 * ticksPerQuarterNote)
  }

}
