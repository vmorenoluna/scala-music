package scalamusic.performance

import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import Metronome.tickedWholeNote
import scalamusic.core.Music.qn

class MeterSpec extends AnyFlatSpec with Matchers {

  val ticksPerQuarterNote = 96

  "tickedWholeNote" should "return the ticked duration of a whole note" in {
    tickedWholeNote(ticksPerQuarterNote) should equal(4 * ticksPerQuarterNote)
  }

  "NoPulse" should "always return an accent modifier equal to 0" in {
    NoPulse().calculateAccent(0, qn, 0) should equal(0)
  }

  "QuadruplePulse" should "return the correct accents' modifiers" in {
    QuadruplePulse().calculateAccent(0, qn, 0, 96) should equal(20)
    QuadruplePulse().calculateAccent(96, qn, 0, 96) should equal(0)
    QuadruplePulse().calculateAccent(192, qn, 0, 96) should equal(10)
    QuadruplePulse().calculateAccent(288, qn, 0, 96) should equal(0)
    QuadruplePulse().calculateAccent(384, qn, 0, 96) should equal(20)
    QuadruplePulse().calculateAccent(480, qn, 0, 96) should equal(0)
    QuadruplePulse().calculateAccent(576, qn, 0, 96) should equal(10)
    QuadruplePulse().calculateAccent(672, qn, 0, 96) should equal(0)

    QuadruplePulse().calculateAccent(768, qn, 384, 96) should equal(20)
    QuadruplePulse().calculateAccent(864, qn, 384, 96) should equal(0)
    QuadruplePulse().calculateAccent(960, qn, 384, 96) should equal(10)
    QuadruplePulse().calculateAccent(1056, qn, 384, 96) should equal(0)
    QuadruplePulse().calculateAccent(1152, qn, 384, 96) should equal(20)
    QuadruplePulse().calculateAccent(1248, qn, 384, 96) should equal(0)
    QuadruplePulse().calculateAccent(1344, qn, 384, 96) should equal(10)
    QuadruplePulse().calculateAccent(1440, qn, 384, 96) should equal(0)
  }

}
