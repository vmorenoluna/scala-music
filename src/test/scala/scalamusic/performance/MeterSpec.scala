package scalamusic.performance

import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import Metronome.tickedWholeNote
import scalamusic.core.Music.qn
import scalamusic.performance.Accent.{HEAVY_ACCENT, LIGHT_ACCENT, NO_ACCENT}

class MeterSpec extends AnyFlatSpec with Matchers {

  val ticksPerQuarterNote = 96

  "tickedWholeNote" should "return the ticked duration of a whole note" in {
    tickedWholeNote(ticksPerQuarterNote) should equal(4 * ticksPerQuarterNote)
  }

  "NoPulse" should "always return an accent modifier equal to NO_ACCENT" in {
    NoPulse().calculateAccent(0, qn, 0) should equal(NO_ACCENT)
  }

  "DuplePulse" should "return the correct accents' modifiers" in {
    DuplePulse().calculateAccent(0, qn, 0, 96) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(96, qn, 0, 96) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(192, qn, 0, 96) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(288, qn, 0, 96) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(384, qn, 0, 96) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(480, qn, 0, 96) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(576, qn, 0, 96) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(672, qn, 0, 96) should equal(NO_ACCENT)

    DuplePulse().calculateAccent(1440, qn, 1440, 96) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(1536, qn, 1440, 96) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(1632, qn, 1440, 96) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(1728, qn, 1440, 96) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(1824, qn, 1440, 96) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(1920, qn, 1440, 96) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(2016, qn, 1440, 96) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(2112, qn, 1440, 96) should equal(NO_ACCENT)
  }

  "QuadruplePulse" should "return the correct accents' modifiers" in {
    QuadruplePulse().calculateAccent(0, qn, 0, 96) should equal(HEAVY_ACCENT)
    QuadruplePulse().calculateAccent(96, qn, 0, 96) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(192, qn, 0, 96) should equal(LIGHT_ACCENT)
    QuadruplePulse().calculateAccent(288, qn, 0, 96) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(384, qn, 0, 96) should equal(HEAVY_ACCENT)
    QuadruplePulse().calculateAccent(480, qn, 0, 96) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(576, qn, 0, 96) should equal(LIGHT_ACCENT)
    QuadruplePulse().calculateAccent(672, qn, 0, 96) should equal(NO_ACCENT)

    QuadruplePulse().calculateAccent(1440, qn, 1440, 96) should equal(HEAVY_ACCENT)
    QuadruplePulse().calculateAccent(1536, qn, 1440, 96) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(1632, qn, 1440, 96) should equal(LIGHT_ACCENT)
    QuadruplePulse().calculateAccent(1728, qn, 1440, 96) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(1824, qn, 1440, 96) should equal(HEAVY_ACCENT)
    QuadruplePulse().calculateAccent(1920, qn, 1440, 96) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(2016, qn, 1440, 96) should equal(LIGHT_ACCENT)
    QuadruplePulse().calculateAccent(2112, qn, 1440, 96) should equal(NO_ACCENT)
  }

  "TriplePulse" should "return the correct accents' modifiers" in {
    TriplePulse().calculateAccent(0, qn, 0, 96) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(96, qn, 0, 96) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(192, qn, 0, 96) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(288, qn, 0, 96) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(384, qn, 0, 96) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(480, qn, 0, 96) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(576, qn, 0, 96) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(672, qn, 0, 96) should equal(NO_ACCENT)

    TriplePulse().calculateAccent(1440, qn, 1440, 96) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(1536, qn, 1440, 96) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(1632, qn, 1440, 96) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(1728, qn, 1440, 96) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(1824, qn, 1440, 96) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(1920, qn, 1440, 96) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(2016, qn, 1440, 96) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(2112, qn, 1440, 96) should equal(NO_ACCENT)
  }

}
