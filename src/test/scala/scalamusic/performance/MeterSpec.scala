package scalamusic.performance

import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import Meter.tickedWholeNote
import scalamusic.core.Music.{en, hn, qn}
import scalamusic.performance.Accent.{HEAVY_ACCENT, LIGHT_ACCENT, NO_ACCENT}
import scalamusic.performance.Performance.TickedTime

class MeterSpec extends AnyFlatSpec with Matchers {

  val tickedBeat: TickedTime = Meter.tickedBeat(qn)

  "tickedWholeNote" should "return the ticked duration of a whole note" in {
    val ticksPerQuarterNote = 96
    tickedWholeNote(ticksPerQuarterNote) should equal(4 * ticksPerQuarterNote)
  }

  "tickedBeat" should "return the ticked duration of a single beat" in {
    val ticksPerQuarterNote = 96
    Meter.tickedBeat(hn, ticksPerQuarterNote) should equal(4 * ticksPerQuarterNote * hn)
    Meter.tickedBeat(qn, ticksPerQuarterNote) should equal(4 * ticksPerQuarterNote * qn)
    Meter.tickedBeat(en, ticksPerQuarterNote) should equal(4 * ticksPerQuarterNote * en)
  }

  "NoPulse" should "always return an accent modifier equal to NO_ACCENT" in {
    NoPulse().calculateAccent(0, tickedBeat, 0) should equal(NO_ACCENT)
  }

  "DuplePulse" should "return the correct accents' modifiers" in {
    DuplePulse().calculateAccent(0, tickedBeat, 0) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(96, tickedBeat, 0) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(192, tickedBeat, 0) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(288, tickedBeat, 0) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(384, tickedBeat, 0) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(480, tickedBeat, 0) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(576, tickedBeat, 0) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(672, tickedBeat, 0) should equal(NO_ACCENT)

    DuplePulse().calculateAccent(1440, tickedBeat, 1440) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(1536, tickedBeat, 1440) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(1632, tickedBeat, 1440) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(1728, tickedBeat, 1440) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(1824, tickedBeat, 1440) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(1920, tickedBeat, 1440) should equal(NO_ACCENT)
    DuplePulse().calculateAccent(2016, tickedBeat, 1440) should equal(HEAVY_ACCENT)
    DuplePulse().calculateAccent(2112, tickedBeat, 1440) should equal(NO_ACCENT)
  }

  "QuadruplePulse" should "return the correct accents' modifiers" in {
    QuadruplePulse().calculateAccent(0, tickedBeat, 0) should equal(HEAVY_ACCENT)
    QuadruplePulse().calculateAccent(96, tickedBeat, 0) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(192, tickedBeat, 0) should equal(LIGHT_ACCENT)
    QuadruplePulse().calculateAccent(288, tickedBeat, 0) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(384, tickedBeat, 0) should equal(HEAVY_ACCENT)
    QuadruplePulse().calculateAccent(480, tickedBeat, 0) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(576, tickedBeat, 0) should equal(LIGHT_ACCENT)
    QuadruplePulse().calculateAccent(672, tickedBeat, 0) should equal(NO_ACCENT)

    QuadruplePulse().calculateAccent(1440, tickedBeat, 1440) should equal(HEAVY_ACCENT)
    QuadruplePulse().calculateAccent(1536, tickedBeat, 1440) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(1632, tickedBeat, 1440) should equal(LIGHT_ACCENT)
    QuadruplePulse().calculateAccent(1728, tickedBeat, 1440) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(1824, tickedBeat, 1440) should equal(HEAVY_ACCENT)
    QuadruplePulse().calculateAccent(1920, tickedBeat, 1440) should equal(NO_ACCENT)
    QuadruplePulse().calculateAccent(2016, tickedBeat, 1440) should equal(LIGHT_ACCENT)
    QuadruplePulse().calculateAccent(2112, tickedBeat, 1440) should equal(NO_ACCENT)
  }

  "TriplePulse" should "return the correct accents' modifiers" in {
    TriplePulse().calculateAccent(0, tickedBeat, 0) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(96, tickedBeat, 0) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(192, tickedBeat, 0) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(288, tickedBeat, 0) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(384, tickedBeat, 0) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(480, tickedBeat, 0) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(576, tickedBeat, 0) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(672, tickedBeat, 0) should equal(NO_ACCENT)

    TriplePulse().calculateAccent(1440, tickedBeat, 1440) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(1536, tickedBeat, 1440) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(1632, tickedBeat, 1440) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(1728, tickedBeat, 1440) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(1824, tickedBeat, 1440) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(1920, tickedBeat, 1440) should equal(NO_ACCENT)
    TriplePulse().calculateAccent(2016, tickedBeat, 1440) should equal(HEAVY_ACCENT)
    TriplePulse().calculateAccent(2112, tickedBeat, 1440) should equal(NO_ACCENT)
  }

}
