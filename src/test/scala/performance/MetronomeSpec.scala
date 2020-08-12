package performance

import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import Metronome.metro
import music.Music.qn

class MetronomeSpec extends AnyFlatSpec with Matchers {

  "metro" should "return the number of ticks in a second" in {
    val bpm = 120
    val beatNoteType = qn

    metro(bpm, beatNoteType) should equal(192)
  }

}
