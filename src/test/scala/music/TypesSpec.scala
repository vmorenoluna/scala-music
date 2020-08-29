package music

import music.Types.PitchClass._
import music.Types._

class TypesSpec extends UnitSpec {

  "Converting an AbsPitch to a Pitch" should "return a valid Pitch for each AbsPitch" in {
    forAll(absPitchGen) { absolutePitch =>
      val (pitchClass, octave): Pitch = absolutePitch.toPitch

      List(C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B) should contain(pitchClass)
      octave should be >= -1
      octave should be <= 9
    }
  }

  "Converting a Pitch to an AbsPitch" should "return a valid AbsPitch for each Pitch" in {
    forAll(pitchGen) { pitch =>
      pitch.toAbsPitch should be >= 0
    }
  }

  "absolutePitch.toPitch.toAbsPitch" should "return the original absolutePitch" in {
    forAll(absPitchGen) { absolutePitch =>
      absolutePitch
        .toPitch
        .toAbsPitch shouldEqual absolutePitch
    }
  }

  "pitch.toAbsPitch.toPitch" should "return the original pitch" in {
    forAll(pitchGen) { pitch =>
      pitch
        .toAbsPitch
        .toPitch shouldEqual pitch
    }
  }

  "Pitch" should "be in the Eq type class" in {
    val pitch: Pitch = (C, 4)

    pitchEq.eqv(pitch, (C,4)) shouldEqual true
    pitchEq.eqv(pitch, (C,5)) shouldEqual false
    pitchEq.eqv(pitch, (D,4)) shouldEqual false
    pitchEq.neqv(pitch, (C,4)) shouldEqual false
    pitchEq.neqv(pitch, (C,5)) shouldEqual true
    pitchEq.neqv(pitch, (E,4)) shouldEqual true
  }

  "Pitch" should "be in the Order type class" in {
    val pitch: Pitch = (C, 4)

    pitchOrder.compare(pitch, (B,3)) shouldEqual 1
    pitchOrder.compare(pitch, (C,4)) shouldEqual 0
    pitchOrder.compare(pitch, (D,4)) shouldEqual -1
    pitchOrder.compare(pitch, (E,4)) shouldEqual -1
    pitchOrder.compare(pitch, (F,4)) shouldEqual -1
    pitchOrder.compare(pitch, (G,4)) shouldEqual -1
    pitchOrder.compare(pitch, (A,4)) shouldEqual -1
    pitchOrder.compare(pitch, (B,4)) shouldEqual -1
    pitchOrder.compare(pitch, (C,5)) shouldEqual -1
  }

  "pitch.transpose" should "correctly transpose a pitch by the given amount of half steps" in {
    val pitch: Pitch = (C, 4)

    pitch.transpose(4) shouldEqual (E,4)
    pitch.transpose(-3) shouldEqual (A,3)
  }

  "pitch.max(p)" should "correctly the maximum pitch among the two pitches" in {
    val pitch: Pitch = (C, 4)

    pitch max (D, 5) shouldEqual (D,5)
    pitch max (G, 3) shouldEqual pitch
  }

}
