package scalamusic.music

import scalamusic.music.Types.PitchClass._
import scalamusic.music.Types.{AbsPitch, Octave, Pitch}
import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.{Generator, GeneratorDrivenPropertyChecks}

abstract class UnitSpec extends AnyFlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  val pitchClassGen: Generator[PitchClass] = specificValues(C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B)
  val absPitchGen: Generator[AbsPitch] = intsBetween(0, 127)
  val octaveGen: Generator[Octave] = intsBetween(-1, 9)
  def pitchGen: Generator[Pitch] = tuple2s(pitchClassGen, octaveGen)

}
