package scalamusic.core

import cats.kernel.{Eq, Order}
import cats.syntax.eq._
import scalamusic.core.Types.PitchClass.PitchClass
import spire.math.Rational

import scala.math.Integral.Implicits._

/**
 * Object containing all the low level types
 */
object Types {

  type Octave = Int
  type AbsPitch = Int
  type Step = Int
  type Volume = Int
  type Pitch = (PitchClass, Octave)
  type Duration = Rational

  /**
   * Half step
   */
  val hs: Step = 1
  /**
   * Whole step
   */
  val ws: Step = 2

  /**
   * Enumerates all the available pitch class in an octave
   */
  object PitchClass extends Enumeration {
    type PitchClass = Value
    val Cff, Cf, C, Dff, Cs, Df, Css, D,
    Eff, Ds, Ef, Fff, Dss, E, Ff, Es, F,
    Gff, Ess, Fs, Gf, Fss, G, Aff, Gs, Af,
    Gss, A, Bff, As, Bf, Ass, B, Bs, Bss = Value
  }

  implicit val octaveEq: Eq[Octave] = Eq.fromUniversalEquals
  implicit val pitchClassEq: Eq[PitchClass] = Eq.fromUniversalEquals
  implicit val pitchEq: Eq[Pitch] = Eq.fromUniversalEquals
  implicit val pitchOrder: Order[Pitch] = Order.from((a, b) => {
    (a, b) match {
      case ((p1, o1), (p2, o2)) if o1 > o2 || (o1 === o2 && (p1.compare(p2) > 0)) => 1
      case ((p1, o1), (p2, o2)) if o1 === o2 && p1 === p2 => 0
      case ((p1, o1), (p2, o2)) if o1 < o2 || (o1 === o2 && (p1.compare(p2) < 0)) => -1
    }
  }
  )

  /**
   * Convert a Pitch to an AbsPitch.
   * The conversion is made in a way that
   * (C,-1) = 0
   * (C,4) = 60
   * (G,9) = 127
   *
   * @param p the pitch to convert
   * @return
   */
  def absPitch(p: Pitch): AbsPitch = 12 * (p._2 + 1) + pcToInt(p._1)

  private def pcToInt(pitchClass: PitchClass): Int = {
    import PitchClass._
    pitchClass match {
      case Cff => -2
      case Cf => -1
      case C => 0
      case Dff => 0
      case Cs => 1
      case Df => 1
      case Css => 2
      case D => 2
      case Eff => 2
      case Ds => 3
      case Ef => 3
      case Fff => 3
      case Dss => 4
      case E => 4
      case Ff => 4
      case Es => 5
      case F => 5
      case Gff => 5
      case Ess => 6
      case Fs => 6
      case Gf => 6
      case Fss => 7
      case G => 7
      case Aff => 7
      case Gs => 8
      case Af => 8
      case Gss => 9
      case A => 9
      case Bff => 9
      case As => 10
      case Bf => 10
      case Ass => 11
      case B => 11
      case Bs => 12
      case Bss => 13
    }
  }

  /**
   * Convert an AbsPitch to a Pitch.
   * The conversion is made in a way that doesn't consider all possible pitch classes, due to the enharmonic equivalence.
   * The resulting Pitch has a Pitch class in
   * (C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B)
   *
   * @param ap the absolute pitch to convert
   * @return
   */
  def pitch(ap: AbsPitch): Pitch = {
    val (oct, n) = ap /% 12
    import PitchClass._
    val pitchClass = List(C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B)(n)
    (pitchClass, oct - 1)
  }

  /**
   * Transpose a pitch by the given amount of half steps
   *
   * @param value number of half steps
   * @param p the pitch to transpose
   * @return
   */
  def transpose(value: Step, p: Pitch): Pitch = pitch(absPitch(p) + value)

  /**
   * Return the maximum among two given pitches
   *
   * @param p1 the first pitch
   * @param p2 the second pitch
   * @return
   */
  def max(p1: Pitch, p2: Pitch): Pitch = if (absPitch(p1) > absPitch(p2)) p1 else p2

  final implicit class PitchOps(private val p: Pitch) {
    /**
     * Convert this Pitch to its equivalent AbsPitch.
     * The conversion is made in a way that
     * (C,-1) = 0
     * (C,4) = 60
     * (G,9) = 127
     *
     * @return
     */
    def toAbsPitch(): AbsPitch = absPitch(p)

    /**
     * Transpose this pitch by the given amount of half steps
     *
     * @param value number of half steps
     * @return
     */
    def transpose(value: Step): Pitch = pitch(absPitch(p) + value)

    /**
     * Return the maximum among this pitch and another given pitch
     *
     * @param pi the pitch to compare with
     * @return
     */
    def max(pi: Pitch): Pitch = if (absPitch(p) > absPitch(pi)) p else pi
  }

  final implicit class AbsPitchOps(private val ap: AbsPitch) {
    /**
     * Convert this AbsPitch to a Pitch.
     * The conversion is made in a way that doesn't consider all possible pitch classes, due to the enharmonic equivalence.
     * The resulting Pitch has a Pitch class in
     * (C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B)
     *
     * @return
     */
    def toPitch(): Pitch = pitch(ap)
  }

}
