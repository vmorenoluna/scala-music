package scalamusic.music

import scalamusic.music.InstrumentName.InstrumentName
import scalamusic.music.Mode.Mode
import scalamusic.music.Types.AbsPitch
import scalamusic.music.Types.PitchClass.PitchClass
import scalamusic.performance.players.PlayersEnum.PlayersEnum
import spire.math.Rational

sealed trait Control
// TODO eq and ord typeclasses
final case class CtrlTempo(value: Rational) extends Control
final case class Transpose(value: AbsPitch) extends Control
final case class Instrument(instrumentName: InstrumentName) extends Control
final case class Phrase(attributes: List[PhraseAttribute]) extends Control
final case class KeySig(pitchClass: PitchClass, mode: Mode) extends Control
final case class Player(player: PlayersEnum) extends Control

/**
 * Express attributes of notes
 */
sealed trait NoteAttribute

// TODO eq typeclass
final case class Volume(value: Int) extends NoteAttribute // 0-127
final case class Fingering(value: Int) extends NoteAttribute
final case class Dynamics(value: String) extends NoteAttribute
final case class Params(value: List[Double]) extends NoteAttribute

/**
 * Express attributes of phrases
 */
sealed trait PhraseAttribute
// TODO eq and ord typeclasses
final case class Dyn(value: Dynamic) extends PhraseAttribute
final case class Tmp(value: Tempo) extends PhraseAttribute
final case class Art(value: Articulation) extends PhraseAttribute
final case class Orn(value: Ornament) extends PhraseAttribute

sealed trait Dynamic
// TODO eq and ord typeclasses
final case class Accent(value: Rational) extends Dynamic
final case class Crescendo(value: Rational) extends Dynamic
final case class Diminuendo(value: Rational) extends Dynamic
final case class StdLoudness(value: StdLoudness) extends Dynamic
final case class Loudness(value: Rational) extends Dynamic

final object StdLoudness extends Enumeration {
  val StdLoudness = Value
  val PPP, PP, P, MP, SF, MF, NF, FF, FFF = Value
}
// TODO eq and ord typeclasses

sealed trait Tempo
// TODO eq and ord typeclasses
final case class Ritardando(value: Rational) extends Tempo
final case class Accelerando(value: Rational) extends Tempo

sealed trait Articulation
// TODO eq and ord typeclasses
final case class Staccato(value: Rational) extends Articulation
final case class Legato(value: Rational) extends Articulation
final case class Slurred(value: Rational) extends Articulation
final case class Tenuto() extends Articulation
final case class Marcato() extends Articulation
final case class Pedal() extends Articulation
final case class Fermata() extends Articulation
final case class FermataDown() extends Articulation
final case class Breath() extends Articulation
final case class DownBow() extends Articulation
final case class UpBow() extends Articulation
final case class Harmonic() extends Articulation
final case class Pizzicato() extends Articulation
final case class LeftPizz() extends Articulation
final case class BartokPizz() extends Articulation
final case class Swell() extends Articulation
final case class Wedge() extends Articulation
final case class Thumb() extends Articulation
final case class Stopped() extends Articulation

sealed trait Ornament
// TODO eq and ord typeclasses
final case class Trill() extends Ornament
final case class Mordent() extends Ornament
final case class InvMordent() extends Ornament
final case class DoubleMordent() extends Ornament
final case class Turn() extends Ornament
final case class TrilledTurn() extends Ornament
final case class ShortTrill() extends Ornament
final case class Arpeggio() extends Ornament
final case class ArpeggioUp() extends Ornament
final case class ArpeggioDown() extends Ornament
final case class Instruction(value: String) extends Ornament
final case class Head(value: NoteHead.Value) extends Ornament
final case class DiatonicTrans(value: Int) extends Ornament

final object NoteHead extends Enumeration {
  val DiamondHead, SquareHead, XHead, TriangleHead, TremoloHead, SlashHead, ArtHarmonic, NoHead = Value
}
// TODO eq and ord typeclasses
