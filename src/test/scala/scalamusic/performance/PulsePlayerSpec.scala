package scalamusic.performance

import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import scalamusic.core.InstrumentName.AltoSax
import scalamusic.core.Mode.Major
import scalamusic.core.Music.{dhn, en, hn, qn}
import scalamusic.core.MusicWithAttributes._
import scalamusic.core.Types.Duration
import scalamusic.core.Types.PitchClass.C
import scalamusic.core.{MusicWithAttributes => _, _}
import scalamusic.performance.players.PulsePlayer
import spire.math.Rational

class PulsePlayerSpec extends AnyFlatSpec with Matchers {

  "PulsePlayer" should "interpret notes with NoPulse" in {
    val timeSignature: TimeSignature = TimeSignature(NoPulse(), qn, 0)
    val c: Context[NoteWithAttributes] = buildContext(cTimeSignature = timeSignature)
    val d: Duration = qn
    val n: NoteWithAttributes = ((C, 5), List(Volume(60), Fingering(25), Dynamics("ppp"), Params(List(1.5, 2.0))))

    PulsePlayer.playNote(c, d, n) should equal(
      List(MusicEvent(0, AltoSax, 197, qn, 60, List(1.5, 2.0)))
    )
  }

  "PulsePlayer" should "interpret notes with DuplePulse" in {
    val timeSignature: TimeSignature = TimeSignature(DuplePulse(), qn, 0)
    val c: Context[NoteWithAttributes] = buildContext(cTimeSignature = timeSignature)
    val d: Duration = qn
    val n: NoteWithAttributes = ((C, 5), List.empty)

    PulsePlayer.playNote(c, d, n) should equal(
      List(MusicEvent(0, AltoSax, 197, qn, 74, List.empty))
    )
    PulsePlayer.playNote(c.copy(cTime = c.cTime + 96), d, n) should equal(
      List(MusicEvent(96, AltoSax, 197, qn, 60, List.empty))
    )
    PulsePlayer.playNote(c.copy(cTime = c.cTime + 2*96), d, n) should equal(
      List(MusicEvent(2*96, AltoSax, 197, qn, 74, List.empty))
    )
    PulsePlayer.playNote(c.copy(cTime = c.cTime + 3*96), d, n) should equal(
      List(MusicEvent(3*96, AltoSax, 197, qn, 60, List.empty))
    )
  }

  "PulsePlayer" should "interpret notes with QuadruplePulse" in {
    val timeSignature: TimeSignature = TimeSignature(QuadruplePulse(), qn, 0)
    val c: Context[NoteWithAttributes] = buildContext(cTimeSignature = timeSignature)
    val d: Duration = qn
    val n: NoteWithAttributes = ((C, 5), List.empty)

    PulsePlayer.playNote(c, d, n) should equal(
      List(MusicEvent(0, AltoSax, 197, qn, 74, List.empty))
    )
    PulsePlayer.playNote(c.copy(cTime = c.cTime + 96), d, n) should equal(
      List(MusicEvent(96, AltoSax, 197, qn, 60, List.empty))
    )
    PulsePlayer.playNote(c.copy(cTime = c.cTime + 2*96), d, n) should equal(
      List(MusicEvent(2*96, AltoSax, 197, qn, 67, List.empty))
    )
    PulsePlayer.playNote(c.copy(cTime = c.cTime + 3*96), d, n) should equal(
      List(MusicEvent(3*96, AltoSax, 197, qn, 60, List.empty))
    )
  }

  "PulsePlayer" should "interpret notes with TriplePulse" in {
    val timeSignature: TimeSignature = TimeSignature(TriplePulse(), qn, 0)
    val c: Context[NoteWithAttributes] = buildContext(cTimeSignature = timeSignature)
    val d: Duration = qn
    val n: NoteWithAttributes = ((C, 5), List.empty)

    PulsePlayer.playNote(c, d, n) should equal(
      List(MusicEvent(0, AltoSax, 197, qn, 74, List.empty))
    )
    PulsePlayer.playNote(c.copy(cTime = c.cTime + 96), d, n) should equal(
      List(MusicEvent(96, AltoSax, 197, qn, 60, List.empty))
    )
    PulsePlayer.playNote(c.copy(cTime = c.cTime + 2*96), d, n) should equal(
      List(MusicEvent(2*96, AltoSax, 197, qn, 60, List.empty))
    )
  }

  "PulsePlayer" should "interpret a phrase with Accent" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(Accent(Rational(2))))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    PulsePlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 120, List()),
        MusicEvent(qn, AltoSax, 197, qn, 120, List())
      ), hn)
    )
  }

  "PulsePlayer" should "interpret a phrase with StandardLoudness" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(StandardLoudness(StdLoudness.PPP)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    PulsePlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 40, List()),
        MusicEvent(qn, AltoSax, 197, qn, 40, List())
      ), hn)
    )
  }

  "PulsePlayer" should "interpret a phrase with Loudness" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(Loudness(40)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    PulsePlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 40, List()),
        MusicEvent(qn, AltoSax, 197, qn, 40, List())
      ), hn)
    )
  }

  "PulsePlayer" should "interpret a phrase with Crescendo" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(Crescendo(0.5)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    PulsePlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 60, List()),
        MusicEvent(qn, AltoSax, 197, qn, 70, List()),
        MusicEvent(hn, AltoSax, 197, qn, 80, List())
      ), dhn)
    )
  }

  "PulsePlayer" should "interpret a phrase with Diminuendo" in {
    val c: Context[NoteWithAttributes] = buildContext(cVol = 120)
    val pas: List[PhraseAttribute] = List(Dyn(Diminuendo(0.5)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    PulsePlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 120, List()),
        MusicEvent(qn, AltoSax, 197, qn, 100, List()),
        MusicEvent(hn, AltoSax, 197, qn, 80, List())
      ), dhn)
    )
  }

  "PulsePlayer" should "interpret a phrase with Ritardando" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Tmp(Ritardando(0.5)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    PulsePlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(7, 24), 60, List()),
        MusicEvent(Rational(7, 24), AltoSax, 197, Rational(3, 8), 60, List()),
        MusicEvent(Rational(2, 3), AltoSax, 197, Rational(11, 24), 60, List())
      ), 9*en)
    )
  }

  "PulsePlayer" should "interpret a phrase with Accelerando" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Tmp(Accelerando(0.5)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    PulsePlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(5, 24), 60, List()),
        MusicEvent(Rational(5, 24), AltoSax, 197, en, 60, List()),
        MusicEvent(Rational(1, 3), AltoSax, 197, Rational(1, 24), 60, List())
      ), 3*en)
    )
  }

  "PulsePlayer" should "interpret a phrase with Staccato" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Art(Staccato(Rational(2, 3))))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    PulsePlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(1, 6), 60, List()),
        MusicEvent(qn, AltoSax, 197, Rational(1, 6), 60, List()),
        MusicEvent(hn, AltoSax, 197, Rational(1, 6), 60, List())
      ), dhn)
    )
  }

  "PulsePlayer" should "interpret a phrase with Legato" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Art(Legato(Rational(7, 5))))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    PulsePlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(7, 20), 60, List()),
        MusicEvent(qn, AltoSax, 197, Rational(7, 20), 60, List()),
        MusicEvent(hn, AltoSax, 197, Rational(7, 20), 60, List())
      ), dhn)
    )
  }

  "PulsePlayer" should "interpret a phrase with Slurred" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Art(Slurred(Rational(2, 3))))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    PulsePlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(1, 6), 60, List()),
        MusicEvent(qn, AltoSax, 197, Rational(1, 6), 60, List()),
        MusicEvent(hn, AltoSax, 197, qn, 60, List())
      ), dhn)
    )
  }

  "PulsePlayer" should "interpret a phrase with many PhraseAttributes" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(Accent(Rational(2))), Art(Legato(Rational(7, 5))))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    PulsePlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(7, 20), 120, List()),
        MusicEvent(qn, AltoSax, 197, Rational(7, 20), 120, List()),
        MusicEvent(hn, AltoSax, 197, Rational(7, 20), 120, List())
      ), dhn)
    )
  }

  private def buildContext(
                            cVol: Int = 60,
                            cTimeSignature: TimeSignature = TimeSignature(NoPulse(), qn, 0)
                          ): Context[NoteWithAttributes] = {
    Context(
      0, PulsePlayer, AltoSax, 1, 125, cVol, (C, Major), cTimeSignature
    )
  }

}
