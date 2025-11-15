package scalamusic.performance

import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import scalamusic.core.InstrumentName.AltoSax
import scalamusic.core.Mode.Major
import scalamusic.core.Music.{dhn, dqn, en, hn, qn}
import scalamusic.core.MusicWithAttributes._
import scalamusic.core.Types.Duration
import scalamusic.core.Types.PitchClass.C
import scalamusic.core.{MusicWithAttributes => _, _}
import scalamusic.performance.players.FancyPlayer
import spire.math.Rational

class FancyPlayerSpec extends AnyFlatSpec with Matchers {

  "FancyPlayer" should "interpret a note" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val d: Duration = qn
    val n: NoteWithAttributes = ((C, 5), List(Volume(60), Fingering(25), Dynamics("ppp"), Params(List(1.5, 2.0))))

    FancyPlayer.playNote(c, d, n) should equal(
      List(MusicEvent(0, AltoSax, 197, qn, 60, 60, List(1.5, 2.0), 120))
    )
  }

  "FancyPlayer" should "interpret a phrase with Accent" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(Accent(Rational(2))))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    FancyPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 60, 120, List(), 120),
        MusicEvent(qn, AltoSax, 197, qn, 60, 120, List(), 120)
      ), hn)
    )
  }

  "FancyPlayer" should "interpret a phrase with StandardLoudness" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(StandardLoudness(StdLoudness.PPP)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    FancyPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 60, 40, List(), 120),
        MusicEvent(qn, AltoSax, 197, qn, 60, 40, List(), 120)
      ), hn)
    )
  }

  "FancyPlayer" should "interpret a phrase with Loudness" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(Loudness(40)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    FancyPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 60, 40, List(), 120),
        MusicEvent(qn, AltoSax, 197, qn, 60, 40, List(), 120)
      ), hn)
    )
  }

  "FancyPlayer" should "interpret a phrase with Crescendo" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(Crescendo(0.5)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    FancyPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 60, 60, List(), 120),
        MusicEvent(qn, AltoSax, 197, qn, 60, 70, List(), 120),
        MusicEvent(hn, AltoSax, 197, qn, 60, 80, List(), 120)
      ), dhn)
    )
  }

  "FancyPlayer" should "interpret a phrase with Diminuendo" in {
    val c: Context[NoteWithAttributes] = buildContext(cVol = 120)
    val pas: List[PhraseAttribute] = List(Dyn(Diminuendo(0.5)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    FancyPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 60, 120, List(), 120),
        MusicEvent(qn, AltoSax, 197, qn, 60, 100, List(), 120),
        MusicEvent(hn, AltoSax, 197, qn, 60, 80, List(), 120)
      ), dhn)
    )
  }

  "FancyPlayer" should "interpret a phrase with Ritardando" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Tmp(Ritardando(0.5)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    FancyPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(7, 24), 60, 60, List(), 120),
        MusicEvent(Rational(7, 24), AltoSax, 197, Rational(3, 8), 60, 60, List(), 120),
        MusicEvent(Rational(2, 3), AltoSax, 197, Rational(11, 24), 60, 60, List(), 120)
      ), 9*en)
    )
  }

  "FancyPlayer" should "interpret a phrase with Accelerando" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Tmp(Accelerando(0.5)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    FancyPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(5, 24), 60, 60, List(), 120),
        MusicEvent(Rational(5, 24), AltoSax, 197, en, 60, 60, List(), 120),
        MusicEvent(Rational(1, 3), AltoSax, 197, Rational(1, 24), 60, 60, List(), 120)
      ), 3*en)
    )
  }

  "FancyPlayer" should "interpret a phrase with Staccato" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Art(Staccato(Rational(2, 3))))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    FancyPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(1, 6), 60, 60, List(), 120),
        MusicEvent(qn, AltoSax, 197, Rational(1, 6), 60, 60, List(), 120),
        MusicEvent(hn, AltoSax, 197, Rational(1, 6), 60, 60, List(), 120)
      ), dhn)
    )
  }

  "FancyPlayer" should "interpret a phrase with Legato" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Art(Legato(Rational(7, 5))))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    FancyPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(7, 20), 60, 60, List(), 120),
        MusicEvent(qn, AltoSax, 197, Rational(7, 20), 60, 60, List(), 120),
        MusicEvent(hn, AltoSax, 197, Rational(7, 20), 60, 60, List(), 120)
      ), dhn)
    )
  }

  "FancyPlayer" should "interpret a phrase with Slurred" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Art(Slurred(Rational(2, 3))))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    FancyPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(1, 6), 60, 60, List(), 120),
        MusicEvent(qn, AltoSax, 197, Rational(1, 6), 60, 60, List(), 120),
        MusicEvent(hn, AltoSax, 197, qn, 60, 60, List(), 120)
      ), dhn)
    )
  }

  "FancyPlayer" should "interpret a phrase with many PhraseAttributes" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(Accent(Rational(2))), Art(Legato(Rational(7, 5))))
    val m: MusicWithAttributes =
      (Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5))) :+: Prim(Note(qn, (C, 5)))).toMusicWithAttributes()

    FancyPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, Rational(7, 20), 60, 120, List(), 120),
        MusicEvent(qn, AltoSax, 197, Rational(7, 20), 60, 120, List(), 120),
        MusicEvent(hn, AltoSax, 197, Rational(7, 20), 60, 120, List(), 120)
      ), dhn)
    )
  }

  private def buildContext(
                            cVol: Int = 60
                          ): Context[NoteWithAttributes] = {
    val timeSignature: TimeSignature = TimeSignature(NoPulse(), qn, 0)
    Context(
      0, FancyPlayer, AltoSax, 1, 125, 60, cVol, (C, Major), timeSignature, 120
    )
  }

}
