package scalamusic.performance

import scalamusic.core.InstrumentName.{AcousticGrandPiano, AltoSax}
import scalamusic.core.Mode.{Major, Minor}
import scalamusic.core.Music.{dhn, hn, qn, wn}
import scalamusic.core.MusicWithAttributes.{MusicWithAttributes, MusicWithAttributesOps, NoteWithAttributes}
import scalamusic.core.Types.{Duration, Pitch, Volume}
import scalamusic.core.{Accent, Art, CtrlTempo, Dyn, Instrument, KeySig, Modification, Note, Phrase, PhraseAttribute, Player, Prim, Rest, Staccato, Transpose}
import scalamusic.core.Types.PitchClass.C
import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import scalamusic.performance.Performance.{Performance, merge, perf, perform}
import scalamusic.performance.players.{DefaultPlayer, PlayersEnum}

class PerformanceSpec extends AnyFlatSpec with Matchers {

  val PulsesPerQuarterNote = 96

  "perform" should "perform a Music" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val m: MusicWithAttributes = (
      (Prim(Note(qn, ((C, 5), 60))) :+: Prim(Rest[(Pitch, Volume)](hn)) :+: Prim(Note(qn, ((C, 5), 50)))) :=:
        (Prim(Rest[(Pitch, Volume)](qn)) :+: Prim(Note(hn, ((C, 3), 40))) :+: Prim(Note(qn, ((C, 3), 30))))
      ).toMusicWithAttributes()

    perform(c, m) should equal(
      List(
        MusicEvent(0, AltoSax, 197, toTicks(qn), 60, 60, List()),
        MusicEvent(toTicks(qn), AltoSax, 173, toTicks(hn), 60, 40, List()),
        MusicEvent(toTicks(dhn), AltoSax, 173, toTicks(qn), 60, 30, List()),
        MusicEvent(toTicks(dhn), AltoSax, 197, toTicks(qn), 60, 50, List())
      )
    )
  }

  "perf" should "perform a Music" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val m: MusicWithAttributes = (
      (Prim(Note(qn, ((C, 5), 60))) :+: Prim(Rest[(Pitch, Volume)](hn)) :+: Prim(Note(qn, ((C, 5), 50)))) :=:
        (Prim(Note(hn, ((C, 3), 40))) :+: Prim(Note(hn, ((C, 3), 30))))
      ).toMusicWithAttributes()

    perf(c, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 173, toTicks(hn), 60, 40, List()),
        MusicEvent(0, AltoSax, 197, toTicks(qn), 60, 60, List()),
        MusicEvent(toTicks(hn), AltoSax, 173, toTicks(hn), 60, 30, List()),
        MusicEvent(toTicks(dhn), AltoSax, 197, toTicks(qn), 60, 50, List())
      ), toTicks(wn))
    )
  }

  "perf" should "properly handle a Tempo Modification while performing a Music " in {
    val c: Context[NoteWithAttributes] = buildContext()
    val m: MusicWithAttributes = (
      Modification(CtrlTempo(2), Prim(Note(hn, ((C, 5), 60)))) :+: Prim(Note(qn, ((C, 6), 80)))
      ).toMusicWithAttributes()

    perf(c, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, toTicks(qn), 60, 60, List()),
        MusicEvent(toTicks(qn), AltoSax, 209, toTicks(qn), 60, 80, List())
      ), toTicks(hn))
    )
  }

  "perf" should "properly handle a Transpose Modification while performing a Music " in {
    val c: Context[NoteWithAttributes] = buildContext()
    val m: MusicWithAttributes = (
      Modification(Transpose(12), Prim(Note(qn, ((C, 5), 60)))) :+: Prim(Note(qn, ((C, 6), 80)))
      ).toMusicWithAttributes()

    perf(c, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 209, toTicks(qn), 60, 60, List()),
        MusicEvent(toTicks(qn), AltoSax, 209, toTicks(qn), 60, 80, List())
      ), toTicks(hn))
    )
  }

  "perf" should "properly handle a Instrument Modification while performing a Music " in {
    val c: Context[NoteWithAttributes] = buildContext()
    val m: MusicWithAttributes = (
      Modification(Instrument(AcousticGrandPiano), Prim(Note(qn, ((C, 5), 60)))) :+: Prim(Note(qn, ((C, 6), 80)))
      ).toMusicWithAttributes()

    perf(c, m) should equal(
      (List(
        MusicEvent(0, AcousticGrandPiano, 197, toTicks(qn), 60, 60, List()),
        MusicEvent(toTicks(qn), AltoSax, 209, toTicks(qn), 60, 80, List())
      ), toTicks(hn))
    )
  }

  "perf" should "properly handle a Key Signature Modification while performing a Music " in {
    val c: Context[NoteWithAttributes] = buildContext()
    val m: MusicWithAttributes = (
      Modification(KeySig(C, Minor), Prim(Note(qn, ((C, 5), 60)))) :+: Prim(Note(qn, ((C, 6), 80)))
      ).toMusicWithAttributes()

    perf(c, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, toTicks(qn), 60, 60, List()),
        MusicEvent(toTicks(qn), AltoSax, 209, toTicks(qn), 60, 80, List())
      ), toTicks(hn))
    )
  }

  "perf" should "properly handle a Phrase Modification while performing a Music " in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(Accent(2)), Art(Staccato(1)))
    val m: MusicWithAttributes = (
      Modification(Phrase(pas), Prim(Note(qn, ((C, 5), 60)))) :+: Prim(Note(qn, ((C, 6), 80)))
      ).toMusicWithAttributes()

    perf(c, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, toTicks(qn), 60, 120, List()),
        MusicEvent(toTicks(qn), AltoSax, 209, toTicks(qn), 60, 80, List())
      ), toTicks(hn))
    )
  }

  "perf" should "properly handle a Player Modification while performing a Music " in {
    val c: Context[NoteWithAttributes] = buildContext()
    val m: MusicWithAttributes = (
      Modification(Player(PlayersEnum.DefaultPlayer), Prim(Note(qn, ((C, 5), 60)))) :+: Prim(Note(qn, ((C, 6), 80)))
      ).toMusicWithAttributes()

    perf(c, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, toTicks(qn), 60, 60, List()),
        MusicEvent(toTicks(qn), AltoSax, 209, toTicks(qn), 60, 80, List())
      ), toTicks(hn))
    )
  }

  "merge" should "merge two performances of equal length" in {
    val p1: Performance = List(
      MusicEvent(0, AltoSax, 75, 45, 60, 60, List.empty),
      MusicEvent(45, AltoSax, 75, 45, 60, 60, List.empty),
      MusicEvent(90, AltoSax, 75, 45, 60, 60, List.empty)
    )
    val p2: Performance = List(
      MusicEvent(5, AltoSax, 75, 20, 60, 60, List.empty),
      MusicEvent(25, AltoSax, 75, 45, 60, 60, List.empty),
      MusicEvent(70, AltoSax, 75, 45, 60, 60, List.empty)
    )

    merge(p1, p2) should equal(
      List(
        MusicEvent(0, AltoSax, 75, 45, 60, 60, List.empty),
        MusicEvent(5, AltoSax, 75, 20, 60, 60, List.empty),
        MusicEvent(25, AltoSax, 75, 45, 60, 60, List.empty),
        MusicEvent(45, AltoSax, 75, 45, 60, 60, List.empty),
        MusicEvent(70, AltoSax, 75, 45, 60, 60, List.empty),
        MusicEvent(90, AltoSax, 75, 45, 60, 60, List.empty)
      )
    )
  }

  "merge" should "merge two performances of different length" in {
    val p1: Performance = List(
      MusicEvent(0, AltoSax, 75, 45, 60, 60, List.empty),
      MusicEvent(45, AltoSax, 75, 45, 60, 60, List.empty),
    )
    val p2: Performance = List(
      MusicEvent(5, AltoSax, 75, 20, 60, 60, List.empty),
      MusicEvent(25, AltoSax, 75, 45, 60, 60, List.empty),
      MusicEvent(70, AltoSax, 75, 45, 60, 60, List.empty)
    )

    merge(p1, p2) should equal(
      List(
        MusicEvent(0, AltoSax, 75, 45, 60, 60, List.empty),
        MusicEvent(5, AltoSax, 75, 20, 60, 60, List.empty),
        MusicEvent(25, AltoSax, 75, 45, 60, 60, List.empty),
        MusicEvent(45, AltoSax, 75, 45, 60, 60, List.empty),
        MusicEvent(70, AltoSax, 75, 45, 60, 60, List.empty)
      )
    )

    merge(p2, p1) should equal(
      List(
        MusicEvent(0, AltoSax, 75, 45, 60, 60, List.empty),
        MusicEvent(5, AltoSax, 75, 20, 60, 60, List.empty),
        MusicEvent(25, AltoSax, 75, 45, 60, 60, List.empty),
        MusicEvent(45, AltoSax, 75, 45, 60, 60, List.empty),
        MusicEvent(70, AltoSax, 75, 45, 60, 60, List.empty)
      )
    )

  }

  private def buildContext(): Context[NoteWithAttributes] = {
    val timeSignature: TimeSignature = TimeSignature(NoPulse(), qn, 0)
    Context(
      0, DefaultPlayer, AltoSax, 4 * PulsesPerQuarterNote, 125, 60, 60, (C, Major), timeSignature
    )
  }

  private def toTicks(duration: Duration): Int =
    (duration * 4 * PulsesPerQuarterNote).intValue

}
