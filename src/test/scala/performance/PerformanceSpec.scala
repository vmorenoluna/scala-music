package performance

import music.InstrumentName.AltoSax
import music.Mode.Major
import music.Music.{dhn, hn, qn, wn}
import music.MusicWithAttributes.{MusicWithAttributes, MusicWithAttributesOps, NoteWithAttributes}
import music.{CtrlTempo, Modification, Note, Prim}
import music.Types.PitchClass.C
import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import performance.Performance.{Performance, merge, perf}
import performance.players.DefaultPlayer

class PerformanceSpec extends AnyFlatSpec with Matchers {

  "perf" should "perform a Music" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val m: MusicWithAttributes = (
      (Prim(Note(qn, ((C, 5), 60))) :+: Prim(Note(hn, ((C, 6), 80))) :+: Prim(Note(qn, ((C, 5), 50)))) :=:
        (Prim(Note(hn, ((C, 3), 40))) :+: Prim(Note(hn, ((C, 3), 30))))
      ).toMusicWithAttributes()

    perf(c, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 173, hn, 40, List()),
        MusicEvent(0, AltoSax, 197, qn, 60, List()),
        MusicEvent(qn, AltoSax, 209, hn, 80, List()),
        MusicEvent(hn, AltoSax, 173, hn, 30, List()),
        MusicEvent(dhn, AltoSax, 197, qn, 50, List())
      ), wn)
    )
  }

  "perf" should "properly handle a Tempo Modification while performing a Music " in {
    val c: Context[NoteWithAttributes] = buildContext()
    val m: MusicWithAttributes = (
      Modification(CtrlTempo(2), Prim(Note(hn, ((C, 5), 60)))) :+: Prim(Note(qn, ((C, 6), 80)))
      ).toMusicWithAttributes()

    perf(c, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 60, List()),
        MusicEvent(qn, AltoSax, 209, qn, 80, List())
      ), hn)
    )
  }

  "merge" should "merge two performances of equal length" in {
    val p1: Performance = List(
      MusicEvent(0, AltoSax, 75, 45, 60, List.empty),
      MusicEvent(45, AltoSax, 75, 45, 60, List.empty),
      MusicEvent(90, AltoSax, 75, 45, 60, List.empty)
    )
    val p2: Performance = List(
      MusicEvent(5, AltoSax, 75, 20, 60, List.empty),
      MusicEvent(25, AltoSax, 75, 45, 60, List.empty),
      MusicEvent(70, AltoSax, 75, 45, 60, List.empty)
    )

    merge(p1, p2) should equal(
      List(
        MusicEvent(0, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(5, AltoSax, 75, 20, 60, List.empty),
        MusicEvent(25, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(45, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(70, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(90, AltoSax, 75, 45, 60, List.empty)
      )
    )
  }

  "merge" should "merge two performances of different length" in {
    val p1: Performance = List(
      MusicEvent(0, AltoSax, 75, 45, 60, List.empty),
      MusicEvent(45, AltoSax, 75, 45, 60, List.empty),
    )
    val p2: Performance = List(
      MusicEvent(5, AltoSax, 75, 20, 60, List.empty),
      MusicEvent(25, AltoSax, 75, 45, 60, List.empty),
      MusicEvent(70, AltoSax, 75, 45, 60, List.empty)
    )

    merge(p1, p2) should equal(
      List(
        MusicEvent(0, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(5, AltoSax, 75, 20, 60, List.empty),
        MusicEvent(25, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(45, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(70, AltoSax, 75, 45, 60, List.empty)
      )
    )

    merge(p2, p1) should equal(
      List(
        MusicEvent(0, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(5, AltoSax, 75, 20, 60, List.empty),
        MusicEvent(25, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(45, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(70, AltoSax, 75, 45, 60, List.empty)
      )
    )

  }

  private def buildContext(): Context[NoteWithAttributes] =
    Context(
      0, DefaultPlayer, AltoSax, 1, 125, 60, (C, Major)
    )

}
