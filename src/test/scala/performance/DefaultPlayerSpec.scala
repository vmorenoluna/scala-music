package performance

import music.Music.{hn, qn}
import music.InstrumentName.AltoSax
import music.Mode.Major
import music.MusicWithAttributes._
import music.Types.Duration
import music.Types.PitchClass.C
import music.{Accent, Art, Dyn, Dynamics, Fingering, Legato, Note, Params, PhraseAttribute, Prim, Staccato, Volume}
import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import performance.players.DefaultPlayer

class DefaultPlayerSpec extends AnyFlatSpec with Matchers {

  "DefaultPlayer" should "interpret a note" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val d: Duration = qn
    val n: NoteWithAttributes = ((C, 5), List(Volume(60), Fingering(25), Dynamics("ppp"), Params(List(1.5, 2.0))))

    DefaultPlayer.playNote(c, d, n) should equal(
      List(MusicEvent(0, AltoSax, 197, qn, 60, List(1.5, 2.0)))
    )
  }

  "DefaultPlayer" should "interpret a phrase with Accent and Staccato" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(Accent(2)), Art(Staccato(1)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, ((C, 5), 60))) :+: Prim(Note(qn, ((C, 5), 30)))).toMusicWithAttributes()

    DefaultPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 120, List()),
        MusicEvent(qn, AltoSax, 197, qn, 120, List())
      ), hn)
    )
  }

  "DefaultPlayer" should "interpret a phrase with Accent and Legato" in {
    val c: Context[NoteWithAttributes] = buildContext()
    val pas: List[PhraseAttribute] = List(Dyn(Accent(2)), Art(Legato(1)))
    val m: MusicWithAttributes =
      (Prim(Note(qn, ((C, 5), 60))) :+: Prim(Note(qn, ((C, 5), 30)))).toMusicWithAttributes()

    DefaultPlayer.interpretPhrase(c, pas, m) should equal(
      (List(
        MusicEvent(0, AltoSax, 197, qn, 120, List()),
        MusicEvent(qn, AltoSax, 197, qn, 120, List())
      ), hn)
    )
  }

  private def buildContext(): Context[NoteWithAttributes] =
    Context(
      0, DefaultPlayer, AltoSax, 1, 125, 60, (C, Major)
    )

}
