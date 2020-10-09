package scalamusic.music

import scalamusic.music.Music._
import scalamusic.music.Types.Pitch
import scalamusic.music.MusicWithAttributes._
import scalamusic.music.Types.PitchClass.{C, D}
import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class MusicWithAttributesSpec extends AnyFlatSpec with Matchers {

  "MusicWithAttributes" should "allow the conversion from a Music[Pitch]" in {
    val mPitch: Music[Pitch] = c(4, qn) :+: rest[Pitch](0) :+: d(4, qn)

    mPitch.toMusicWithAttributes should equal(
      Prim(Note(qn,((C,4),List[NoteAttribute]()))) :+: rest[(Pitch, List[NoteAttribute])](0) :+: Prim(Note(qn,((D,4),List[NoteAttribute]())))
    )
  }

  "MusicWithAttributes" should "allow the conversion from a Music[(Pitch, Volume)]" in {
    val mPitchVolume: Music[(Pitch, Types.Volume)] = Prim(Note(qn, ((C,4), 60))) :+: rest[(Pitch, Types.Volume)](0) :+: Prim(Note(qn, ((D,4), 60)))

    mPitchVolume.toMusicWithAttributes should equal(
      Prim(Note(qn,((C,4),List[NoteAttribute](Volume(60))))) :+: rest[(Pitch, List[NoteAttribute])](0) :+: Prim(Note(qn,((D,4),List[NoteAttribute](Volume(60)))))
    )
  }

  "MusicWithAttributes" should "preserve the identity" in {
    val mWithAttributes: Music[NoteWithAttributes] = Prim(Note(qn,((C,4),List[NoteAttribute](Volume(60))))) :+: rest[(Pitch, List[NoteAttribute])](0) :+: Prim(Note(qn,((D,4),List[NoteAttribute](Volume(60)))))

    mWithAttributes.toMusicWithAttributes should equal(
      Prim(Note(qn,((C,4),List[NoteAttribute](Volume(60))))) :+: rest[(Pitch, List[NoteAttribute])](0) :+: Prim(Note(qn,((D,4),List[NoteAttribute](Volume(60)))))
    )
  }

}
