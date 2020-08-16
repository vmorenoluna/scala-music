package music

import music.Types.{Pitch, Volume => Vol}
import music.Music.mMap

/**
 * An object to transform a Music to a MusicWithAttributes.
 * The latter is the one that eventually gets translated to
 * a series of MusicEvents
 */
object MusicWithAttributes {

  type NoteWithAttributes = (Pitch, List[NoteAttribute])
  type MusicWithAttributes = Music[NoteWithAttributes]

  /**
   * Type class for the conversion from Music
   * to MusicWithNoteAttributes
   *
   * @tparam A
   */
  sealed trait ToMusicWithAttributesTypeClass[A] {
    def transform(m: Music[A]): MusicWithAttributes
  }

  implicit val ToMusicWithAttributesPitchInstance: ToMusicWithAttributesTypeClass[Pitch] = new ToMusicWithAttributesTypeClass[Pitch] {
    override def transform(m: Music[Pitch]): MusicWithAttributes =
      mMap[Pitch, NoteWithAttributes](p => (p, List.empty[NoteAttribute]), m)
  }
  implicit val ToMusicWithAttributesPitchVolumeInstance: ToMusicWithAttributesTypeClass[(Pitch, Vol)] = new ToMusicWithAttributesTypeClass[(Pitch, Vol)] {
    override def transform(m: Music[(Pitch, Vol)]): MusicWithAttributes =
      mMap[(Pitch, Vol), NoteWithAttributes](t => (t._1, List(Volume(t._2))), m)
  }
  implicit val ToMusicWithAttributesInstance: ToMusicWithAttributesTypeClass[NoteWithAttributes] = new ToMusicWithAttributesTypeClass[NoteWithAttributes] {
    override def transform(m: Music[NoteWithAttributes]): MusicWithAttributes = m
  }

  final implicit class MusicWithAttributesOps[A](private val self: Music[A]) {
    def toMusicWithAttributes()(implicit p: ToMusicWithAttributesTypeClass[A]): MusicWithAttributes = p.transform(self)
  }

}

