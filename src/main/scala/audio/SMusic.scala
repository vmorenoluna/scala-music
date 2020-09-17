package audio

import music.Music
import music.MusicWithAttributes.{MusicWithAttributes, MusicWithAttributesOps, NoteWithAttributes}
import music.Types.Pitch
import performance.Context
import performance.Performance.perform

object SMusic {

  type Part = (Context[NoteWithAttributes], Music[Pitch])

  /**
   * Play the given parts. Each part consists of a
   * starting context and the music to play.
   *
   * @param parts a list of Part
   */
  def play(parts: List[Part]): Unit = {
    val performances = for {
      part <- parts
      musicWithAttributes: MusicWithAttributes = part._2.toMusicWithAttributes()
    } yield perform(part._1, musicWithAttributes)

    MidiService.play(performances)
  }

  /**
   * Write the given parts to a MIDI file. Each part consists of a
   * starting context and the music to play.
   *
   * @param parts    a list of Part
   * @param pathName path of the file
   */
  def write(parts: List[Part], pathName: String): Unit = {
    val performances = for {
      part <- parts
      musicWithAttributes: MusicWithAttributes = part._2.toMusicWithAttributes()
    } yield perform(part._1, musicWithAttributes)

    MidiService.writePerformance(performances, s"$pathName")
  }

}
