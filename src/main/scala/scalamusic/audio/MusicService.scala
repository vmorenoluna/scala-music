package scalamusic.audio

import scalamusic.music.Music
import scalamusic.music.MusicWithAttributes.{MusicWithAttributes, MusicWithAttributesOps, NoteWithAttributes}
import scalamusic.music.Types.Pitch
import scalamusic.performance.Context
import scalamusic.performance.Performance.perform

class MusicService(soundfontPath: String) {

  type Part = (Context[NoteWithAttributes], Music[Pitch])

  /**
   * Play the given parts. Each part consists of a
   * starting context and the scalamusic.music to play.
   *
   * @param parts a list of Part
   */
  def play(parts: List[Part]): Unit = {
    val performances = for {
      part <- parts
      musicWithAttributes: MusicWithAttributes = part._2.toMusicWithAttributes()
    } yield perform(part._1, musicWithAttributes)

    MidiService.play(performances, soundfontPath)
  }

  /**
   * Write the given parts to a MIDI file. Each part consists of a
   * starting context and the scalamusic.music to play.
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
