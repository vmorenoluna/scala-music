package midi

import java.io.File

import javax.sound.midi._
import javax.sound.midi.ShortMessage._
import performance.MusicEvent
import performance.Performance.Performance

import scala.util.Try

/**
 * Object that provides midi integration
 */
object MidiService {

  val Resolution = 96
  val currentTempoInBeatsPerMinute = 90
  val ticksPerSecond = Resolution * (currentTempoInBeatsPerMinute / 60.0)
  val tickSize = 1.0 / ticksPerSecond

  def writePerformance(performance: Performance, pathName: String): Unit = {
    val events: Seq[MidiEvent] =
      performance
        .map(eventToMidiEvents(_))
        .flatMap(t => List(t._1, t._2))

    val sequence: Sequence = new Sequence(Sequence.PPQ, Resolution)
    val track: Track = sequence.createTrack()
    for (ev <- events)
      track.add(ev)

    val file = new File(pathName)
    MidiSystem.write(sequence, 1, file)
  }

  private def eventToMidiEvents(event: MusicEvent): (MidiEvent, MidiEvent) = {
    val ev1 = createNoteOnEvent(0, event.ePitch, event.eVol, event.eTime.longValue)
    val ev2 = createNoteOffEvent(0, event.ePitch, event.eVol, (event.eTime + event.eDur).longValue)
    (ev1.get, ev2.get)
  }

  /**
   * Create a Note On Event
   *
   * @param channel  is the channel to change
   * @param pitch    is the pitch of the note
   * @param velocity is the velocity of the note
   * @param tick     is the time this event occurs
   */
  private def createNoteOnEvent(channel: Int, pitch: Int, velocity: Int, tick: Long): Try[MidiEvent] =
    Try {
      val msg: ShortMessage = new ShortMessage(NOTE_ON, channel, pitch, velocity)
      new MidiEvent(msg, tick)
    }

  /**
   * Create a Note Off Event
   *
   * @param channel  is the channel to change
   * @param pitch    is the pitch of the note
   * @param velocity is the velocity of the note
   * @param tick     is the time this event occurs
   */
  private def createNoteOffEvent(channel: Int, pitch: Int, velocity: Int, tick: Long): Try[MidiEvent] =
    Try {
      val msg: ShortMessage = new ShortMessage(NOTE_OFF, channel, pitch, velocity)
      new MidiEvent(msg, tick)
    }

  /**
   * Create a Program Change Event
   *
   * @param channel is the channel to change
   * @param value   is the new value to use
   * @param tick    is the time this event occurs
   */
  private def createProgramChangeEvent(channel: Int, value: Int, tick: Long): Try[MidiEvent] =
    Try {
      val msg: ShortMessage = new ShortMessage(PROGRAM_CHANGE, channel, value, 0)
      new MidiEvent(msg, tick)
    }

  /**
   * Create a Control Change event
   *
   * @param channel    is the channel to use
   * @param controlNum is the control change number to use
   * @param value      is the value of the control change
   * @param tick       is the time this event occurs
   */
  private def createCChangeEvent(channel: Int, controlNum: Int, value: Int, tick: Long): Try[MidiEvent] =
    Try {
      val msg: ShortMessage = new ShortMessage(CONTROL_CHANGE, channel, controlNum, value)
      new MidiEvent(msg, tick)
    }

}

