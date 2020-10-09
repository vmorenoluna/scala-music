package scalamusic.audio

import java.io.File

import com.sun.media.sound.MidiUtils
import javax.sound.midi._
import javax.sound.midi.ShortMessage._
import scalamusic.performance.MusicEvent
import scalamusic.performance.Performance.Performance
import cats.implicits._
import scalamusic.music.InstrumentName.Percussion
import scala.util.{Failure, Success, Try}

/**
 * Object that provides midi integration
 */
object MidiService {

  private val pulsesPerQuarterNote = 96
  private val percussionChannel = 9

  /**
   * Store a scalamusic.music in a MIDI file
   *
   * @param performances a list containing the performances that make the scalamusic.music
   * @param pathName     path of the file
   */
  def writePerformance(performances: List[Performance], pathName: String): Unit =
    performancesToMidiEvents(performances) match {
      case Failure(exception) => handleError("performances", exception)
      case Success(sequence) =>
        val file = new File(pathName)
        MidiSystem.write(sequence, 1, file)
    }

  /**
   * Plays the scalamusic.performance via the JavaSound MIDI synthesizer
   *
   * @param performances a list containing the performances to play
   * @param soundfontPath the filesystem path of the soundfont to use
   * @return
   */
  def play(performances: List[Performance], soundfontPath: String): Unit =
    init(soundfontPath) match {
      case None => ()
      case Some(value) =>
        val sequencer = value._1
        val synthesizer = value._2
        sequencer.setTickPosition(0)
        sequencer.setTempoInBPM(184) // TODO set it in the context
        sequencer.addMetaEventListener((metaMsg: MetaMessage) => {
          if (metaMsg.getType() == MidiUtils.META_END_OF_TRACK_TYPE) {
            sequencer.close()
            synthesizer.close()
          }
        })
        performancesToMidiEvents(performances).map { sequence =>
          sequencer.setSequence(sequence)
          sequencer.start()
        }
    }

  /**
   * Converts a list of Performance into a MIDI Sequence.
   * Each scalamusic.performance is added to a different track of the sequence.
   *
   * @param performances the list of performances
   * @return Sequence to be played
   */
  def performancesToMidiEvents(performances: List[Performance]): Try[Sequence] =
    Try(new Sequence(Sequence.PPQ, pulsesPerQuarterNote))
      .map { sequence =>
        for (i <- performances.indices) {
          val events: Seq[MidiEvent] = performances(i).flatMap(eventToMidiEvents(i, _))
          val track: Track = sequence.createTrack()
          for (ev <- events) {
            track.add(ev)
          }
          // TODO let it be conditionally added
          // Add a meta event to indicate the end of the track.
          // The track will end one whole note after the ticked length of the track, so the sound is allowed to fade.
          val msg = new MetaMessage(MidiUtils.META_END_OF_TRACK_TYPE, Array[Byte](0), 0)
          val evt = new MidiEvent(msg, track.ticks + 4 * pulsesPerQuarterNote)
          track.add(evt)
        }
        sequence
      }

  /**
   * Convert a MusicEvent to a list of MidiEvents
   *
   * @param channel the channel to which this event belongs
   * @param event   the event to convert
   * @return the list of MidiEvent
   */
  private def eventToMidiEvents(channel: Int, event: MusicEvent): List[MidiEvent] = {
    val eventsList = event.eInst match {
      case Percussion =>
        val ev1 = createNoteOnEvent(percussionChannel, event.ePitch, event.eVol, event.eTime.longValue)
        val ev2 = createNoteOffEvent(percussionChannel, event.ePitch, event.eVol, (event.eTime + event.eDur).longValue)
        List(ev1, ev2).sequence
      case _ =>
        val ev1 = createProgramChangeEvent(channel, event.eInst.id, event.eTime.longValue)
        val ev2 = createNoteOnEvent(channel, event.ePitch, event.eVol, event.eTime.longValue)
        val ev3 = createNoteOffEvent(channel, event.ePitch, event.eVol, (event.eTime + event.eDur).longValue)
        List(ev1, ev2, ev3).sequence
    }
    eventsList match {
      case Failure(exception) =>
        handleError("MusicEvent", exception)
        List.empty
      case Success(events) =>
        events
    }
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

  /**
   * Init the Midi Service
   *
   * @param soundfontPath the filesystem path of the soundfont to use
   * @return true if the operation succeeded, false otherwise
   */
  private def init(soundfontPath: String): Option[(Sequencer, Synthesizer)] =
    Try {
      val sequencer: Sequencer = MidiSystem.getSequencer(false)
      sequencer.open()
      initSyntesizerWithSoundfont(soundfontPath)
        .flatMap(connectSeqToSynth(sequencer, _))
    } match {
      case Failure(ex) =>
        handleError("Init", ex)
        None
      case Success(value) => value
    }

  /**
   * Initialise the synthesizer
   *
   * @param soundfontPath the filesystem path of the soundfont to use
   * @return
   */
  private def initSyntesizerWithSoundfont(soundfontPath: String): Option[Synthesizer] =
    Try {
      val soundbank: Soundbank = MidiSystem.getSoundbank(new File(soundfontPath))
      val synthesizer: Synthesizer = MidiSystem.getSynthesizer()
      synthesizer.open()
      synthesizer.unloadAllInstruments(synthesizer.getDefaultSoundbank())
      synthesizer.loadAllInstruments(soundbank)
      synthesizer
    } match {
      case Failure(exception) =>
        handleError("Synthesizer", exception)
        None
      case Success(value) =>
        Some(value)
    }

  /**
   * Connect the sequencer to the synthesizer
   *
   * @param seq   the sequencer
   * @param synth the synthesizer
   * @return
   */
  private def connectSeqToSynth(seq: Sequencer, synth: Synthesizer): Option[(Sequencer, Synthesizer)] =
    Try {
      seq.getTransmitter().setReceiver(synth.getReceiver())
      (seq, synth)
    } match {
      case Failure(exception) =>
        handleError("load soundfonts", exception)
        None
      case Success(value) => Some(value)
    }

  /**
   * Handle a Java exception by printing
   * the error on the console
   *
   * @param context the context of the failure
   * @param ex      the exception
   * @return
   */
  private def handleError(context: String = "", ex: Throwable): Unit =
    println(s"Error: [$context] ${
      ex.getMessage
    }")

}

