package scalamusic.audio

import cats.implicits._
import com.sun.media.sound.MidiUtils
import scalamusic.core.InstrumentName.Percussion
import scalamusic.performance.MusicEvent
import scalamusic.performance.Performance.Performance
import spire.math.Rational

import java.io.File
import javax.sound.midi.ShortMessage._
import javax.sound.midi._
import scala.util.{Failure, Success, Try}

/** Object that provides midi integration
  */
object MidiService {

  private val pulsesPerQuarterNote = 96

  /** Store a scalamusic.music in a MIDI file
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

  /** Plays the scalamusic.performance via the JavaSound MIDI synthesizer
    *
    * @param performances a list containing the performances to play
    * @param soundfontPath the filesystem path of the soundfont to use
    * @return
    */
  def play(performances: List[Performance], soundfontPath: String): Unit =
    init(soundfontPath) match {
      case None => ()
      case Some(value) =>
        val sequencer   = value._1
        val synthesizer = value._2

        // Use a flag to track when playback is complete
        @volatile var isPlaying = true

        sequencer.addMetaEventListener((metaMsg: MetaMessage) => {
          if (metaMsg.getType() == MidiUtils.META_END_OF_TRACK_TYPE) {
            sequencer.close()
            synthesizer.close()
            isPlaying = false
          }
        })
        performancesToMidiEvents(performances).map { sequence =>
          sequencer.setSequence(sequence)
          sequencer.setTickPosition(0)
          sequencer.start()

          // Block until playback is complete
          while (isPlaying) {
            Thread.sleep(100)
          }
        }
    }

  /** Converts a list of Performance into a MIDI Sequence.
    * Each scalamusic.performance is added to a different track of the sequence.
    *
    * @param performances the list of performances
    * @return Sequence to be played
    */
  def performancesToMidiEvents(performances: List[Performance]): Try[Sequence] =
    Try(new Sequence(Sequence.PPQ, pulsesPerQuarterNote))
      .map { sequence =>
        // Collect all tempo changes across all performances
        val tempoChanges = collectTempoChanges(performances)

        for (i <- performances.indices) {
          val events: Seq[MidiEvent] = performances(i).flatMap(eventToMidiEvents(i, _))
          val track: Track           = sequence.createTrack()

          // Add tempo changes to the first track only (MIDI convention)
          if (i == 0) {
            for ((tick, tempo) <- tempoChanges) {
              createTempoChangeEvent(tempo, tick).foreach(track.add)
            }
          }

          for (ev <- events) {
            track.add(ev)
          }
          // Add a meta event to indicate the end of the track.
          // The track will end one whole note after the ticked length of the track, so the sound is allowed to fade.
          val msg = new MetaMessage(MidiUtils.META_END_OF_TRACK_TYPE, Array[Byte](0), 0)
          val evt = new MidiEvent(msg, track.ticks + 4 * pulsesPerQuarterNote)
          track.add(evt)
        }
        sequence
      }

  /** Convert a MusicEvent to a list of MidiEvents.
    * Sets the initial volume using Main Channel Volume (CC 7) and then update the subsequent
    * dynamic changes using Expression (CC 11), both controlled by event.eVol
    *
    * @param channel the channel to which this event belongs
    * @param event   the event to convert
    * @return the list of MidiEvent
    */
  private def eventToMidiEvents(channel: Int, event: MusicEvent): List[MidiEvent] = {

    val MIDI_PERCUSSION_CHANNEL            = 9
    val MIDI_VOLUME_CONTROLLER_CHANNEL     = 7
    val MIDI_EXPRESSION_CONTROLLER_CHANNEL = 11

    val (midiChannel, programChangeOpt) = event.eInst match {
      case Percussion => (MIDI_PERCUSSION_CHANNEL, None)
      case _          => (channel, Some(createProgramChangeEvent(channel, event.eInst.id, event.eTime.longValue)))
    }

    // Dynamic Expression (Time >= 0): Always set CC 11 using eVol
    // (This is the dynamic value updated by Loudness/Dynamics)
    val ev_expr =
      createControlChangeEvent(midiChannel, MIDI_EXPRESSION_CONTROLLER_CHANNEL, event.eVol, event.eTime.longValue)

    val ev_note_on  = createNoteOnEvent(midiChannel, event.ePitch, event.eVel, event.eTime.longValue)
    val ev_note_off = createNoteOffEvent(midiChannel, event.ePitch, event.eVel, (event.eTime + event.eDur).longValue)

    val eventsList = if (event.eTime == Rational.zero) {
      // Initial Setup (Time 0): Set both CC 7 and CC 11
      // Set CC 7 (Main Volume) to the starting volume
      val ev_cc7 =
        createControlChangeEvent(midiChannel, MIDI_VOLUME_CONTROLLER_CHANNEL, event.eVol, event.eTime.longValue)
      // the order ensures the MIDI messages are sent in the optimal sequence for reliable sound rendering,
      // while correctly mapping the musical properties:
      // - event.eInst.id => Program Change (Sets the instrument, non-percussion only)
      // - event.eVol => CC 7 (Main Volume) (Initial track mix level, Time 0 only)
      // - event.eVol => CC 11 (Expression) (Dynamic updates from Loudness)
      // - event.eVel => Note-On Velocity (Individual note dynamics/articulations)
      programChangeOpt match {
        case Some(ev_prog) => List(ev_prog, ev_cc7, ev_expr, ev_note_on, ev_note_off).sequence // order matters!
        case None          => List(ev_cc7, ev_expr, ev_note_on, ev_note_off).sequence          // order matters!
      }
    } else {
      programChangeOpt match {
        case Some(ev_prog) => List(ev_prog, ev_expr, ev_note_on, ev_note_off).sequence // order matters!
        case None          => List(ev_expr, ev_note_on, ev_note_off).sequence          // order matters!
      }
    }
    eventsList match {
      case Failure(exception) =>
        handleError("MusicEvent", exception)
        List.empty
      case Success(events) =>
        events
    }
  }

  /** Create a Note On Event
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

  /** Create a Note Off Event
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

  /** Create a Program Change Event
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

  /** Create a Control Change event
    *
    * @param channel    is the channel to use
    * @param controlNum is the control change number to use
    * @param value      is the value of the control change
    * @param tick       is the time this event occurs
    */
  private def createControlChangeEvent(channel: Int, controlNum: Int, value: Int, tick: Long): Try[MidiEvent] =
    Try {
      val msg: ShortMessage = new ShortMessage(CONTROL_CHANGE, channel, controlNum, value)
      new MidiEvent(msg, tick)
    }

  /** Create a Tempo Change meta event
    *
    * @param bpm  the tempo in beats per minute
    * @param tick the time this event occurs
    * @return the MidiEvent containing the tempo change
    */
  private def createTempoChangeEvent(bpm: Int, tick: Long): Try[MidiEvent] =
    Try {
      // MIDI tempo is specified in microseconds per quarter note
      val microsecondsPerQuarterNote = (60000000.0 / bpm).toInt

      // Convert to 3-byte big-endian format
      val data = Array[Byte](
        ((microsecondsPerQuarterNote >> 16) & 0xFF).toByte,
        ((microsecondsPerQuarterNote >> 8) & 0xFF).toByte,
        (microsecondsPerQuarterNote & 0xFF).toByte
      )

      val msg = new MetaMessage(0x51, data, 3) // 0x51 is the MIDI meta event type for tempo
      new MidiEvent(msg, tick)
    }

  /** Collect all tempo changes from performances
    *
    * @param performances the list of performances
    * @return a sorted list of (tick, tempo) pairs containing only actual tempo changes
    */
  private def collectTempoChanges(performances: List[Performance]): List[(Long, Int)] = {
    // Collect all (tick, tempo) pairs from all events
    val allTempos = for {
      performance <- performances
      event <- performance
    } yield (event.eTime.longValue, event.eTempo)

    // Sort by tick
    val sortedTempos = allTempos.sortBy(_._1)

    // Keep only tempo changes (where tempo differs from previous)
    val tempoChanges = scala.collection.mutable.ListBuffer[(Long, Int)]()
    var lastTempo: Option[Int] = None

    for ((tick, tempo) <- sortedTempos) {
      lastTempo match {
        case None =>
          // First tempo event
          tempoChanges += ((tick, tempo))
          lastTempo = Some(tempo)
        case Some(prevTempo) if prevTempo != tempo =>
          // Tempo changed
          tempoChanges += ((tick, tempo))
          lastTempo = Some(tempo)
        case _ =>
        // Same tempo, skip
      }
    }

    tempoChanges.toList.distinct
  }

  /** Init the Midi Service
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

  /** Initialise the synthesizer
    *
    * @param soundfontPath the filesystem path of the soundfont to use
    * @return
    */
  private def initSyntesizerWithSoundfont(soundfontPath: String): Option[Synthesizer] =
    Try {
      val soundbank: Soundbank     = MidiSystem.getSoundbank(new File(soundfontPath))
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

  /** Connect the sequencer to the synthesizer
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

  /** Handle a Java exception by printing
    * the error on the console
    *
    * @param context the context of the failure
    * @param ex      the exception
    * @return
    */
  private def handleError(context: String = "", ex: Throwable): Unit =
    println(s"Error: [$context] ${ex.getMessage}")

}
