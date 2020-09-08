package midi

import java.io.File

import com.sun.media.sound.MidiUtils
import javax.sound.midi._
import javax.sound.midi.ShortMessage._
import performance.MusicEvent
import performance.Performance.Performance
import cats.implicits._
import scala.util.{Failure, Success, Try}

/**
 * Object that provides midi integration
 */
object MidiService {

  val Resolution = 96
  private val sequencer: Try[Sequencer] = Try(MidiSystem.getSequencer())
  private val synthesizer: Try[Synthesizer] = Try(MidiSystem.getSynthesizer())

  //  val currentTempoBPM = 90
  //  val ticksPerSecond = Resolution * (currentTempoBPM / 60.0)
  //  val tickSize = 1.0 / ticksPerSecond

  // TODO must accept a list of performances, so to use multiple instruments on the different channels/tracks
  def writePerformance(performance: Performance, pathName: String): Unit = {
    val events: Seq[MidiEvent] =
      performance
        .map(eventToMidiEvents)
        .flatten

    val sequence: Sequence = new Sequence(Sequence.PPQ, Resolution)
    // TODO One track per Part
    val track: Track = sequence.createTrack()
    for (ev <- events)
      track.add(ev)

    val file = new File(pathName)
    MidiSystem.write(sequence, 1, file)
  }

  /**
   * Plays the performance via the JavaSound MIDI synthesizer
   *
   * @param performance the performance to play
   * @return
   */
  def play(performance: Performance): Unit =
    init() match {
      case None => ()
      case Some(value) =>
        performanceToMidiEvents(performance).map { sequence =>
          val seq = value._1
          val synth = value._2
          seq.addMetaEventListener((metaMsg: MetaMessage) => {
            if (metaMsg.getType() == MidiUtils.META_END_OF_TRACK_TYPE) {
              teardown()
            }
          })
          // seq.setTempoInBPM(m_masterTempo);
          seq.setSequence(sequence)
          seq.setTickPosition(0)
          //          seq.getTransmitter().setReceiver(synth.getReceiver())
          seq.start()
        }
    }

  /**
   * Converts a Performance into a MIDI Sequence
   *
   * @param performance the performance
   * @return Sequence to be played
   */
  private def performanceToMidiEvents(performance: Performance): Try[Sequence] =
    Try {
      // TODO One track per Part

      val events: Seq[MidiEvent] =
        performance
          .map(eventToMidiEvents)
          .flatten

      val sequence: Sequence = new Sequence(Sequence.PPQ, Resolution)
      val track: Track = sequence.createTrack()
      for (ev <- events) {
        track.add(ev)
      }

      // TODO let it be conditionally added
      // Add a meta event to indicate the end of the track. There is already one when a track is created but it ends too soon.
      // The track will end one whole note after the ticked length of the track, so the sound is allowed to fade.
      val msg = new MetaMessage(MidiUtils.META_END_OF_TRACK_TYPE, Array[Byte](0), 0)
      val evt = new MidiEvent(msg, track.ticks + 4 * Resolution)
      track.add(evt)

      sequence
    }

  /**
   * Convert a MusicEvent to a list of MidiEvents
   *
   * @param event the event to convert
   * @return
   */
  private def eventToMidiEvents(event: MusicEvent): List[MidiEvent] = {
    // TODO set the channel
    val ev1 = createProgramChangeEvent(0, event.eInst.id, event.eTime.longValue)
    val ev2 = createNoteOnEvent(0, event.ePitch, event.eVol, event.eTime.longValue)
    val ev3 = createNoteOffEvent(0, event.ePitch, event.eVol, (event.eTime + event.eDur).longValue)
    List(ev1, ev2, ev3).sequence match {
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

  //  /** Pulses per quarter note value */
  //  private short m_ppqn;
  //  /** The Synthesizer we are using */
  //  private Synthesizer m_synth;
  //  /** The Sequence we are using */
  //  private Sequence m_seq;
  //  /** The Sequencer we are using */
  //  private Sequencer m_sequencer;

  //  /**
  //   * Plays back the already computed sequencer via a MIDI synthesizer
  //   * @exception Exception
  //   */
  //  private void rePlay() {
  //    if (null == m_sequencer) {
  //      if (!initSynthesizer()) {
  //        return;
  //      }
  //    }
  //    if (update) {
  //      System.out.println("Updating playback sequence");
  //      //Sequence seq;
  //      try {
  //        m_seq = scoreToSeq(updateScore);
  //        if (null != m_seq) {
  //          try {
  //            m_sequencer.open();
  //          }
  //          catch (MidiUnavailableException e) {
  //            System.err.println("MIDI System Unavailable:" + e);
  //            return;
  //          }
  //          m_sequencer.setSequence(m_seq);
  //        }
  //      } catch (InvalidMidiDataException e) {
  //        System.err.println("MIDISynth updating sequence error:" + e);
  //        return;
  //      }
  //      update = false;
  //    }
  //    m_sequencer.setMicrosecondPosition(0l);
  //    m_sequencer.setTempoInBPM(m_masterTempo);
  //    m_sequencer.start();
  //  }
  //

  /**
   * Init the Midi Service
   *
   * @return true if the operation succeeded, false otherwise
   */
  private def init(): Option[(Sequencer, Synthesizer)] =
    sequencer match {
      case Failure(ex) =>
        handleError("Sequencer", ex)
        None
      case Success(seq) =>
        Try(seq.open()) match {
          case Failure(ex) =>
            handleError("Sequencer", ex)
            None
          case _ =>
            initSyntesizer()
              .map((seq, _))
        }
    }

  /**
   * Check that the synthesizer has been properly
   * created and opens it.
   *
   * @return
   */
  private def initSyntesizer(): Option[Synthesizer] = {
    synthesizer match {
      case Failure(ex) =>
        handleError("Synthesizer", ex)
        None
      case Success(synth) =>
        Try(synth.open()) match {
          case Failure(ex) =>
            handleError("Synthesizer", ex)
            None
          case _ => Some(synth)
        }
    }
  }

  /**
   * Teardown the Midi Service
   *
   * @return
   */
  private def teardown(): Unit = {
    sequencer.map(_.close())
    synthesizer.map(_.close())
  }

  /**
   * Handle a Java exception by printing
   * the error on the console
   *
   * @param entity the midi entity that failed
   * @param ex     the exception
   * @return
   */
  private def handleError(entity: String = "", ex: Throwable): Unit =
    println(s"Error: [$entity] ${ex.getMessage}")

}

