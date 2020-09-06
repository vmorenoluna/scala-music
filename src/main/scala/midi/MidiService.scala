package midi

import java.io.File

import javax.sound.midi._
import javax.sound.midi.ShortMessage._
import performance.MusicEvent
import performance.Performance.Performance

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

  // TODO must accept a list of performances, so to use multiple instruments on the different channels
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
  def play(performance: Performance): Unit = {
    init()
    performanceToMidiEvents(performance).map { sequence =>
      sequencer match {
        case Failure(exception) => handleError(ex = exception)
        case Success(seq) => {
          seq.open()
          seq.addMetaEventListener((metaMsg: MetaMessage) => {
            if (metaMsg.getType() == 0x2F) {
              seq.close();
            }
          })
          // seq.setTempoInBPM(m_masterTempo);
          seq.setSequence(sequence)
          seq.stop()
          seq.setTickPosition(0)
          seq.start();
          seq.close()
        }
      }
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
      // TODO handle program and control changes
      // TODO One track per Part
      // TODO Error handling

      val events: Seq[MidiEvent] =
        performance
          .map(eventToMidiEvents)
          .flatten

      val sequence: Sequence = new Sequence(Sequence.PPQ, Resolution)
      val track: Track = sequence.createTrack()
      for (ev <- events) {
        track.add(ev)
      }

      //        if (phrase.getInstrument() != NO_INSTRUMENT) instrument = phrase.getInstrument();
      //        evt = createProgramChangeEvent(currChannel, instrument, phraseTick);
      //        currTrack.add(evt);

      //          if (pitch == Note.REST) {
      //            phraseTick += note.getDuration() * m_ppqn * elementTempoRatio;
      //            continue;
      //          }

      //          long onTick = (long)(phraseTick);
      //           pan
      //          if (note.getPan() != lastPanPosition) {
      //            evt = createCChangeEvent(currChannel, 10, (int)(note.getPan()*127), onTick);
      //            currTrack.add(evt);
      //            lastPanPosition = note.getPan();
      //          }

      // add a meta event to indicate the end of the sequence.
      // add a bit of time for reverb tail to fade if not cycling.
      //    if (longestTime > 0.0 && longestTrack != null) {
      //      MetaMessage msg = new MetaMessage();
      //      byte [] data = new byte[0];
      //      msg.setMessage(StopType, data, 0);
      //      if (msCycle) {
      //        MidiEvent evt = new MidiEvent(msg, (long)longestTime);
      //        longestTrack.add(evt);
      //      } else {
      //        MidiEvent evt = new MidiEvent(msg, (long)(longestTime + 100)); //+ 100 if you want leave some space for reverb tail
      //        longestTrack.add(evt);
      //      }
      //    }

      sequence
    }

  /**
   * Convert a MusicEvent to a list of MidiEvents
   *
   * @param event
   * @return
   */
  private def eventToMidiEvents(event: MusicEvent): List[MidiEvent] = {
    // TODO handle program and control changes
    val ev1 = createNoteOnEvent(0, event.ePitch, event.eVol, event.eTime.longValue)
    val ev2 = createNoteOffEvent(0, event.ePitch, event.eVol, (event.eTime + event.eDur).longValue)
    List(ev1.get, ev2.get)
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
  def init(): Boolean =
    sequencer match {
      case Failure(ex) => handleError("Sequencer", ex)
      case _ => initSyntesizer()
    }

  /**
   * Teardown the Midi Service
   *
   * @return
   */
  def teardown(): Unit = {
    sequencer.map(_.close())
    synthesizer.map(_.close())
  }

  /**
   * Check that the synthesizer has been properly
   * created and opens it.
   *
   * @return
   */
  private def initSyntesizer(): Boolean = {
    synthesizer match {
      case Failure(ex) => handleError("Synthesizer", ex)
      case Success(value) =>
        Try(value.open()) match {
          case Failure(ex) => handleError("Synthesizer", ex)
          case _ => true
        }
    }
  }

  /**
   * Handle a Java exception by printing
   * the error on the console
   *
   * @param entity the midi entity that failed
   * @param ex     the exception
   * @return
   */
  private def handleError(entity: String = "", ex: Throwable): Boolean = {
    println(s"MidiSystem Unavailable: [$entity] ${ex.getMessage}")
    false
  }

}

