package scalamusic.performance.players

import scalamusic.core.MusicWithAttributes.NoteWithAttributes
import scalamusic.core.Types.{Duration, absPitch}
import scalamusic.core._
import scalamusic.performance.Performance.{Performance, TickedDuration, perf}
import scalamusic.performance.{Context, MusicEvent, players}
import spire.math.Rational

object ComprehensivePlayer extends players.Player[NoteWithAttributes] {

  private val DEFAULT_DURATION_FACTOR: Rational = Rational(7, 8) // 0.875
  // Maximum MIDI velocity value used as baseline for absolute dynamics
  private val MAX_MIDI_VELOCITY: Int = 127
  // Staccato: Play for 1/4 or 1/3 of written duration
  private val STACCATO_DURATION: Rational = Rational(1, 4)
  // Legato: Play slightly longer than written duration for overlap
  private val LEGATO_DURATION: Rational = Rational(11, 10)
  // Tenuto: Play for full written duration (no rest)
  private val TENUTO_DURATION: Rational = Rational(1, 1)
  // Marcato: Needs a velocity increase and a duration decrease
  private val MARCATO_VELOCITY: Rational = Rational(5, 4) // 1.25 (25% louder)
  private val MARCATO_DURATION: Rational = Rational(1, 2) // half duration

  /** Interpret a note
    *
    * @param c current context
    * @param d duration
    * @param n the note to interpret
    * @return a scalamusic.performance made of a single event
    */
  override def playNote(c: Context[NoteWithAttributes], d: Duration, n: NoteWithAttributes): Performance = {
    // 1. Calculate Initial Velocity: Use the Context's current volume (cVol)
    val initialVel = c.cVol
    // 2. Calculate Initial Effective Duration: Apply the default factor
    val initialEffectiveDur = d * DEFAULT_DURATION_FACTOR

    // 3. Create the MusicEvent
    val event = MusicEvent(
      eTime = c.cTime,
      eInst = c.cInst,
      ePitch = absPitch(n._1) + c.cPch, // Pitch from the Note
      eDur = initialEffectiveDur,       // Effective duration (will be post-processed by Articulations)
      eVel = initialVel,                // Initial velocity (will be post-processed by Dynamics/Accents)
      eVol = c.cVol,
      eParams = List.empty
    )

    // A Performance is a list of MusicEvents
    List(event)
  }

  /** Interpret a phrase
    *
    * @param c   current context
    * @param pas phrase attributes
    * @param m   the music to interpret
    * @return a scalamusic.performance
    */
  override def interpretPhrase(
      c: Context[NoteWithAttributes],
      pas: List[PhraseAttribute],
      m: Music[NoteWithAttributes]
  ): (Performance, TickedDuration) =
    pas match {
      case Nil =>
        val perfResult = perf(c, m)
        perfResult
      case ::(pa, pas) =>
        val pfd @ (pf, dur) = interpretPhrase(c, pas, m)

        val loud: StdLoudness.Value => (Performance, TickedDuration) = l => {
          val loudValue = l match {
            case StdLoudness.PPP => 40
            case StdLoudness.PP  => 50
            case StdLoudness.P   => 60
            case StdLoudness.MP  => 70
            case StdLoudness.SF  => 80
            case StdLoudness.MF  => 90
            case StdLoudness.NF  => 100
            case StdLoudness.FF  => 110
            case StdLoudness.FFF => 120
          }
          interpretPhrase(c, Dyn(Loudness(loudValue)) :: pas, m)
        }

        val stretch: Rational => (Performance, TickedDuration) = x => {
          val t0 = pf.head.eTime
          val r  = x / dur
          val upd: MusicEvent => MusicEvent = e => {
            val dt = e.eTime - t0
            val t1 = (1 + dt * r) * dt + t0
            val d1 = (1 + (2 * dt + e.eDur) * r) * e.eDur
            e.copy(eTime = t1, eDur = d1)
          }
          (pf.map(upd), (1 + x) * dur)
        }

        val inflate: Rational => (Performance, TickedDuration) = x => {
          val t0 = pf.head.eTime
          val r  = x / dur
          val upd: MusicEvent => MusicEvent = e => {
            val dt  = e.eTime - t0
            val vol = (1 + dt * r) * e.eVol
            e.copy(eVol = vol.round.intValue)
          }
          (pf.map(upd), dur)
        }

        pa match {
          case Dyn(Accent(x))           => (pf.map(e => e.copy(eVel = (x * e.eVel).round.intValue)), dur)
          case Dyn(StandardLoudness(l)) => loud(l)
          case Dyn(Loudness(x))         => interpretPhrase(c.copy(cVol = x.round.intValue), pas, m)
          case Dyn(Crescendo(x))        => inflate(x)
          case Dyn(Diminuendo(x))       => inflate(-x)
          case Tmp(Ritardando(x))       => stretch(x)
          case Tmp(Accelerando(x))      => stretch(-x)
          case Art(Staccato(_))         => (pf.map(e => e.copy(eDur = STACCATO_DURATION * e.eDur)), dur)
          case Art(Legato(_))           => (pf.map(e => e.copy(eDur = LEGATO_DURATION * e.eDur)), dur)
          case Art(Slurred(x)) =>
            val lastStartTime                    = pf.foldRight(Rational.zero)((e, t) => e.eTime max Rational(t))
            val setDur: MusicEvent => MusicEvent = e => if (e.eTime < lastStartTime) e.copy(eDur = x * e.eDur) else e
            (pf.map(setDur), dur)
          case Art(Tenuto()) => (pf.map(e => e.copy(eDur = TENUTO_DURATION * e.eDur)), dur)
          case Art(Marcato()) =>
            (
              pf.map(e => e.copy(eDur = MARCATO_DURATION * e.eDur, eVel = (MARCATO_VELOCITY * e.eVel).round.intValue)),
              dur
            )
          case Art(PerformancePreset(vFactor, dFactor)) =>
            (
              pf.map(e =>
                e.copy(
                  eDur = dFactor * e.eDur,
                  // Absolute dynamic: set velocity based on max MIDI velocity
                  eVel = (vFactor * MAX_MIDI_VELOCITY).round.intValue,
                  // Absolute dynamic: set volume based on max MIDI velocity
                  eVol = (vFactor * MAX_MIDI_VELOCITY).round.intValue
                )
              ),
              dur
            )
          case Art(_) => pfd
          case Orn(_) => pfd
        }

    }

}
