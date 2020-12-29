package scalamusic.performance.players

import scalamusic.core.MusicWithAttributes.NoteWithAttributes
import scalamusic.core._
import scalamusic.core.Types.{Duration, absPitch}
import scalamusic.performance.{Context, MusicEvent, Accent => AccentEnum}
import scalamusic.performance.Performance.{Performance, TickedDuration, perf}
import spire.math.Rational

/**
 * An enumeration of Players
 */
object PlayersEnum extends Enumeration {
  type PlayersEnum = Value
  val DefaultPlayer = Value
  val FancyPlayer = Value
  val PulsePlayer = Value
}

/**
 * A player that "knows" about differences with respect to scalamusic.performance and notation.
 *
 * @tparam A
 */
trait Player[A] {
  /**
   * Interpret a note
   *
   * @param c current context
   * @param d duration
   * @param a the note to interpret
   * @return a scalamusic.performance made of a single event
   */
  def playNote(c: Context[A], d: Duration, a: A): Performance

  /**
   * Interpret a phrase
   *
   * @param c   current context
   * @param pas phrase attributes
   * @param m   the scalamusic.music to interpret
   * @return a scalamusic.performance
   */
  def interpretPhrase(c: Context[A], pas: List[PhraseAttribute], m: Music[A]): (Performance, TickedDuration)
}

object Player {
  val players = Map(
    PlayersEnum.DefaultPlayer -> DefaultPlayer,
    PlayersEnum.FancyPlayer -> FancyPlayer
  ).withDefaultValue(DefaultPlayer)
}

object DefaultPlayer extends Player[NoteWithAttributes] {

  override def playNote(c: Context[NoteWithAttributes], d: Duration, n: NoteWithAttributes): Performance = {
    val initEv = MusicEvent(
      eTime = c.cTime,
      eInst = c.cInst,
      ePitch = absPitch(n._1) + c.cPch,
      eDur = d * c.cDur,
      eVol = c.cVol,
      eParams = List.empty
    )
    List(n._2.foldRight(initEv)(nasHandler(c)))
  }

  private def nasHandler(c: Context[NoteWithAttributes])(na: NoteAttribute, ev: MusicEvent): MusicEvent =
    (c, na, ev) match {
      case (_, Volume(v), ev) => ev.copy(eVol = v)
      case (_, Params(pms), ev) => ev.copy(eParams = pms)
      case (_, _, ev) => ev
    }

  override def interpretPhrase(c: Context[NoteWithAttributes], pas: List[PhraseAttribute], m: Music[NoteWithAttributes]): (Performance, TickedDuration) = {
    val (pf, dur) = perf(c, m)
    (pas.foldRight(pf)(pasHandler), dur)
  }

  /**
   * Apply phrase attributes to a Performance by using
   * the numeric arguments as a "scaling" factor.
   *
   * @param pa
   * @param p
   * @return
   */
  private def pasHandler(pa: PhraseAttribute, p: Performance): Performance =
    pa match {
      case Dyn(Accent(x)) => p.map(e => e.copy(eVol = (x * p.head.eVol).round.intValue))
      case Art(Staccato(x)) => p.map(e => e.copy(eDur = x * p.head.eDur))
      case Art(Legato(x)) => p.map(e => e.copy(eDur = x * p.head.eDur))
      case _ => p
    }

}

object FancyPlayer extends Player[NoteWithAttributes] {

  override def playNote(c: Context[NoteWithAttributes], d: Duration, n: NoteWithAttributes): Performance =
    DefaultPlayer.playNote(c, d, n)

  override def interpretPhrase(c: Context[NoteWithAttributes], pas: List[PhraseAttribute], m: Music[NoteWithAttributes]): (Performance, TickedDuration) = {
    pas match {
      case Nil => {
        val perfResult = perf(c, m)
        perfResult
      }
      case ::(pa, pas) => {
        val pfd@(pf, dur) = interpretPhrase(c, pas, m)

        val loud: StdLoudness.Value => (Performance, TickedDuration) = l => {
          val loudValue = l match {
            case StdLoudness.PPP  => 40
            case StdLoudness.PP   => 50
            case StdLoudness.P    => 60
            case StdLoudness.MP   => 70
            case StdLoudness.SF   => 80
            case StdLoudness.MF   => 90
            case StdLoudness.NF   => 100
            case StdLoudness.FF   => 110
            case StdLoudness.FFF  => 120
          }
          interpretPhrase(c, Dyn(Loudness(loudValue)) :: pas, m)
        }

        val stretch: Rational => (Performance, TickedDuration) = x => {
          val t0 = pf.head.eTime
          val r = x / dur
          val upd: MusicEvent => MusicEvent = e => {
            val dt = e.eTime - t0
            val t1 = (1 + dt*r) * dt + t0
            val d1 = (1 + (2*dt + e.eDur) * r) * e.eDur
            e.copy(eTime = t1, eDur = d1)
          }
          (pf.map(upd), (1 + x) * dur)
        }

        val inflate: Rational => (Performance, TickedDuration) = x => {
          val t0 = pf.head.eTime
          val r = x / dur
          val upd: MusicEvent => MusicEvent = e => {
            val dt = e.eTime - t0
            val vol = (1 + dt*r) * e.eVol
            e.copy(eVol = vol.round.intValue)
          }
          (pf.map(upd), dur)
        }

        pa match {
          case Dyn(Accent(x))       => (pf.map(e => e.copy(eVol = (x * e.eVol).round.intValue)), dur)
          case Dyn(StandardLoudness(l))  => loud(l)
          case Dyn(Loudness(x))     => interpretPhrase(c.copy(cVol = x.round.intValue), pas, m)
          case Dyn(Crescendo(x))    => inflate(x)
          case Dyn(Diminuendo(x))   => inflate(-x)
          case Tmp(Ritardando(x))   => stretch(x)
          case Tmp(Accelerando(x))  => stretch(-x)
          case Art(Staccato(x))     => (pf.map(e => e.copy(eDur = x * e.eDur)), dur)
          case Art(Legato(x))       => (pf.map(e => e.copy(eDur = x * e.eDur)), dur)
          case Art(Slurred(x))      => {
            val lastStartTime = pf.foldRight(Rational.zero)((e ,t) => e.eTime max Rational(t))
            val setDur: MusicEvent => MusicEvent = e => if (e.eTime < lastStartTime) e.copy(eDur = x * e.eDur) else e
            (pf.map(setDur), dur)
          }
          case Art(_)               => pfd
          case Orn(_)               => pfd
        }

      }
    }
  }

}

object PulsePlayer extends Player[NoteWithAttributes] {

  override def playNote(c: Context[NoteWithAttributes], d: Duration, n: NoteWithAttributes): Performance = {
    val pulsifiedVolume: Types.Volume = {
      val accent = c.cTimeSignature.pulse.calculateAccent(c.cTime, c.cTimeSignature.getTickedBeat(), c.cTimeSignature.startTime)
      accent match {
        case AccentEnum.NO_ACCENT => c.cVol
        case AccentEnum.LIGHT_ACCENT => c.cVol + 7
        case AccentEnum.HEAVY_ACCENT => c.cVol + 14
      }
    }
    val initEv = MusicEvent(
      eTime = c.cTime,
      eInst = c.cInst,
      ePitch = absPitch(n._1) + c.cPch,
      eDur = d * c.cDur,
      eVol = pulsifiedVolume ,
      eParams = List.empty
    )
    List(n._2.foldRight(initEv)(nasHandler(c)))
  }

  private def nasHandler(c: Context[NoteWithAttributes])(na: NoteAttribute, ev: MusicEvent): MusicEvent =
    (c, na, ev) match {
      case (_, Volume(v), ev) => ev.copy(eVol = v)
      case (_, Params(pms), ev) => ev.copy(eParams = pms)
      case (_, _, ev) => ev
    }

  override def interpretPhrase(c: Context[NoteWithAttributes], pas: List[PhraseAttribute], m: Music[NoteWithAttributes]): (Performance, TickedDuration) = {
    FancyPlayer.interpretPhrase(c, pas, m)
  }

}