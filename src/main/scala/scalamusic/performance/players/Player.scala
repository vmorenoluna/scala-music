package scalamusic.performance.players

import scalamusic.core.MusicWithAttributes.NoteWithAttributes
import scalamusic.core.{Accent, Art, Dyn, Legato, Music, NoteAttribute, Params, PhraseAttribute, Staccato, Volume}
import scalamusic.core.Types.{Duration, absPitch}
import scalamusic.performance.{Context, MusicEvent}
import scalamusic.performance.Performance.{TickedDuration, Performance, perf}

/**
 * An enumeration of Players
 */
object PlayersEnum extends Enumeration {
  type PlayersEnum = Value
  val DefaultPlayer = Value
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
    PlayersEnum.DefaultPlayer -> DefaultPlayer
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
      case Dyn(Accent(x)) => p.map(e => e.copy(eVol = x.intValue * p.head.eVol))
      case Art(Staccato(x)) => p.map(e => e.copy(eDur = x.intValue * p.head.eDur))
      case Art(Legato(x)) => p.map(e => e.copy(eDur = x.intValue * p.head.eDur))
      case _ => p
    }

}
