package scalamusic.performance

import scalamusic.music.MusicWithAttributes.NoteWithAttributes
import scalamusic.music._
import scalamusic.performance.players.Player.players
import scalamusic.performance.players.DefaultPlayer
import spire.math.Rational

object Performance {

  type Performance = List[MusicEvent]
  type TickedTime = Rational
  type TickedDuration = Rational

  /**
   * Perform a Music starting with an initial Context
   *
   * @param c the initial context
   * @param m the scalamusic.music to perform
   * @return  the scalamusic.performance
   */
  def perform(c: Context[NoteWithAttributes], m: Music[NoteWithAttributes]): Performance =
    perf(c, m)._1

  /**
   * Perform a Music starting with an initial Context
   *
   * @param c the initial context
   * @param m the scalamusic.music to perform
   * @return  a tuple composed by the scalamusic.performance and its ticked duration
   */
  def perf(c: Context[NoteWithAttributes], m: Music[NoteWithAttributes]): (Performance, TickedDuration) = m match {
    case Prim(Note(d, p)) => (c.cPlayer.playNote(c, d, p), d * c.cDur)
    case Prim(Rest(d)) => (List.empty, d * c.cDur)
    case :+:(m1, m2) => {
      val (pf1, d1) = perf(c, m1)
      val (pf2, d2) = perf(c.copy(cTime = c.cTime + d1), m2)
      (pf1 ++ pf2, d1 + d2)
    }
    case :=:(m1, m2) => {
      val (pf1, d1) = perf(c, m1)
      val (pf2, d2) = perf(c, m2)
      (merge(pf1, pf2), d1 max d2)
    }
    case Modification(CtrlTempo(r), m) => perf(c.copy(cDur = c.cDur / r), m)
    case Modification(Transpose(p), m) => perf(c.copy(cPch = c.cPch + p), m)
    case Modification(Instrument(i), m) => perf(c.copy(cInst = i), m)
    case Modification(KeySig(pc, mo), m) => perf(c.copy(cKey = (pc, mo)), m)
    case Modification(Phrase(pas), m) => c.cPlayer.interpretPhrase(c, pas, m)
    case Modification(Player(s), m) => perf(c.copy(cPlayer = players.getOrElse(s, DefaultPlayer)), m)
  }

  /**
   * Merge two performances
   *
   * @param p1
   * @param p2
   * @return
   */
  def merge(p1: Performance, p2: Performance): Performance = (p1, p2) match {
    case (Nil, es2) => es2
    case (es1, Nil) => es1
    case (a @ e1 :: es1, b @ e2 :: es2) =>
      if (e1.eTime < e2.eTime) e1 :: merge(es1, b)
      else e2 :: merge(a, es2)
  }

}
