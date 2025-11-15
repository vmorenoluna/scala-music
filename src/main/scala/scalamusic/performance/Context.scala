package scalamusic.performance

import scalamusic.core.InstrumentName.InstrumentName
import scalamusic.core.Mode.Mode
import scalamusic.core.Types.PitchClass.PitchClass
import scalamusic.core.Types.{AbsPitch, Velocity, Volume}
import scalamusic.performance.Performance.{TickedDuration, TickedTime}
import scalamusic.performance.players.Player

/** The current context in a scalamusic.performance
  *
  * @param cTime   current time in ticks
  * @param cPlayer current player
  * @param cInst   current instrument
  * @param cDur    duration, in ticks, of one whole note
  * @param cPch    current pitch
  * @param cVel    current velocity 0-127
  * @param cVol    current volume 0-127
  * @param cKey    current key
  * @param cTimeSignature  current time signature
  * @param cTempo  current tempo in BPM (beats per minute)
  * @tparam A
  */
case class Context[A](
    cTime: TickedTime,
    cPlayer: Player[A],
    cInst: InstrumentName,
    cDur: TickedDuration,
    cPch: AbsPitch,
    cVel: Velocity,
    cVol: Volume,
    cKey: (PitchClass, Mode),
    cTimeSignature: TimeSignature,
    cTempo: Int
)
