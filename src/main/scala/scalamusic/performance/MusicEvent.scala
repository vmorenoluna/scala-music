package scalamusic.performance

import scalamusic.core.InstrumentName.InstrumentName
import scalamusic.core.Types.{AbsPitch, Velocity, Volume}
import scalamusic.performance.Performance.{TickedDuration, TickedTime}

/** A scalamusic.music event
  *
  * @param eTime   onset time in ticks
  * @param eInst   assigned instrument
  * @param ePitch  pitch value 0-127
  * @param eDur    note duration in ticks
  * @param eVol    volume value 0-127
  * @param eParams optional other parameters
  * @param eTempo  tempo in BPM (beats per minute)
  */
case class MusicEvent(
    eTime: TickedTime,
    eInst: InstrumentName,
    ePitch: AbsPitch,
    eDur: TickedDuration,
    eVel: Velocity,
    eVol: Volume,
    eParams: List[Double],
    eTempo: Int
)
