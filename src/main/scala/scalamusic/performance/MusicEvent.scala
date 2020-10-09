package scalamusic.performance

import scalamusic.core.InstrumentName.InstrumentName
import scalamusic.core.Types.{AbsPitch, Volume}
import scalamusic.performance.Performance.{TickedDuration, TickedTime}

/**
 * A scalamusic.music event
 *
 * @param eTime   onset time in ticks
 * @param eInst   assigned instrument
 * @param ePitch  pitch value 0-127
 * @param eDur    note duration in ticks
 * @param eVol    volume value 0-127
 * @param eParams optional other parameters
 */
case class MusicEvent(
                       eTime: TickedTime,
                       eInst: InstrumentName,
                       ePitch: AbsPitch,
                       eDur: TickedDuration,
                       eVol: Volume,
                       eParams: List[Double]
                 )
