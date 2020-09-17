package performance

import music.InstrumentName.InstrumentName
import music.Types.{AbsPitch, Volume}
import performance.Performance.{TickedDuration, TickedTime}

/**
 * A music event
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
