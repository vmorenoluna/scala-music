package performance

import music.InstrumentName.InstrumentName
import music.Mode.Mode
import music.Types.PitchClass.PitchClass
import music.Types.{AbsPitch, Volume}
import performance.Performance.{TickedDuration, TickedTime}
import performance.players.Player

/**
 * The current context in a performance
 *
 * @param cTime   current time in ticks
 * @param cPlayer current player
 * @param cInst   current instrument
 * @param cDur    duration, in ticks, of one whole note
 * @param cPch    current pitch
 * @param cVol    current volume 0-127
 * @param cKey    current key
 * @tparam A
 */
case class Context[A](
                       cTime: TickedTime,
                       cPlayer: Player[A],
                       cInst: InstrumentName,
                       cDur: TickedDuration,
                       cPch: AbsPitch,
                       cVol: Volume,
                       cKey: (PitchClass, Mode)
                     )