package performance

import music.InstrumentName.InstrumentName
import music.Mode.Mode
import music.Types.PitchClass.PitchClass
import music.Types.{AbsPitch, Duration, Volume}
import performance.Performance.{DurT, PTime}
import performance.players.Player

/**
 * The current context in a performance
 *
 * @param cTime   current time
 * @param cPlayer current player
 * @param cInst   current instrument
 * @param cDur    duration, in seconds, of one whole note
 * @param cPch    current pitch
 * @param cVol    current volume 0-127
 * @param cKey    current key
 * @tparam A
 */
case class Context[A](
                       cTime: PTime,
                       cPlayer: Player[A],
                       cInst: InstrumentName,
                       cDur: DurT,
                       cPch: AbsPitch,
                       cVol: Volume,
                       cKey: (PitchClass, Mode)
                     )