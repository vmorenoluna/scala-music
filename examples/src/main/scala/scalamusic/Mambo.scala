package examples

import scalamusic.audio.MusicService
import scalamusic.music.InstrumentName.{AcousticBass, AcousticGrandPiano, Flute}
import scalamusic.music.Music._
import scalamusic.music.MusicWithAttributes.NoteWithAttributes
import scalamusic.music.PercussionSound.{Claves, HiBongo, HighTimbale, LongGuiro, LowBongo, LowConga, LowTimbale, Maracas, MuteHiConga, ShortGuiro}
import scalamusic.music.Types.Pitch
import scalamusic.music.Types.PitchClass.C
import scalamusic.music.{Mode, Music}
import scalamusic.performance.players.DefaultPlayer
import scalamusic.performance.{Context, Metronome}

object Mambo extends App {

  val soundfontPath: String = "FluidR3_GM.sf2"
  val musicService: MusicService = new MusicService(soundfontPath)
  val repetitions: Int = 8
  val context: Context[NoteWithAttributes] = Context(0, DefaultPlayer, AcousticGrandPiano, Metronome.tickedWholeNote(96), 0, 127, (C, Mode.Major))

  val piano: Music[Pitch] = times(repetitions,
    (c(5, qn) :=: c(4, qn) :=: c(3, qn)) :+:
      (g(4, en) :=: e(4, en) :=: e(3, en) :=: g(3, en)) :+:
      (d(5, qn) :=: d(4, qn) :=: d(3, qn)) :+:
      (a(4, qn) :=: f(4, qn) :=: a(3, qn) :=: f(3, qn)) :+:
      (d(5, qn) :=: d(4, qn) :=: d(3, qn)) :+:
      (b(5, qn) :=: g(4, qn) :=: b(3, qn) :=: g(3, qn)) :+:
      (d(5, qn) :=: d(4, qn) :=: d(3, qn)) :+:
      (a(4, qn) :=: f(4, qn) :=: a(3, qn) :=: f(3, qn)) :+:
      (c(5, en) :=: c(4, en) :=: c(3, en))
  )

  val bass: Music[Pitch] = times(repetitions,
    c(2, dqn) :+: f(2, dqn) :+: g(2, qn + dqn) :+: f(2, dqn) :+: c(2, qn)
  )

  val claves = percussion(Claves, _)
  val clave: Music[Pitch] = times(repetitions,
    qnr :+: claves(qn) :+: claves(qn) :+: qnr :+: claves(qn) :+: enr :+: claves(en) :+: qnr :+: claves(qn)
  )

  val longGuiro = percussion(LongGuiro, en)
  val shortGuiro = percussion(ShortGuiro, en)
  val guiro: Music[Pitch] = times(repetitions,
    longGuiro :+: enr :+: shortGuiro :+: shortGuiro :+:
      longGuiro :+: enr :+: shortGuiro :+: shortGuiro :+:
      longGuiro :+: enr :+: shortGuiro :+: shortGuiro :+:
      longGuiro :+: enr :+: shortGuiro :+: shortGuiro
  )

  val highConga = percussion(MuteHiConga, en)
  val lowConga = percussion(LowConga, en)
  val congas: Music[Pitch] = times(repetitions,
    highConga :+: highConga :+: highConga :+: lowConga :+:
      lowConga :+: highConga :+: lowConga :+: lowConga :+:
      highConga :+: highConga :+: highConga :+: highConga :+:
      highConga :+: highConga :+: lowConga :+: lowConga
  ) :+: lowConga

  val highTimbale = percussion(HighTimbale, _)
  val lowTimbale = percussion(LowTimbale, en)
  val timbales: Music[Pitch] = times(repetitions,
    highTimbale(qn) :+: highTimbale(qn) :+: highTimbale(en) :+: highTimbale(en) :+: lowTimbale :+: highTimbale(en) :+:
      highTimbale(qn) :+: lowTimbale :+: highTimbale(en) :+: enr :+: highTimbale(en) :+: lowTimbale :+: highTimbale(en)
  ) :+: highTimbale(en)

  val maracas = percussion(Maracas, en)
  val maracasPart: Music[Pitch] = times(repetitions,
    maracas :+: maracas :+: maracas :+: maracas :+: maracas :+: maracas :+: maracas :+: maracas
  ) :+: maracas

  val highBongo = percussion(HiBongo, en)
  val lowBongo = percussion(LowBongo, en)
  val bongos: Music[Pitch] = times(repetitions,
    highBongo :+: lowBongo :+: lowBongo :+: lowBongo :+:
      highBongo :+: lowBongo :+: lowBongo :+: lowBongo :+:
      highBongo :+: lowBongo :+: lowBongo :+: lowBongo :+:
      highBongo :+: lowBongo :+: lowBongo :+: lowBongo
  ) :+: lowBongo

  val flute: Music[Pitch] =
    wnr :+: wnr :+: wnr :+: wnr :+:
      dhnr :+: enr :+: c(7, en) :+:
      enr :+: c(7, en) :+: enr :+: c(7, en) :+: enr :+: c(7, en) :+: c(7, dqn) :+:
      qnr :+: g(6, qn) :+: f(6, en) :+: enr :+: e(6, qn) :+:
      enr :+: e(6, en) :+: g(6, en) :+: e(7, en) :+: c(7, en) :+: e(7, en) :+: e(7, en) :+: c(7, qn) :+: qnr :+: hnr :+:
      qnr :+: e(6, en) :+: g(6, en) :+: e(7, en) :+: c(7, en) :+: e(7, en) :+: e(7, en) :+: c(7, qn) :+: qnr :+: hnr :+:
      qnr :+: e(6, en) :+: g(6, en) :+: e(7, en) :+: c(7, en) :+: e(7, en) :+: e(7, en) :+:
      c(7, en) :+: qnr :+: g(6, en) :+: f(6, en) :+: e(6, en) :+: enr :+: f(6, qn) :+: d(6, qn) :+: e(6, qn) :+: c(6, qn) :+:
      e(7, en) :+: c(7, en) :+: enr :+: g(6, en) :+: f(6, en) :+: e(6, en) :+: c(7, qn) :+: g(6, qn) :+:
      enr :+: e(6, en) :+: g(6, en) :+: e(7, en) :+: c(7, en) :+: e(7, en) :+: e(7, en) :+:
      c(7, en)

  val score: List[(Context[NoteWithAttributes], Music[Pitch])] =
    List(
      (context.copy(cVol = 75, cInst = Flute), flute),
      (context.copy(cVol = 60, cInst = AcousticGrandPiano), piano),
      (context.copy(cVol = 70, cInst = AcousticBass), bass),
      (context.copy(cVol = 90), clave),
      (context.copy(cVol = 60), guiro),
      (context.copy(cVol = 60), congas),
      (context.copy(cVol = 60), timbales),
      (context.copy(cVol = 60), maracasPart),
      (context.copy(cVol = 60), bongos)
    )

  musicService.play(score)
  musicService.write(score, "Mambo.mid")

}
