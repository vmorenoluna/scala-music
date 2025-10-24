package scalamusic.examples

import scalamusic.audio.MusicService
import scalamusic.core.InstrumentName.{AcousticBass, AcousticGrandPiano, Flute}
import scalamusic.core.Music._
import scalamusic.core.MusicWithAttributes.NoteWithAttributes
import scalamusic.core.PercussionSound._
import scalamusic.core.Types.Pitch
import scalamusic.core.Types.PitchClass.C
import scalamusic.core.{Mode, Music}
import scalamusic.performance.players.DefaultPlayer
import scalamusic.performance.{Context, Meter, NoPulse, TimeSignature}

object Mambo extends App {

  val soundfontPath: String = "FluidR3_GM.sf2"
  val musicService: MusicService = new MusicService(soundfontPath)
  val repetitions: Int = 8
  val timeSignature: TimeSignature = TimeSignature(NoPulse(), qn, 0)
  val context: Context[NoteWithAttributes] = Context(0, DefaultPlayer, AcousticGrandPiano, Meter.tickedWholeNote(96), 0, 127, 127, (C, Mode.Major), timeSignature)

  val piano: Music[Pitch] = (
    (c(5, qn) :=: c(4, qn) :=: c(3, qn)) :+:
      (g(4, en) :=: e(4, en) :=: e(3, en) :=: g(3, en)) :+:
      (d(5, qn) :=: d(4, qn) :=: d(3, qn)) :+:
      (a(4, qn) :=: f(4, qn) :=: a(3, qn) :=: f(3, qn)) :+:
      (d(5, qn) :=: d(4, qn) :=: d(3, qn)) :+:
      (b(5, qn) :=: g(4, qn) :=: b(3, qn) :=: g(3, qn)) :+:
      (d(5, qn) :=: d(4, qn) :=: d(3, qn)) :+:
      (a(4, qn) :=: f(4, qn) :=: a(3, qn) :=: f(3, qn)) :+:
      (c(5, en) :=: c(4, en) :=: c(3, en))
  ).times(repetitions)

  val bass: Music[Pitch] = (
    c(2, dqn) :+: f(2, dqn) :+: g(2, qn + dqn) :+: f(2, dqn) :+: c(2, qn)
  ).times(repetitions)

  val claves = percussion(Claves, _)
  val clave: Music[Pitch] = (
    qnr :+: claves(qn) :+: claves(qn) :+: qnr :+: claves(qn) :+: enr :+: claves(en) :+: qnr :+: claves(qn)
  ).times(repetitions)

  val longGuiro = percussion(LongGuiro, en)
  val shortGuiro = percussion(ShortGuiro, en)
  val guiro: Music[Pitch] = (
    longGuiro :+: enr :+: shortGuiro :+: shortGuiro :+:
      longGuiro :+: enr :+: shortGuiro :+: shortGuiro :+:
      longGuiro :+: enr :+: shortGuiro :+: shortGuiro :+:
      longGuiro :+: enr :+: shortGuiro :+: shortGuiro
  ).times(repetitions)

  val highConga = percussion(MuteHiConga, en)
  val lowConga = percussion(LowConga, en)
  val congas: Music[Pitch] = (
    highConga :+: highConga :+: highConga :+: lowConga :+:
      lowConga :+: highConga :+: lowConga :+: lowConga :+:
      highConga :+: highConga :+: highConga :+: highConga :+:
      highConga :+: highConga :+: lowConga :+: lowConga
  ).times(repetitions):+: lowConga

  val highTimbale = percussion(HighTimbale, _)
  val lowTimbale = percussion(LowTimbale, en)
  val timbales: Music[Pitch] = (
    highTimbale(qn) :+: highTimbale(qn) :+: highTimbale(en) :+: highTimbale(en) :+: lowTimbale :+: highTimbale(en) :+:
      highTimbale(qn) :+: lowTimbale :+: highTimbale(en) :+: enr :+: highTimbale(en) :+: lowTimbale :+: highTimbale(en)
  ).times(repetitions) :+: highTimbale(en)

  val maracas = percussion(Maracas, en)
  val maracasPart: Music[Pitch] = (
    maracas :+: maracas :+: maracas :+: maracas :+: maracas :+: maracas :+: maracas :+: maracas
  ).times(repetitions) :+: maracas

  val highBongo = percussion(HiBongo, en)
  val lowBongo = percussion(LowBongo, en)
  val bongos: Music[Pitch] = (
    highBongo :+: lowBongo :+: lowBongo :+: lowBongo :+:
      highBongo :+: lowBongo :+: lowBongo :+: lowBongo :+:
      highBongo :+: lowBongo :+: lowBongo :+: lowBongo :+:
      highBongo :+: lowBongo :+: lowBongo :+: lowBongo
  ).times(repetitions) :+: lowBongo

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
