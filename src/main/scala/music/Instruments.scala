package music

/**
 * An enumeration of instruments
 */
object InstrumentName extends Enumeration {
  type InstrumentName = Value
  val AcousticGrandPiano, BrightAcousticPiano, ElectricGrandPiano, HonkyTonkPiano,
  RhodesPiano, ChorusedPiano, Harpsichord, Clavinet, Celesta, Glockenspiel, MusicBox,
  Vibraphone, Marimba, Xylophone, TubularBells, Dulcimer, HammondOrgan, PercussiveOrgan,
  RockOrgan, ChurchOrgan, ReedOrgan, Accordion, Harmonica, TangoAccordion, AcousticGuitarNylon,
  AcousticGuitarSteel, ElectricGuitarJazz, ElectricGuitarClean, ElectricGuitarMuted,
  OverdrivenGuitar, DistortionGuitar, Guitarharmonics, AcousticBass, ElectricBassFingered,
  ElectricBassPicked, FretlessBass, SlapBass1, SlapBass2, SynthBass1, SynthBass2,
  Violin, Viola, Cello, Contrabass, TremoloStrings, PizzicatoStrings, OrchestralHarp,
  Timpani, StringEnsemble1, StringEnsemble2, SynthStrings1, SynthStrings2, ChoirAahs,
  VoiceOohs, SynthVoice, OrchestraHit, Trumpet, Trombone, Tuba, MutedTrumpet, FrenchHorn,
  BrassSection, SynthBrass1, SynthBrass2, SopranoSax, AltoSax, TenorSax, BaritoneSax,
  Oboe, Bassoon, EnglishHorn, Clarinet, Piccolo, Flute, Recorder, PanFlute, BlownBottle,
  Shakuhachi, Whistle, Ocarina, Lead1Square, Lead2Sawtooth, Lead3Calliope, Lead4Chiff,
  Lead5Charang, Lead6Voice, Lead7Fifths, Lead8BassLead, Pad1Newage, Pad2Warm, Pad3Polysynth,
  Pad4Choir, Pad5Bowed, Pad6Metallic, Pad7Halo, Pad8Sweep, FX1Train, FX2Soundtrack, FX3Crystal,
  FX4Atmosphere, FX5Brightness, FX6Goblins, FX7Echoes, FX8SciFi, Sitar, Banjo, Shamisen,
  Koto, Kalimba, Bagpipe, Fiddle, Shanai, TinkleBell, Agogo, SteelDrums, Woodblock, TaikoDrum,
  MelodicDrum, SynthDrum, ReverseCymbal, GuitarFretNoise, BreathNoise, Seashore, BirdTweet,
  TelephoneRing, Helicopter, Applause, Gunshot, Percussion, CustomInstrument = Value
}

/**
 * An enumeration of percussion sounds.
 * The order is the same of the MIDI standard, with
 * the first element corresponding to MIDI key 35 and
 * the last one to the MIDI key 82
 */
object PercussionSound extends Enumeration {
  val AcousticBassDrum = Value(0)
  val BassDrum1 = Value(1)
  val SideStick = Value(2)
  val AcousticSnare = Value(3)
  val HandClap = Value(4)
  val ElectricSnare = Value(5)
  val LowFloorTom = Value(6)
  val ClosedHiHat = Value(7)
  val HighFloorTom = Value(8)
  val PedalHiHat = Value(9)
  val LowTom = Value(10)
  val OpenHiHat = Value(11)
  val LowMidTom = Value(12)
  val HiMidTom = Value(13)
  val CrashCymbal1 = Value(14)
  val HighTom = Value(15)
  val RideCymbal1 = Value(16)
  val ChineseCymbal = Value(17)
  val RideBell = Value(18)
  val Tambourine = Value(19)
  val SplashCymbal = Value(20)
  val Cowbell = Value(21)
  val CrashCymbal2 = Value(22)
  val Vibraslap = Value(23)
  val RideCymbal2 = Value(24)
  val HiBongo = Value(25)
  val LowBongo = Value(26)
  val MuteHiConga = Value(27)
  val OpenHiConga = Value(28)
  val LowConga = Value(29)
  val HighTimbale = Value(30)
  val LowTimbale = Value(31)
  val HighAgogo = Value(32)
  val LowAgogo = Value(33)
  val Cabasa = Value(34)
  val Maracas = Value(35)
  val ShortWhistle = Value(36)
  val LongWhistle = Value(37)
  val ShortGuiro = Value(38)
  val LongGuiro = Value(39)
  val Claves = Value(40)
  val HiWoodBlock = Value(41)
  val LowWoodBlock = Value(42)
  val MuteCuica = Value(43)
  val OpenCuica = Value(44)
  val MuteTriangle = Value(45)
  val OpenTriangle = Value(46)
}