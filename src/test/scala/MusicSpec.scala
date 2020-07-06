import music.Music._
import music.Types._
import music.InstrumentName.Violin
import music.Types.PitchClass._
import music._
import spire.math.Rational

class MusicSpec extends UnitSpec {

  "line" should "create a sequential Music from a list of notes" in {
    val notes: List[Prim[Pitch]] = List(c(4, qn), d(4, qn), e(4, qn), fs(4, qn), gs(4, qn))

    line(notes) should equal(
      c(4, qn) :+: d(4, qn) :+: e(4, qn) :+: fs(4, qn) :+: gs(4, qn) :+: rest(0)
    )
  }

  "chord" should "create a parallel Music from a list of notes" in {
    val notes: List[Music[Pitch]] = List(c(4, qn), d(4, qn), e(4, qn), fs(4, qn), gs(4, qn))

    chord(notes) should equal(
      c(4, qn) :=: d(4, qn) :=: e(4, qn) :=: fs(4, qn) :=: gs(4, qn) :=: rest(0)
    )
  }

  "maxPitch" should "return the highest pitch from a list of pitches" in {
    val pitches: List[Pitch] = List((C, 4), (D, 2), (E, 7), (F, 5))

    maxPitch(pitches) should equal((E, 7))
  }

  "maxAbsPitch" should "return the highest absolute pitch from a list of absolute pitches" in {
    val pitches: List[AbsPitch] = List(50, 68, 72, 59)

    maxAbsPitch(pitches) should equal(72)
  }

  "minAbsPitch" should "return the lowest absolute pitch from a list of absolute pitches" in {
    val pitches: List[AbsPitch] = List(50, 68, 72, 59)

    minAbsPitch(pitches) should equal(50)
  }

  "fuse" should "fuse a list of Duration with a list of Prims with unspecified Duration" in {
    val durations: List[Duration] = List(qn, en, sn, qn, wn, qn)
    val notes: List[Duration => Prim[Pitch]] = List(c(4, _), d(4, _), e(4, _), rest, fs(4, _), g(4, _))

    fuse(durations, notes) should equal(
      List(c(4, qn), d(4, en), e(4, sn), rest(qn), fs(4, wn), g(4, qn))
    )
  }

  "times" should "repeat a music the specified amount of times" in {
    val m: Music[Pitch] = c(4, qn) :+: d(4, qn) :+: e(4, qn)
    val n: Int = 2

    times(n, m) should equal(
      (c(4, qn) :+: d(4, qn) :+: e(4, qn)) :+: ((c(4, qn) :+: d(4, qn) :+: e(4, qn)) :+: rest(0))
    )
  }

  "addDuration" should "add a duration to a list of notes" in {
    val notes: List[Duration => Prim[Pitch]] = List(c(4, _), d(4, _), e(4, _))
    val duration: Duration = qn

    addDuration(duration, notes) should equal(
      c(4, qn) :+: d(4, qn) :+: e(4, qn) :+: rest(0)
    )
  }

  "graceNote" should "add to a note a grace note that is higher than the main note by a given step" in {
    val note: Prim[Pitch] = e(4, qn)

    graceNote(ws, note) should equal(
      fs(4, qn / 8) :+: e(4, 7 * qn / 8)
    )
  }

  "graceNoteFraction" should "add to a note a grace note that will be higher than the main note by a given step and will last for the given main note's time fraction" in {
    val note: Prim[Pitch] = e(4, qn)
    val fraction: Rational = Rational(1) / 8

    graceNoteFraction(ws, fraction, note) should equal(
      fs(4, qn / 8) :+: e(4, 7 * qn / 8)
    )
  }

  "graceNoteDownbeat" should "add to a note a grace note so that the downbeat of the main note is unchanged" in {
    val note1: Prim[Pitch] = c(4, qn)
    val note2: Prim[Pitch] = e(4, qn)
    val fraction: Rational = Rational(1) / 8

    graceNoteDownbeat(ws, fraction, note1, note2) should equal(
      c(4, 7 * qn / 8) :+: fs(4, qn / 8) :+: e(4, qn)
    )
  }

  "offset" should "prepend a rest to the music" in {
    val duration = wn
    val music = c(4, qn)

    offset(duration, music) should equal(
      rest[Pitch](duration) :+: music
    )
  }

  "lineToList" should "create a list of notes from a sequential Music made with line" in {
    val music = c(4, qn) :+: d(4, qn) :+: e(4, qn) :+: fs(4, qn) :+: gs(4, qn) :+: rest(0)

    lineToList(music) should equal(
      List(c(4, qn), d(4, qn), e(4, qn), fs(4, qn), gs(4, qn))
    )
  }

  "invert" should "invert a line" in {
    val music = b(4, qn) :+: f(5, qn) :+: g(4, qn) :+: c(5, qn)

    invert(music) should equal(
      b(4, qn) :+: f(4, qn) :+: ds(5, qn) :+: as(4, qn) :+: rest(0)
    )
  }

  "retro" should "reverse a Music" in {
    val m1 = b(4, qn) :+: f(5, qn) :+: g(4, qn) :+: c(5, qn)
    val m2 = Modification(Instrument(Violin), g(5, en)) :+: as(4, qn) :+: d(5, qn)
    val music: Music[Pitch] = m1 :=: m2

    retro(music) should equal(
      (((c(5, qn) :+: g(4, qn)) :+: f(5, qn)) :+: b(4, qn)) :=:
        (rest[Pitch](dqn) :+: (d(5, qn) :+: as(4, qn)) :+: Modification(Instrument(Violin), g(5, en)))
    )
  }

  "pitches" should "convert a line music to a list of pitches by applying a function to the original pitches in the Music" in {
    val music: Music[Pitch] = c(4, qn) :+: d(4, qn) :+: rest[Pitch](qn) :+: b(4, qn) :+: rest[Pitch](hn)
    val function: Pitch => Pitch = p => p.transpose(ws)

    pitches(music, function) should equal(
      List((D, 4), (E, 4), (Cs, 5))
    )
  }

  "retroPitches" should "reverse the pitches in a line while maintaining the durations order" in {
    val music = b(4, en) :+: c(5, qn) :+: cs(4, hn) :+: df(5, wn)

    retroPitches(music) should equal(
      df(5, en) :+: cs(4, qn) :+: c(5, hn) :+: b(4, wn) :+: rest(0)
    )
  }

  "transM" should "transpose a Music by a given absolute pitch" in {
    val m1 = b(4, qn) :+: f(5, qn) :+: rest[Pitch](0) :+: c(5, qn)
    val m2 = Modification(Instrument(Violin), e(5, en)) :+: as(4, qn) :+: d(5, qn)
    val music: Music[Pitch] = m1 :=: m2

    transM(2, music) should equal(
      (cs(5, qn) :+: g(5, qn) :+: rest[Pitch](0) :+: d(5, qn)) :=:
        (Modification(Instrument(Violin), fs(5, en)) :+: c(5, qn) :+: e(5, qn))
    )
  }

  "duration" should "calculate the duration of a Music" in {
    val m1 = b(4, qn) :+: f(5, qn) :+: Modification(Tempo(2), g(4, hn)) :+: c(5, qn)
    val m2 = Modification(Instrument(Violin), g(5, en)) :+: as(4, qn) :+: d(5, qn)
    val music: Music[Pitch] = m1 :=: m2

    duration(music) should equal(wn)
  }

  "cut" should "cut the initial specified duration from a Music" in {
    val m1 = b(4, qn) :+: Modification(Tempo(2), g(4, hn)) :+: c(5, qn)
    val m2 = Modification(Instrument(Violin), g(5, en)) :+: as(4, qn) :+: d(5, qn)
    val music: Music[Pitch] = m1 :=: m2

    cut(dqn, music) should equal(
      (b(4, qn) :+: Modification(Tempo(2), g(4, qn)) :+: rest(0)) :=:
        (Modification(Instrument(Violin), g(5, en)) :+: as(4, qn) :+: rest(0))
    )
  }

  "remove" should "remove the initial specified duration from a Music" in {
    val m1 = b(4, qn) :+: Modification(Tempo(2), g(4, hn)) :+: c(5, qn)
    val m2 = Modification(Instrument(Violin), g(5, en)) :+: as(4, qn) :+: d(5, qn)
    val music: Music[Pitch] = m1 :=: m2

    remove(dqn, music) should equal(
      (b(4, 0) :+: Modification(Tempo(2), g(4, qn)) :+: c(5, qn)) :=:
        (Modification(Instrument(Violin), g(5, 0)) :+: as(4, 0) :+: d(5, qn))
    )
  }

  "removeZeros" should "remove all notes and rests with zero duration from a Music" in {
    val m1 = b(4, 0) :+: Modification(Tempo(2), g(4, qn)) :+: c(5, qn)
    val m2 = Modification(Instrument(Violin), g(5, 0)) :+: as(4, 0) :+: d(5, qn)
    val music: Music[Pitch] = m1 :=: m2

    removeZeros(music) should equal(
      (Modification(Tempo(2), g(4, qn)) :+: c(5, qn)) :=:
        (Modification(Instrument(Violin), g(5, 0)) :+: d(5, qn))
    )
  }

  "trill" should "add a trill note to a given note" in {
    val note: Music[Pitch] = Modification(Tempo(2), c(4, hn))
    val interval: Step = ws
    val duration: Duration = en

    trill(interval, duration, note) should equal(
      Modification(Tempo(2), c(4, qn) :+: d(4, qn))
    )
  }

  "trillOtherNote" should "add a trill note to a given note so that the result starts on the trill note" in {
    val note: Music[Pitch] = Modification(Tempo(2), c(4, hn))
    val interval: Step = ws
    val duration: Duration = en

    trillOtherNote(interval, duration, note) should equal(
      Modification(Transpose(2), Modification(Tempo(2), c(4, qn) :+: as(3, qn)))
    )
  }

  "trilln" should "add a trill note to a given note" in {
    val note: Music[Pitch] = Modification(Tempo(2), c(4, hn))
    val interval: Step = ws
    val subdivisions: Int = 4

    trilln(interval, subdivisions, note) should equal(
      Modification(Tempo(2), c(4, en) :+: d(4, en) :+: c(4, en) :+: d(4, en))
    )
  }

  "trillnOtherNote" should "add a trill note to a given note with the result starting on the trill note" in {
    val note: Music[Pitch] = Modification(Tempo(2), c(4, hn))
    val interval: Step = ws
    val subdivisions: Int = 4

    trillnOtherNote(interval, subdivisions, note) should equal(
      Modification(Transpose(2), Modification(Tempo(2), c(4, en) :+: as(3, en) :+: c(4, en) :+: as(3, en)))
    )
  }

  "roll" should "define a roll" in {
    val note: Music[Pitch] = Modification(Tempo(2), c(4, hn))
    val duration: Duration = en

    roll(duration, note) should equal(
      Modification(Tempo(2), c(4, qn) :+: c(4, qn))
    )
  }

  "rolln" should "define a roll" in {
    val note: Music[Pitch] = Modification(Tempo(2), c(4, hn))
    val interval: Step = ws
    val subdivisions: Int = 4

    rolln(subdivisions, note) should equal(
      Modification(Tempo(2), c(4, en) :+: c(4, en) :+: c(4, en) :+: c(4, en))
    )
  }

  "percussion" should "convert a PercussionSound to a note" in {
    val duration: Duration = en
    val percussionSound = PercussionSound.Cowbell

    percussion(percussionSound, duration) should equal(
      Modification(Instrument(InstrumentName.Percussion), Music.note(duration, pitch(56)))
    )
  }

  "pMap" should "map the Primitive type" in {
    val note = Note(qn, (C, 4))
    val rest = Rest(qn)
    val f: Pitch => Pitch = pc => (pc._1, pc._2 + 1)

    pMap(f, note) should equal(Note(qn, (C, 5)))
    pMap(f, rest) should equal(rest)
  }

  "mMap" should "map the Music type" in {
    val music = (c(4, qn) :+: Modification(Instrument(Violin), d(4, qn))) :=: (rest[Pitch](qn) :+: c(4, qn))
    val f: Pitch => Pitch = pc => (pc._1, pc._2 + 1)

    mMap(f, music) should equal(
      (c(5, qn) :+: Modification(Instrument(Violin), d(5, qn))) :=: (rest[Pitch](qn) :+: c(5, qn))
    )
  }

  "addVolume" should "convert a Music[Pitch] to a Music[(Pitch, Volume)]" in {
    val music = c(4, qn) :+: d(4, qn)
    val volume = 5

    addVolume(volume, music) should equal(
      Prim(Note(qn, ((C,4),5))) :+: Prim(Note(qn, ((D,4),5)))
    )
  }

  "scaleVolume" should "scale the volume of each note in a music by a given factor" in {
    val music = Prim(Note(qn, ((C,4),8))) :+: Prim(Note(qn, ((D,4),8)))

    scaleVolume(Rational(1,2), music) should equal(
      Prim(Note(qn, ((C,4), 4))) :+: Prim(Note(qn, ((D,4), 4)))
    )
  }

  "mFold" should "fold the Music type" in {
    val f: Primitive[Pitch] => Music[Pitch] = p => Prim(p)
    val music = (c(4, qn) :+: Modification(Instrument(Violin), d(4, qn))) :=: (rest[Pitch](qn) :+: c(4, qn))

    mFold(f)(:+:[Pitch])(:=:[Pitch])(Modification[Pitch])(music) should equal (music)
  }

}