import music.Music._
import music.Types._
import music.InstrumentName.Violin
import music.Types.PitchClass._
import music._
import spire.math.Rational

class MusicSpec extends UnitSpec {

  "line" should "create a sequential Music from a list of notes" in {
    val notes: List[Music[Pitch]] = List(c(4, qn), d(4, qn), e(4, qn), fs(4, qn), gs(4, qn))

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
    val notes: List[Duration => Music[Pitch]] = List(c(4, _), d(4, _), e(4, _), rest(_), fs(4, _), g(4, _))

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
    val notes: List[Duration => Music[Pitch]] = List(c(4, _), d(4, _), e(4, _))
    val duration: Duration = qn

    addDuration(duration, notes) should equal(
      c(4, qn) :+: d(4, qn) :+: e(4, qn) :+: rest(0)
    )
  }

  "graceNote" should "add to a note a grace note that is higher than the main note by a given step" in {
    val note: Music[Pitch] = e(4, qn)

    graceNote(ws, note) should equal(
      fs(4, qn / 8) :+: e(4, 7 * qn / 8)
    )
  }

  "graceNoteFraction" should "add to a note a grace note that will be higher than the main note by a given step and will last for the given main note's time fraction" in {
    val note: Music[Pitch] = e(4, qn)
    val fraction: Rational = Rational(1) / 8

    graceNoteFraction(ws, fraction, note) should equal(
      fs(4, qn / 8) :+: e(4, 7 * qn / 8)
    )
  }

  "graceNoteDownbeat" should "add to a note a grace note so that the downbeat of the main note is unchanged" in {
    val note1: Music[Pitch] = c(4, qn)
    val note2: Music[Pitch] = e(4, qn)
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
    val m2 = c(4, en) :+: Modification(Instrument(Violin), g(5, en)) :+: as(4, qn) :+: d(5, qn)
    val music: Music[Pitch] = m1 :=: m2

    retro(music) should equal(
      (((c(5, qn) :+: g(4, qn)) :+: f(5, qn)) :+: b(4, qn)) :=:
        ((((rest[Pitch](qn) :+: d(5, qn)) :+: as(4, qn)) :+: Modification(Instrument(Violin), g(5, en))) :+: c(4, en))
    )
  }

  "pitches" should "convert a line music to a list of pitches by applying a function to the original pitches in the Music" in {
    val music: Music[Pitch] = c(4, qn) :+: d(4, qn) :+: rest[Pitch](qn) :+: b(4, qn) :+: rest[Pitch](hn)
    val function: Pitch => Pitch = p => pitch(absPitch(p) + ws)

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

}