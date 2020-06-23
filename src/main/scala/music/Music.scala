package music

import music.InstrumentName.InstrumentName
import music.Mode.Mode
import music.Types.PitchClass.PitchClass
import music.Types.PitchClass._
import music.Types._
import spire.math.Rational
import spire.math.Rational.zero

import scala.collection.immutable.LazyList.#::
import scala.math.{max, min}

/**
 * The Primitive data type.
 * A Primitive can be either a Note or a Rest of type `A`.
 *
 * @tparam A
 */
sealed trait Primitive[A]

/**
 * The Note Primitive of type `A`.
 * A Note has a duration and some features of type `A`.
 *
 * @param d        the duration
 * @param features the note's features
 * @tparam A
 */
final case class Note[A](d: Duration, features: A) extends Primitive[A]

/**
 * The Rest Primitive of type `A`.
 * A Rest has a duration.
 *
 * @param d the duration
 */
final case class Rest[A](d: Duration) extends Primitive[A]

/**
 * The Music data type of type `A`.
 * A Music can be either a Prim (primitive), a :+: (two Musics played in sequence),
 * a :=: (two Musics played in parallel), or a Modification (a change applied to a Music)
 *
 * @tparam A
 */
sealed trait Music[A] {
  /**
   * Create a new Music by sequentially adding a Music to this one.
   *
   * @param that the Music to sequentially add to this one
   * @return
   */
  def :+:(that: Music[A]): Music[A] = new :+:(that, this)

  /**
   * Create a new Music by adding a Music in parallel to this one.
   *
   * @param that the Music to add in parallel to this one
   * @return
   */
  def :=:(that: Music[A]): Music[A] = new :=:(that, this)

  /**
   * Truncate a parallel composition made by this Music and that Music.
   *
   * @param that the parallel Music to cut with this
   * @return
   */
  def /=:(that: Music[A]): Music[A] = Music.cut(Music.duration(that), this) :=: Music.cut(Music.duration(this), that)
}

/**
 * The Prim Music of type `A`.
 * This type wraps a Primitive of type `A`.
 *
 * @param primitive the wrapped Primitive
 * @tparam A
 */
final case class Prim[A](primitive: Primitive[A]) extends Music[A]

/**
 * The Modified Music of type `A`.
 * This type wraps a music of type `A` and a Control that describes the changes to apply on it.
 *
 * @param m the Music to modify
 * @param c the changes to apply to the music
 * @tparam A
 */
final case class Modification[A](c: Control, m: Music[A]) extends Music[A]

/**
 * A sequential composition of two Musics of type `A`.
 *
 * @param m the first Music
 * @param n the second Music
 * @tparam A
 */
final case class :+:[A](m: Music[A], n: Music[A]) extends Music[A]

/**
 * A parallel composition of two Musics of type `A`.
 *
 * @param m the first Music
 * @param n the second Music
 * @tparam A
 */
final case class :=:[A](m: Music[A], n: Music[A]) extends Music[A]

object Music {

  /**
   * A handy method to create a note of type `A` with some features and a duration.
   *
   * @param d        the duration of the note
   * @param features the features of the note
   * @tparam A
   * @return
   */
  def note[A](d: Duration, features: A): Prim[A] = Prim(Note(d, features))

  /**
   * A handy method to create a rest of type `A` with a specific duration.
   *
   * @param d the duration of the rest
   * @return
   */
  def rest[A](d: Duration): Prim[A] = Prim(Rest(d))

  /**
   * Scale the tempo of a Music of type `A` by a given factor.
   *
   * @param d the factor by which the music will be scaled
   * @param m the Music to modify
   * @tparam A
   * @return
   */
  def tempo[A](d: Duration, m: Music[A]): Music[A] = Modification(Tempo(d), m)

  /**
   * Transpose a Music of type `A` to a given absolute pitch.
   *
   * @param i the absolute pitch
   * @param m the Music to transpose
   * @tparam A
   * @return
   */
  def transpose[A](i: AbsPitch, m: Music[A]): Music[A] = Modification(Transpose(i), m)

  /**
   * Change the instrument of a Music of type `A`.
   *
   * @param i the instrument
   * @param m the Music
   * @tparam A
   * @return
   */
  def instrument[A](i: InstrumentName, m: Music[A]): Music[A] = Modification(Instrument(i), m)

  /**
   * Apply a list of PhraseAttributes to a Music of type `A`.
   *
   * @param pa the list of PhraseAttribute
   * @param m  the Music
   * @tparam A
   * @return
   */
  def phrase[A](pa: List[PhraseAttribute], m: Music[A]): Music[A] = Modification(Phrase(pa), m)

  /**
   * Change the key signature of a Music of type `A`.
   *
   * @param pc the PitchClass
   * @param mo the Mode
   * @param m  the Music
   * @tparam A
   * @return
   */
  def keysig[A](pc: PitchClass, mo: Mode, m: Music[A]): Music[A] = Modification(KeySig(pc, mo), m)

  /**
   * Create a note of PitchClass Cff in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def cff(o: Octave, d: Duration): Prim[Pitch] = note(d, (Cff, o))

  /**
   * Create a note of PitchClass Cf in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def cf(o: Octave, d: Duration): Prim[Pitch] = note(d, (Cf, o))

  /**
   * Create a note of PitchClass C in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def c(o: Octave, d: Duration): Prim[Pitch] = note(d, (C, o))

  /**
   * Create a note of PitchClass Cs in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def cs(o: Octave, d: Duration): Prim[Pitch] = note(d, (Cs, o))

  /**
   * Create a note of PitchClass Css in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def css(o: Octave, d: Duration): Prim[Pitch] = note(d, (Css, o))

  /**
   * Create a note of PitchClass Dff in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def dff(o: Octave, d: Duration): Prim[Pitch] = note(d, (Dff, o))

  /**
   * Create a note of PitchClass Df in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def df(o: Octave, d: Duration): Prim[Pitch] = note(d, (Df, o))

  /**
   * Create a note of PitchClass D in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def d(o: Octave, d: Duration): Prim[Pitch] = note(d, (D, o))

  /**
   * Create a note of PitchClass Ds in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def ds(o: Octave, d: Duration): Prim[Pitch] = note(d, (Ds, o))

  /**
   * Create a note of PitchClass Dss in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def dss(o: Octave, d: Duration): Prim[Pitch] = note(d, (Dss, o))

  /**
   * Create a note of PitchClass Eff in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def eff(o: Octave, d: Duration): Prim[Pitch] = note(d, (Eff, o))

  /**
   * Create a note of PitchClass Ef in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def ef(o: Octave, d: Duration): Prim[Pitch] = note(d, (Ef, o))

  /**
   * Create a note of PitchClass E in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def e(o: Octave, d: Duration): Prim[Pitch] = note(d, (E, o))

  /**
   * Create a note of PitchClass Es in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def es(o: Octave, d: Duration): Prim[Pitch] = note(d, (Es, o))

  /**
   * Create a note of PitchClass Ess in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def ess(o: Octave, d: Duration): Prim[Pitch] = note(d, (Ess, o))

  /**
   * Create a note of PitchClass Fff in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def fff(o: Octave, d: Duration): Prim[Pitch] = note(d, (Fff, o))

  /**
   * Create a note of PitchClass Ff in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def ff(o: Octave, d: Duration): Prim[Pitch] = note(d, (Ff, o))

  /**
   * Create a note of PitchClass F in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def f(o: Octave, d: Duration): Prim[Pitch] = note(d, (F, o))

  /**
   * Create a note of PitchClass Fs in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def fs(o: Octave, d: Duration): Prim[Pitch] = note(d, (Fs, o))

  /**
   * Create a note of PitchClass Fss in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def fss(o: Octave, d: Duration): Prim[Pitch] = note(d, (Fss, o))

  /**
   * Create a note of PitchClass Gff in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def gff(o: Octave, d: Duration): Prim[Pitch] = note(d, (Gff, o))

  /**
   * Create a note of PitchClass Gf in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def gf(o: Octave, d: Duration): Prim[Pitch] = note(d, (Gf, o))

  /**
   * Create a note of PitchClass G in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def g(o: Octave, d: Duration): Prim[Pitch] = note(d, (G, o))

  /**
   * Create a note of PitchClass Gs in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def gs(o: Octave, d: Duration): Prim[Pitch] = note(d, (Gs, o))

  /**
   * Create a note of PitchClass Gss in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def gss(o: Octave, d: Duration): Prim[Pitch] = note(d, (Gss, o))

  /**
   * Create a note of PitchClass Aff in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def aff(o: Octave, d: Duration): Prim[Pitch] = note(d, (Aff, o))

  /**
   * Create a note of PitchClass Af in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def af(o: Octave, d: Duration): Prim[Pitch] = note(d, (Af, o))

  /**
   * Create a note of PitchClass A in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def a(o: Octave, d: Duration): Prim[Pitch] = note(d, (A, o))

  /**
   * Create a note of PitchClass As in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def as(o: Octave, d: Duration): Prim[Pitch] = note(d, (As, o))

  /**
   * Create a note of PitchClass Ass in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def ass(o: Octave, d: Duration): Prim[Pitch] = note(d, (Ass, o))

  /**
   * Create a note of PitchClass Bff in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def bff(o: Octave, d: Duration): Prim[Pitch] = note(d, (Bff, o))

  /**
   * Create a note of PitchClass Bf in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def bf(o: Octave, d: Duration): Prim[Pitch] = note(d, (Bf, o))

  /**
   * Create a note of PitchClass B in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def b(o: Octave, d: Duration): Prim[Pitch] = note(d, (B, o))

  /**
   * Create a note of PitchClass Bs in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def bs(o: Octave, d: Duration): Prim[Pitch] = note(d, (Bs, o))

  /**
   * Create a note of PitchClass Bss in the given octave and with
   * the specified duration.
   * The created note is a Music of Pitch.
   *
   * @param o the octave
   * @param d the duration
   * @return
   */
  def bss(o: Octave, d: Duration): Prim[Pitch] = note(d, (Bss, o))

  val bn: Duration = 2
  val wn: Duration = 1
  val hn: Duration = Rational(1) / 2
  val qn: Duration = Rational(1) / 4
  val en: Duration = Rational(1) / 8
  val sn: Duration = Rational(1) / 16
  val tn: Duration = Rational(1) / 32
  val sfn: Duration = Rational(1) / 64
  val dwn: Duration = Rational(3) / 2
  val dhn: Duration = Rational(3) / 4
  val dqn: Duration = Rational(3) / 8
  val den: Duration = Rational(3) / 16
  val dsn: Duration = Rational(3) / 32
  val dtn: Duration = Rational(3) / 64
  val ddhn: Duration = Rational(7) / 8
  val ddqn: Duration = Rational(7) / 16
  val dden: Duration = Rational(7) / 32

  def bnr: Prim[Pitch] = rest(bn)

  def wnr: Prim[Pitch] = rest(wn)

  def hnr: Prim[Pitch] = rest(hn)

  def qnr: Prim[Pitch] = rest(qn)

  def enr: Prim[Pitch] = rest(en)

  def snr: Prim[Pitch] = rest(sn)

  def tnr: Prim[Pitch] = rest(tn)

  def sfnr: Prim[Pitch] = rest(sfn)

  def dwnr: Prim[Pitch] = rest(dwn)

  def dhnr: Prim[Pitch] = rest(dhn)

  def dqnr: Prim[Pitch] = rest(dqn)

  def denr: Prim[Pitch] = rest(den)

  def dsnr: Prim[Pitch] = rest(dsn)

  def dtnr: Prim[Pitch] = rest(dtn)

  def ddhnr: Prim[Pitch] = rest(ddhn)

  def ddqnr: Prim[Pitch] = rest(ddqn)

  def ddenr: Prim[Pitch] = rest(dden)

  /**
   * Sequentially compose a list of Musics
   *
   * @param notes
   * @tparam A
   * @return
   */
  def line[A](notes: List[Prim[A]]): Music[A] = notes.foldRight(rest(0).asInstanceOf[Music[A]])(_ :+: _)

  /**
   * Parallelly compose a list of Musics
   *
   * @param notes
   * @tparam A
   * @return
   */
  def chord[A](notes: List[Music[A]]): Music[A] = notes.foldRight(rest(0).asInstanceOf[Music[A]])(_ :=: _)

  /**
   * Get the highest Pitch from a list of Pitch
   *
   * @param pitches
   * @return
   */
  def maxPitch(pitches: List[Pitch]): Pitch = pitches.foldRight(pitch(0))(_ max _)

  /**
   * Get the highest absolute pitch from a list of absolute pitches
   *
   * @param absPitches
   * @return
   */
  def maxAbsPitch(absPitches: List[AbsPitch]): AbsPitch = absPitches.foldLeft(Int.MinValue)(max)

  /**
   * Get the minimum absolute pitch from a list of absolute pitches
   *
   * @param absPitches
   * @return
   */
  def minAbsPitch(absPitches: List[AbsPitch]): AbsPitch = absPitches.foldLeft(Int.MaxValue)(min)

  /**
   * Fuse a list of Duration with a list of Prims with unspecified Duration.
   *
   * @param ds    the list of durations
   * @param notes the list of notes with unspecified duration
   * @tparam A
   * @return
   */
  def fuse[A](ds: List[Duration], notes: List[Duration => Prim[A]]): List[Music[A]] =
    ds zip notes map { case (a, b) => b(a) }

  /**
   * Compose a Music by sequentially repeating a given Music for the
   * given amount of times.
   *
   * @param n the number of repetitions
   * @param m the Music to repeat
   * @tparam A
   * @return
   */
  def times[A](n: Int, m: Music[A]): Music[A] = n match {
    case 0 => rest(0)
    case n => m :+: times(n - 1, m)
  }

  /**
   * Add a duration to a list of notes with unspecified duration.
   *
   * @param d
   * @param notes
   * @tparam A
   * @return
   */
  def addDuration[A](d: Duration, notes: List[Duration => Prim[A]]): Music[A] =
    line(notes.map(note => note(d)))

  /**
   * Add a grace note to a given note.
   * The grace note will be higher than the main note by a given step.
   *
   * @param step the number of steps from the main note at which the grace note will be placed.
   * @param n    the main note
   * @return
   */
  def graceNote(step: Step, n: Prim[Pitch]): Music[Pitch] = n match {
    case Prim(Note(d, p: Pitch)) => note(d / 8, p.transpose(step)) :+: note(7 * d / 8, p)
    case _ => n
  }

  /**
   * Add a grace note to a given note.
   * The grace note will be higher than the main note by a given step and
   * will last for the given main note's time fraction.
   * The downbeat of the grace note is placed at the point of the main note.
   *
   * @param step     step between the grace note and the main note
   * @param fraction scale factor on grace note's duration
   * @param n        the main note
   * @return
   */
  def graceNoteFraction(step: Step, fraction: Rational, n: Prim[Pitch]): Music[Pitch] = (step, fraction, n) match {
    case (n, r, Prim(Note(d, p: Pitch))) => note(r * d, p.transpose(n)) :+: note((1 - r) * d, p)
    case _ => n
  }

  /**
   * Add a grace note to a given note, by reducing the previous note's duration
   * The grace note will be higher than the main note by a given step.
   * The downbeat of the main note is unchanged.
   *
   * @param step     step between the grace note and the main note
   * @param fraction scale factor on grace note's duration
   * @param n1       the previous note
   * @param n2       the main note
   * @return
   */
  def graceNoteDownbeat(step: Step, fraction: Rational, n1: Prim[Pitch], n2: Prim[Pitch]): Music[Pitch] = (step, fraction, n1, n2) match {
    case (n, r, Prim(Note(d1, p1: Pitch)), Prim(Note(d2, p2: Pitch))) =>
      note(d1 - r * d2, p1) :+: note(r * d2, p2.transpose(n)) :+: note(d2, p2)
    case _ => n2
  }

  /**
   * Offset a given Music by a given duration
   *
   * @param d the offset duration
   * @param m the Music
   * @tparam A
   * @return
   */
  def offset[A](d: Duration, m: Music[A]): Music[A] =
    rest[A](d) :+: m

  /**
   * Convert a Music generated by the line function to a list of Music.
   * A Music generated by the line function should only have rests and notes
   * composed sequentially: if anything else is encountered during the processing
   * it is ignored.
   *
   * @param m
   * @tparam A
   * @return
   */
  def lineToList[A](m: Music[A]): List[Music[A]] = m match {
    case Prim(Rest(d)) if d == 0 => List.empty
    case Prim(Note(d, f)) => List(Prim(Note(d, f)))
    case :+:(n, ns) => n :: lineToList(ns)
    case _ => List.empty
  }

  /**
   * Invert a Music generated by the line function.
   * A Music generated by the line function should only have rests and notes
   * composed sequentially: if anything else is encountered during the processing
   * it is ignored.
   *
   * @param m
   * @return
   */
  def invert(m: Music[Pitch]): Music[Pitch] = {
    val l@(Prim(Note(_, r)) :: _) = lineToList(m)
    line(
      l.map {
        case Prim(Note(d, p)) => note(d, pitch(2 * absPitch(r) - absPitch(p)))
        case Prim(Rest(d)) => rest(d)
        case _ => rest(0)
      }
    )
  }

  /**
   * Reverse a Music.
   *
   * @param m
   * @tparam A
   * @return
   */
  def retro[A](m: Music[A]): Music[A] = m match {
    case n: Prim[A] => n
    case Modification(control, music) => Modification(control, retro(music))
    case :+:(m1, m2) => retro(m2) :+: retro(m1)
    case :=:(m1, m2) => {
      val d1 = duration(m1)
      val d2 = duration(m2)
      if (d1 > d2) retro(m1) :=: (rest[A](d1 - d2) :+: retro(m2))
      else (rest[A](d2 - d1) :+: retro(m1)) :=: retro(m2)
    }
  }

  /**
   * Convert a Music to a list of Pitch, which have been obtained
   * by applying the given function to the original pitches in the Music.
   * The Music must have been generated with the line function: it should only have rests
   * and notes composed sequentially. If anything else is encountered during the processing
   * it is ignored.
   *
   * @param m
   * @param f
   * @return
   */
  def pitches(m: Music[Pitch], f: Pitch => Pitch): List[Pitch] = m match {
    case Prim(Rest(_)) => List.empty
    case Prim(Note(_, p)) => List(f(p))
    case :+:(Prim(Rest(_)), ns) => pitches(ns, f)
    case :+:(Prim(Note(_, p)), ns) => f(p) :: pitches(ns, f)
    case _ => List.empty
  }

  /**
   * Reverse the pitches in a given Music while maintaining the durations in the same order.
   * The Music must have been generated with the line function: it should only have rests and
   * notes composed sequentially. If anything else is encountered during the processing
   * it is ignored.
   *
   * @param m
   * @return
   */
  def retroPitches(m: Music[Pitch]): Music[Pitch] = {
    val music = lineToList(m)
    val musicZipped: List[(Music[Pitch], Music[Pitch])] = music.zip(music.reverse)

    def go(list: List[(Music[Pitch], Music[Pitch])]): Music[Pitch] =
      list match {
        case Nil => rest(0)
        case ::((Prim(Note(d, _)), Prim(Note(_, p))), tail) => note(d, p) :+: go(tail)
        case _ => rest(0)
      }

    go(musicZipped)
  }

  /**
   * Transpose a Music by a given absolute pitch.
   *
   * @param ap
   * @param mp
   * @return
   */
  def transM(ap: AbsPitch, mp: Music[Pitch]): Music[Pitch] = mp match {
    case Prim(Note(d, p)) => Prim(Note(d, p.transpose(ap)))
    case Prim(Rest(d)) => Prim(Rest(d))
    case :+:(m, n) => transM(ap, m) :+: transM(ap, n)
    case :=:(m, n) => transM(ap, m) :=: transM(ap, n)
    case Modification(control, music) => Modification(control, transM(ap, music))
  }

  /**
   * Calculate the duration of a Music.
   *
   * @param m
   * @tparam A
   * @return
   */
  def duration[A](m: Music[A]): Duration = m match {
    case Prim(Note(d, _)) => d
    case Prim(Rest(d)) => d
    case Modification(Tempo(r), m) => duration(m) / r
    case Modification(_, m) => duration(m)
    case :+:(m1, m2) => duration(m1) + duration(m2)
    case :=:(m1, m2) => duration(m1) max duration(m2)
  }

  /**
   * Cut the initial specified duration from a Music.
   *
   * @param d
   * @param m
   * @tparam A
   * @return
   */
  def cut[A](d: Duration, m: Music[A]): Music[A] = m match {
    case _ if d <= 0 => rest(0)
    case Prim(Note(oldD, p)) => note(oldD min d, p)
    case Prim(Rest(oldD)) => rest(oldD min d)
    case Modification(Tempo(r), m) => tempo(r, cut(d * r, m))
    case Modification(c, m) => Modification(c, cut(d, m))
    case :+:(m1, m2) => {
      val m3 = cut[A](d, m1)
      val m4 = cut[A](d - duration(m3), m2)
      m3 :+: m4
    }
    case :=:(m1, m2) => cut(d, m1) :=: cut(d, m2)
  }

  /**
   * Remove the specified duration from a Music.
   *
   * @param d
   * @param m
   * @tparam A
   * @return
   */
  def remove[A](d: Duration, m: Music[A]): Music[A] = m match {
    case _ if d <= 0 => m
    case Prim(Note(oldD, p)) => note((oldD - d) max 0, p)
    case Prim(Rest(oldD)) => rest((oldD - d) max 0)
    case Modification(Tempo(r), m) => tempo(r, remove(d * r, m))
    case Modification(c, m) => Modification(c, remove(d, m))
    case :+:(m1, m2) => {
      val m3 = remove[A](d, m1)
      val m4 = remove[A](d - duration(m1), m2)
      m3 :+: m4
    }
    case :=:(m1, m2) => remove(d, m1) :=: remove(d, m2)
  }

  /**
   * Remove all notes and rests with zero duration from a Music
   *
   * @param m
   * @tparam A
   * @return
   */
  def removeZeros[A](m: Music[A]): Music[A] = m match {
    case Prim(p) => Prim(p)
    case Modification(c, m) => Modification(c, removeZeros(m))
    case :=:(m1, m2) => (removeZeros(m1), removeZeros(m2)) match {
      case (Prim(Note(d, _)), m) if (d == zero) => m
      case (Prim(Rest(d)), m) if (d == zero) => m
      case (m, Prim(Note(d, _))) if (d == zero) => m
      case (m, Prim(Rest(d))) if (d == zero) => m
      case (m1, m2) => m1 :=: m2
    }
    case :+:(m1, m2) => (removeZeros(m1), removeZeros(m2)) match {
      case (Prim(Note(d, _)), m) if (d == zero) => m
      case (Prim(Rest(d)), m) if (d == zero) => m
      case (m, Prim(Note(d, _))) if (d == zero) => m
      case (m, Prim(Rest(d))) if (d == zero) => m
      case (m1, m2) => m1 :+: m2
    }
  }

  /**
   * Add a trill note to a given note.
   *
   * @param interval the interval at which the trill must be played
   * @param d        the duration of the trill notes
   * @param m        the music. It can be a single note or a Modification of a single note.
   * @return
   */
  def trill(interval: Int, d: Duration, m: Music[Pitch]): Music[Pitch] = (interval, d, m) match {
    case (i, sDur, Prim(Note(tDur, p))) =>
      if (sDur >= tDur)
        note(tDur, p)
      else
        note(sDur, p) :+: trill(-i, sDur, note(tDur - sDur, p.transpose(i)))
    case (i, d, Modification(Tempo(r), m)) => tempo(r, trill(i, d * r, m))
    case (i, d, Modification(c, m)) => Modification(c, trill(i, d, m))
    case _ => rest(zero)
  }

  /**
   * Add a trill note to a given note.
   * The result starts on the trill note.
   *
   * @param interval the interval at which the trill must be played
   * @param d        the duration of the trill notes
   * @param m        the music. It can be a single note or a Modification of a single note.
   * @return
   */
  def trillOtherNote(interval: Int, d: Duration, m: Music[Pitch]): Music[Pitch] =
    trill(-interval, d, transpose(interval, m))

  /**
   * Add a trill note to a given note.
   *
   * @param interval the interval at which the trill must be played
   * @param nTimes   how many subdivided notes to include in the trill
   * @param m        the music. It can be a single note or a Modification of a single note.
   * @return
   */
  def trilln(interval: Int, nTimes: Int, m: Music[Pitch]): Music[Pitch] =
    trill(interval, duration(m) / Rational(nTimes), m)

  /**
   * Add a trill note to a given note.
   * The result starts on the trill note.
   *
   * @param interval the interval at which the trill must be played
   * @param nTimes   how many subdivided notes to include in the trill
   * @param m        the music. It can be a single note or a Modification of a single note.
   * @return
   */
  def trillnOtherNote(interval: Int, nTimes: Int, m: Music[Pitch]): Music[Pitch] =
    trilln(-interval, nTimes, transpose(interval, m))

  /**
   * Define a roll. A roll is a trill with a zero interval.
   *
   * @param d the duration of the roll notes
   * @param m the music. It can be a single note or a Modification of a single note.
   * @return
   */
  def roll(d: Duration, m: Music[Pitch]): Music[Pitch] =
    trill(0, d, m)

  /**
   * Define a roll. A roll is a trill with a zero interval.
   *
   * @param nTimes how many subdivided notes to include in the roll
   * @param m      the music. It can be a single note or a Modification of a single note.
   * @return
   */
  def rolln(nTimes: Int, m: Music[Pitch]): Music[Pitch] =
    trilln(0, nTimes, m)

  /**
   * Convert a percussion sound to a note.
   *
   * @param ps the percussion sound
   * @param d  the duration
   * @return
   */
  def perc(ps: PercussionSound.Value, d: Duration): Music[Pitch] =
    instrument(InstrumentName.Percussion, note(d, pitch(ps.id + 35)))

  /**
   * A map function for the Primitive type.
   *
   * @param f
   * @param p
   * @tparam A
   * @tparam B
   * @return
   */
  def pMap[A, B](f: A => B, p: Primitive[A]): Primitive[B] = p match {
    case Note(d, x) => Note(d, f(x))
    case Rest(d) => Rest(d)
  }

  /**
   * A map function for the Music type.
   *
   * @param f
   * @param m
   * @tparam A
   * @tparam B
   * @return
   */
  def mMap[A, B](f: A => B, m: Music[A]): Music[B] = m match {
    case Prim(p) => Prim(pMap(f, p))
    case Modification(c, m) => Modification(c, mMap(f, m))
    case :+:(m1, m2) => mMap(f, m1) :+: mMap(f, m2)
    case :=:(m1, m2) => mMap(f, m1) :=: mMap(f, m2)
  }

  /**
   * Convert a Music[Pitch] to a Music[(Pitch, Volume)]
   *
   * @param v the volume
   * @param m the music
   * @return
   */
  def addVolume(v: Volume, m: Music[Pitch]): Music[(Pitch, Volume)] =
    mMap[Pitch, (Pitch, Volume)](p => (p, v), m)

  /**
   * Scale the volume of each note in a music by a given factor.
   *
   * @param s the scale factor
   * @param m the music
   * @return
   */
  def scaleVolume(s: Rational, m: Music[(Pitch, Volume)]): Music[(Pitch, Volume)] =
    mMap[(Pitch, Volume), (Pitch, Volume)](f => (f._1, (s * Rational(f._2)).intValue), m)

  /**
   * A fold for the Music type. It takes four constructors and
   * basically takes apart a Music value and puts it back together
   * with different constructors.
   *
   * @param f
   * @param +:
   * @param =:
   * @param g
   * @param m
   * @tparam A
   * @tparam B
   * @return
   */
  def mFold[A, B](f: Primitive[A] => B)
                 (+: : (B, B) => B)
                 (=: : (B, B) => B)
                 (g: (Control, B) => B)
                 (m: Music[A]): B = {
    val rec: Music[A] => B = mFold(f)(+:)(=:)(g)
    m match {
      case Prim(p) => f(p)
      case Modification(c, m) => g(c, rec(m))
      case :+:(m1, m2) => +:(rec(m1), rec(m2))
      case :=:(m1, m2) => =:(rec(m1), rec(m2))
    }
  }

  /**
   * Crete triplets out of eight notes.
   * TODO make it more generic
   *
   * @param m the music formed by eight notes
   * @tparam A
   * @return
   */
  def triplets[A](m: Music[A]): Music[A] =
    tempo(Rational(3) / Rational(2), m)

  /**
   * Create a phase composition by repeating the music in parallel
   * with itself.
   *
   * @param factor the phase factor
   * @param m      the music
   * @tparam A
   * @return
   */
  def phaseIt[A](factor: Duration, m: Music[A]): Music[A] =
    m :=: tempo(factor, m)

  /**
   * Recursively apply transformations f (to elements in a sequence)
   * and g (to accumulated phrases) some specified number of times.
   *
   * @param f transformation f
   * @param g transofrmation g
   * @param n number of times
   * @param m the music 
   * @tparam A
   * @return
   */
  def rep[A](f: Music[A] => Music[A], g: Music[A] => Music[A], n: Int, m: Music[A]): Music[A] = n match {
    case 0 => rest(zero)
    case n => m :=: g(rep(f, g, n - 1, f(m)))
  }

}