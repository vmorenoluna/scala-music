package music

/**
 * An enumeration of modes
 */
object Mode extends Enumeration {
  type Mode = Value
  val Major, Minor, Ionian, Dorian,
  Phrygian, Lydian, Mixolydian,
  Aeolian, Locrian, CustomMode = Value
}
