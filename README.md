# Scala Music
Scala Music is a library to work with music using the Scala programming language.

Copyright (c) 2020 Vincenzo Moreno Luna. Licensed [MIT](LICENSE.md).

# Overview

Scala Music is a library for defining music using functional programming.


Its structure is:

- `core` - data structures and DSL for defining music;
- `audio` - services for playback and storage;
- `performance` - objects and functions for music interpretation ;
- `examples` - example musics;

It uses Java MIDI to play musics, and it currently requires to specify the path of the soundfont to use. 

# Using Scala Music

There isn't a package available in online repositories yet. You have to build a JAR from this project and put it in your project.
Once you do it, you can write songs as follows:

```scala
import scalamusic.core.Music._

val music: Music[Pitch] =
      c(7, en) :+: enr :+: c(7, en) :+: enr :+: c(7, en) :+: c(7, dqn) :+:
      qnr :+: g(6, qn) :+: f(6, en) :+: enr :+: e(6, qn)
```

Each note is expressed by its name, its octave, and its duration. For the sake of simplicity, durations are expressed using their initials. 

For example:  

- `hn` - half note 
- `dqn` - dotted quarter note
- `qn` - quarter note
- `den` - dotted eighth note
- `en` - eighth note

Rests are expressed with duration's naming convention and a final "r". 

For example:

- `hnr` - half note rest
- `qnr` - quarter note rest
- `enr` - eighth note rest

You can use ":+:" to define a sequence of notes, and ":=:" to define what has to be played at the same time. You can also wrap a music inside a [Modification](https://github.com/vmorenoluna/scala-music/blob/6492acb13f299480e0e4022bcb83af19ef874044/src/main/scala/scalamusic/core/Music.scala#L88) in order to modify it according to some [Control](scalamusic/core/Control.scala).

See the _example_ folder for more examples.

An alternative way to describe a music is by providing absolute pitches (i.e. numbers) and then convert them to Pitches using the [pitch](https://github.com/vmorenoluna/scala-music/blob/6492acb13f299480e0e4022bcb83af19ef874044/src/main/scala/scalamusic/core/Types.scala#L116) function. This can be useful for algorithmic composition.

To play a music you must first create a service providing the path of the soundfont to use:

```scala
val musicService: MusicService = new MusicService(soundfontPath)
```

You must also declare a starting [Context](src/main/scala/scalamusic/performance/Context.scala) for your music:

```scala
val context: Context[NoteWithAttributes] = Context(0, DefaultPlayer, AcousticGrandPiano, Metronome.tickedWholeNote(96), 0, 127, (C, Mode.Major))
```

The context define things like the starting speed, instrument, the player that will perform the music, and more.

You have to attach a context to each of your musics, defining the parts that make the music score:

```
val score: List[Part] =
    List(
      (context.copy(cVol = 75, cInst = Flute), fluteMusic),
      (context.copy(cVol = 60, cInst = AcousticGrandPiano), pianoMusic),
      (context.copy(cVol = 70, cInst = AcousticBass), bassMusic)
    )
```

Now you can play the score as follows:

```scala
musicService.play(score)
```

You can also save the score into a MIDI file:

```scala
musicService.write(score, "Score.mid")
```

**Note on playback**: Due to timing issues in Java's MIDI Sequencer, live playback via `musicService.play()` may occasionally experience timing glitches. For reliable, glitch-free playback, it's recommended to export to a MIDI file using `musicService.write()` and play it in an external MIDI player. See [MIDI_PLAYBACK_NOTES.md](MIDI_PLAYBACK_NOTES.md) for details.

At this stage, there isn't support for all the MIDI features. Everything that is not yet supported is silently ignored when playing a song.

# Credits

This project is built upon the [scala-music-school](https://github.com/vmorenoluna/scala-music-school) project, which is the playground I used while following the book [The Haskell School Of Music](https://www.cambridge.org/core/books/haskell-school-of-music/6B377BCD40386E9D27EB93FC2F3B13FB). 
Scala Music, at this stage, is hence a kind of porting in Scala of the Haskell's library [Euterpea](http://euterpea.com/). At least as it is described in the book.
Here I want to give credit to Paul Hudak and Donya Quick for their work, as I really enjoyed the reading.