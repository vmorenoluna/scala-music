# MIDI Playback Notes

## Known Limitations

### Java MIDI Sequencer Timing Issues

The Java MIDI Sequencer (used by `MusicService.play()`) has known timing glitches that affect live playback, particularly in the first few seconds of playback. These glitches manifest as:

- Notes playing at incorrect times
- Inconsistent timing between playback runs
- Some notes appearing "rushed" or poorly synchronized

**Important**: These issues are **not** caused by incorrect MIDI data generation. The MIDI files created by `MusicService.write()` are correct and play perfectly in external MIDI players (e.g., MuseScore, VLC, Windows Media Player).

### Root Cause

This is a long-standing bug in Java's `javax.sound.midi.Sequencer` implementation. The sequencer has race conditions in its internal timing system that cause inconsistent playback, especially during the first few events. Multiple workarounds documented in the Java MIDI community (lead-in offsets, loop point adjustments, tempo setting order) have been tested and do not reliably fix the issue.

### Recommended Workflow

For reliable, glitch-free playback:

1. **Use `MusicService.write()` to export MIDI files**:
   ```scala
   val service = new MusicService(soundfontPath)
   service.write(score, "output.mid")
   ```

2. **Play the exported MIDI file in an external player**:
   - MuseScore (free, cross-platform)
   - VLC Media Player
   - Windows Media Player
   - Any DAW (Digital Audio Workstation)

3. **Use `MusicService.play()` only for quick previews**:
   - Useful for rapid iteration during composition
   - Acceptable for testing/debugging
   - Not recommended for final playback or recordings

## References

- [Java MIDI Sequencer timing issues on Stack Overflow](https://stackoverflow.com/questions/63670860/java-midi-sequencer-timing-is-off)
- [Java Sound MIDI documentation](https://docs.oracle.com/javase/tutorial/sound/MIDI-seq-adv.html)