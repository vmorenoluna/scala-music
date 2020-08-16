package performance

import music.InstrumentName.AltoSax
import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import performance.Performance.{Performance, merge}

class PerformanceSpec extends AnyFlatSpec with Matchers {

  "merge" should "merge two performances of equal length" in {
    val p1: Performance = List(
      MusicEvent(0, AltoSax, 75, 45, 60, List.empty),
      MusicEvent(45, AltoSax, 75, 45, 60, List.empty),
      MusicEvent(90, AltoSax, 75, 45, 60, List.empty)
    )
    val p2: Performance = List(
      MusicEvent(5, AltoSax, 75, 20, 60, List.empty),
      MusicEvent(25, AltoSax, 75, 45, 60, List.empty),
      MusicEvent(70, AltoSax, 75, 45, 60, List.empty)
    )

    merge(p1, p2) should equal(
      List(
        MusicEvent(0, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(5, AltoSax, 75, 20, 60, List.empty),
        MusicEvent(25, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(45, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(70, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(90, AltoSax, 75, 45, 60, List.empty)
      )
    )
  }

  "merge" should "merge two performances of different length" in {
    val p1: Performance = List(
      MusicEvent(0, AltoSax, 75, 45, 60, List.empty),
      MusicEvent(45, AltoSax, 75, 45, 60, List.empty),
    )
    val p2: Performance = List(
      MusicEvent(5, AltoSax, 75, 20, 60, List.empty),
      MusicEvent(25, AltoSax, 75, 45, 60, List.empty),
      MusicEvent(70, AltoSax, 75, 45, 60, List.empty)
    )

    merge(p1, p2) should equal(
      List(
        MusicEvent(0, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(5, AltoSax, 75, 20, 60, List.empty),
        MusicEvent(25, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(45, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(70, AltoSax, 75, 45, 60, List.empty)
      )
    )

    merge(p2, p1) should equal(
      List(
        MusicEvent(0, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(5, AltoSax, 75, 20, 60, List.empty),
        MusicEvent(25, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(45, AltoSax, 75, 45, 60, List.empty),
        MusicEvent(70, AltoSax, 75, 45, 60, List.empty)
      )
    )

  }

}
