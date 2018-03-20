import org.scalatest._
import com.walcron.etc._

class TestQuickSort extends FlatSpec {
  def fixture =
    new {
      val nonSortedNumberArray = (10 to 1 by -1).toArray
      val sortedNumberArray = (1 to 10).toArray

      val nonSortedCharArray = ('a' to 'z').reverse.toArray
      val sortedCharArray = ('a' to 'z').toArray

      val nonSortedString = "HelloThere"
      val sortedString = "HTeeehllor"
    }

  "Given a nonsorted number array" should "sorted" in {
    val f = fixture
    //assert(Quicksort.sort(f.nonSortedNumberArray).sameElements(f.sortedNumberArray))
    assertResult(f.sortedNumberArray) {
      Quicksort.sort(f.nonSortedNumberArray)
    }
  }

  it should "sort characters too" in {
    val f = fixture
    assertResult(f.sortedCharArray) {
      Quicksort.sort(f.nonSortedCharArray)
    }
  }

  it should "sort String too" in {
    val f = fixture
    assertResult(f.sortedString) {
      Quicksort.sortLetter(f.nonSortedString)
    }
  }
}
