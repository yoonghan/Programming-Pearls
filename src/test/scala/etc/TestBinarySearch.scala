import org.scalatest._
import com.walcron.etc._

class TestBinarySearch extends FlatSpec {
  def fixture =
    new {
      val sortedNumberArray = Array(1,2,3,4,5,6,7,8,9,10,11,12,14,15,18,20,22,23,25,26)
      val unsortedNumberArray = Array(5,4,3,2,1)
    }

  "Given a sorted number array" should "be able to find value x in pos (x-1)" in {
    val f = fixture
    for(x <- 1 to 2) {
      assertResult(x-1) {
        BinarySearch.binarySearch(f.sortedNumberArray, x)
      }
    }
  }

  it should "return a negative array length if value is notfound" in {
    val f = fixture
    for(x <- 1 to 2) {
      assert(BinarySearch.binarySearch(f.sortedNumberArray, (99 + x)) == (f.sortedNumberArray.length * -1))
    }
  }

  "Given an unsorted number array it" should "return the wrong position" in {
    val f = fixture
    for(x <- 1 to 2) {
      assert(BinarySearch.binarySearch(f.unsortedNumberArray, x) != (x-1))
    }
  }
}
