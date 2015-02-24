package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only same elements") {
    new TestSets {
      val falseIntersect = intersect(s1, s2)
      val trueIntersect1 = intersect(s1, singletonSet(1))
      val trueIntersect2 = intersect(s2, singletonSet(2))
      val trueIntersect3 = intersect(s3, singletonSet(3))

      assert(!contains(falseIntersect, 1), "False Intersect 1 (they don't have common elements)")
      assert(!contains(falseIntersect, 2), "False Intersect 2 (they don't have common elements)")

      assert(contains(trueIntersect1, 1), "True Intersect 1 (both have element '1' in common)")
      assert(contains(trueIntersect2, 2), "True Intersect 2 (both have element '2' in common)")
      assert(contains(trueIntersect3, 3), "True Intersect 3 (both have element '3' in common)")
    }
  }

  test("diff contains difference in sets") {
    new TestSets {
      val diffContainsOne = diff(s1, s2)
      val diffContainsTwo = diff(s2, s3)
      val diffContainsTwo2 = diff(s2, s1)
      val diffContainsThree = diff(s3, s1)
      val diffContainsThree2 = diff(s3, s2)

      val diffContainsNone = diff(s1, s1)
      val diffContainsNone2 = diff(s2, s2)
      val diffContainsNone3 = diff(s3, s3)

      assert(contains(diffContainsOne, 1), "Diff 1")
      assert(contains(diffContainsTwo, 2), "Diff 2")
      assert(contains(diffContainsTwo, 2), "Diff 2 - 2")
      assert(contains(diffContainsThree, 3), "Diff 3")
      assert(contains(diffContainsThree2, 3), "Diff 3 - 2")

      assert(!contains(diffContainsNone, 1), "Diff None - 1")
      assert(!contains(diffContainsNone, 2), "Diff None - 2")
      assert(!contains(diffContainsNone, 3), "Diff None - 3")

      assert(!contains(diffContainsNone2, 1), "Diff None 2 - 1")
      assert(!contains(diffContainsNone2, 2), "Diff None 2 - 2")
      assert(!contains(diffContainsNone2, 3), "Diff None 2 - 3")

      assert(!contains(diffContainsNone3, 1), "Diff None 3 - 1")
      assert(!contains(diffContainsNone3, 2), "Diff None 3 - 2")
      assert(!contains(diffContainsNone3, 3), "Diff None 3 - 3")
    }
  }

  test("filter returns subset of s which returns true for predicate") {
    new TestSets {
      val emptySet: Set = x => false

      assert(contains(filter(s1, x => x == 1), 1), "Filter 1")
      assert(contains(filter(s1, s1), 1), "Filter 2")

      assert(!contains(filter(s2, x => x == 3), 2), "Filter 3")
      assert(!contains(filter(s3, x => x == 1), 1), "Filter 4")
    }
  }

  test("forall works!") {
    new TestSets {
      val u2 = union(s1, s2)
      val u3 = union(union(s1, s2), s3)
      val u4 = union(u3, singletonSet(7))

      assert(forall(s1, x => x == 1))
      assert(forall(u2, _ < 3))
      assert(forall(u3, _ < 4))
      assert(!forall(u2, _ == 5))
      assert(forall(u4, x => x < 4 || x == 7))
      assert(!forall(u4, x => x < 4 || x == 5))
    }
  }

  test("exists works") {
    new TestSets {
      val s = union(union(s1, s2), s3)

      assert(exists(s, (_ < 4)))
      assert(exists(s, (_ < 2)))
      assert(!exists(s, (_ < 0)))
    }
  }

  test("map works") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      printSet(s)
      // {1, 2, 3}
      val t = map(s, (x: Int) => x * x)
      printSet(t) // {1, 4, 9}
      //assert(contains(t, 9))
    }
  }


}
