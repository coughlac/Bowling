import org.scalatest.{Matchers}

class BowlingTest extends org.scalatest.FunSuite with Matchers {

  test("The overall game score is the accumulated frame score") {
    Bowling.total((1, 2) ::(3, 4) ::(5, 6) :: Nil) should be(21)
  }

  test("If bowler gets a spare (frame score of 10), he scores 10 + next attempt score") {
    Bowling.total((9, 1) ::(6, 7) :: Nil) should be(29)
  }

  test("If bowler gets a score of 10 ina second attempt, it is still a spare, he scores 10 + next attempt score") {
    Bowling.total((0, 10) ::(6, 7) :: Nil) should be(29)
  }

  test("If bowler gets a strike (first attempt score of 10), he scores 10 for that frame + next frame score as bonus") {
    Bowling.total((10, 0) ::(6, 7) :: Nil) should be(36)
  }

  test("small example") {
    Bowling.total((1, 4) ::(4, 5) ::(6, 4) ::(5, 5) ::(10, 0) ::(0, 1) :: Nil) should be(61)
  }

  test("acceptance example") {
    Bowling.total((1, 4) ::(4, 5) ::(6, 4) ::(5, 5) ::(10, 0) ::(0, 1) ::(6, 4) ::(0, 10) ::(2, 8) ::(6, 0) :: Nil) should be(133)
  }

}
