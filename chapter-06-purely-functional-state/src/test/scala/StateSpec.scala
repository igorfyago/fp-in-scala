package com.github.plippe.fpinscala.chapter06

import org.scalatest._
import org.scalatest.prop.PropertyChecks

import com.github.plippe.fpinscala.chapter06.Gen._

class StateSpec extends FunSuite with PropertyChecks {

  test("unit") {
    forAll { a: Int =>
      val expected = Answers.State.unit(a).run(())
      val result = State.unit(a).run(())

      assert(expected == result)
    }
  }

  test("map") {
    def f(a: Int) = a + 1

    forAll { s: State[Unit, Int] =>
      val expected = Answers.State.map(s)(f).run(())
      val result = State.map(s)(f).run(())

      assert(expected == result)
    }
  }

  test("map2") {
    forAll { (sa: State[Unit, Int], sb: State[Unit, Int]) =>
      val expected = Answers.State.map2(sa)(sb)(_ + _).run(())
      val result = State.map2(sa)(sb)(_ + _).run(())

      assert(expected == result)
    }
  }

  test("flatMap") {
    def f(a: Int) = State[Unit, Int](s => (a + 1, s))

    forAll { s: State[Unit, Int] =>
      val expected = Answers.State.flatMap(s)(f).run(())
      val result = State.flatMap(s)(f).run(())

      assert(expected == result)
    }
  }

  test("sequence") {
    forAll { s: List[State[Unit, Int]] =>
      val expected = Answers.State.sequence(s).run(())
      val result = State.sequence(s).run(())

      assert(expected == result)
    }
  }

  test("simulateMachine") {
    forAll { (machine: Machine, is: List[Input]) =>
      val expected = Answers.State.simulateMachine(is).run(machine)
      val result = State.simulateMachine(is).run(machine)

      assert(expected == result)
    }
  }

}
