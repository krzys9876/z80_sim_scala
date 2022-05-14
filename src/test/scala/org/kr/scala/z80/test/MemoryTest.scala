package org.kr.scala.z80.test

import org.kr.scala.z80.system.{BaseStateMonad, Memory}
import org.scalatest.funsuite.AnyFunSuite

class MemoryTest extends AnyFunSuite {
  test("always pass") {
    assert(1==1)
  }

  test("init blank memory") {
    //given
    val memoryController=BaseStateMonad[Memory](Memory.blank(5))
    //when
    //then
    assert(memoryController.get.mem.equals(Vector[Int](0,0,0,0,0)))
  }

  test("init preloaded memory") {
    //given
    val memoryController=BaseStateMonad[Memory](Memory.preloaded(Vector[Int](1,2,3),4))
    //when
    //then
    assert(memoryController.get.mem.equals(Vector[Int](1,2,3,0)))
  }

  test("poke memory") {
    //given
    val memoryController=BaseStateMonad[Memory](Memory.blank(5))
    //when
    val afterState=memoryController >>== Memory.poke(1,34) >>== Memory.poke(4,56) >>== Memory.poke(0,12)
    //then
    assert(afterState.get.mem.equals(Vector[Int](12,34,0,0,56)))
  }

  test("poke multiple values") {
    //given
    val memoryController=BaseStateMonad[Memory](Memory.blank(5))
    //when
    val afterState=memoryController >>== Memory.pokeMulti(1,Vector(34,56,78))
    //then
    assert(afterState.get.mem.equals(Vector[Int](0,34,56,78,0)))
  }

  test("poke memory direct function declaration") {
    //given
    val memoryController=BaseStateMonad[Memory](Memory.blank(5))
    //when
    val afterState=memoryController >>= (mem=>BaseStateMonad[Memory](mem.poke(2,67)))
    //then
    assert(afterState.get.mem.equals(Vector[Int](0,0,67,0,0)))
  }

  test("lock lower memory") {
    //given
    val memoryController=BaseStateMonad[Memory](Memory.blank(10))
    val memoryAfter1=memoryController >>== Memory.pokeMulti(0,Vector(100,101,102,103,104,105,106,107,108,109))
    //when
    val memoryAfter2=
      memoryAfter1 >>==
      Memory.lockTo(5) >>==
      Memory.poke(0,200) >>==
      Memory.pokeMulti(1,Vector(201,202)) >>==
      Memory.pokeMulti(3,Vector(203,204,205,206)) >>==
      Memory.pokeMulti(7,Vector(207,208)) >>==
      Memory.poke(9,209)
    //then
    assert(memoryAfter2.get.mem.equals(Vector[Int](100,101,102,103,104,205,206,207,208,209)))
  }

}
