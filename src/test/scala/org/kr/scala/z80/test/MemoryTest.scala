package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Memory, MemoryController}
import org.scalatest.funsuite.AnyFunSuite

class MemoryTest extends AnyFunSuite {
  test("always pass") {
    assert(1==1)
  }

  test("init blank memory") {
    //given
    val memoryController=MemoryController.blank(5)
    //when
    //then
    assert(memoryController.get.mem.equals(Vector[Int](0,0,0,0,0)))
  }

  test("init preloaded memory") {
    //given
    val memoryController=MemoryController(Memory.preloaded(Vector[Int](1,2,3),4))
    //when
    //then
    assert(memoryController.get.mem.equals(Vector[Int](1,2,3,0)))
  }

  test("poke memory") {
    //given
    val memoryController=MemoryController.blank(5)
    //when
    val afterState=memoryController >>= MemoryController.poke(1,34) >>= MemoryController.poke(4,56) >>= MemoryController.poke(0,12)
    //then
    assert(afterState.get.mem.equals(Vector[Int](12,34,0,0,56)))
  }

  test("poke multiple values") {
    //given
    val memoryController=MemoryController.blank(5)
    //when
    val afterState=memoryController >>= MemoryController.pokeMulti(1,Vector(34,56,78))
    //then
    assert(afterState.get.mem.equals(Vector[Int](0,34,56,78,0)))
  }

  test("poke memory direct function declaration") {
    //given
    val memoryController=MemoryController.blank(5)
    //when
    val afterState=memoryController >>= (mem=>MemoryController(mem.replaceAt(2,67)))
    //then
    assert(afterState.get.mem.equals(Vector[Int](0,0,67,0,0)))
  }

  test("lock lower memory") {
    //given
    val memoryController=MemoryController.blank(10)
    val memoryAfter1=memoryController >>= MemoryController.pokeMulti(0,Vector(100,101,102,103,104,105,106,107,108,109))
    //when
    val memoryAfter2=
      memoryAfter1 >>=
      MemoryController.lockTo(5) >>=
      MemoryController.poke(0,200) >>=
      MemoryController.pokeMulti(1,Vector(201,202)) >>=
      MemoryController.pokeMulti(3,Vector(203,204,205,206)) >>=
      MemoryController.pokeMulti(7,Vector(207,208)) >>=
      MemoryController.poke(9,209)
    //then
    assert(memoryAfter2.get.mem.equals(Vector[Int](100,101,102,103,104,205,206,207,208,209)))
  }

}
