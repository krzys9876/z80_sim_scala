package org.kr.scala.z80.test

import org.kr.scala.z80.{Memory, MemoryController}
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

  test("poke memory direct function declaration") {
    //given
    val memoryController=MemoryController.blank(5)
    //when
    val afterState=memoryController >>= (mem=>MemoryController(mem.replaceAt(2,67)))
    //then
    assert(afterState.get.mem.equals(Vector[Int](0,0,67,0,0)))
  }
}
