package org.kr.scala.z80.test

import org.kr.scala.z80.{MemoryController, Z80System, Z80SystemController}
import org.scalatest.funsuite.AnyFunSuite

class SystemTest extends AnyFunSuite {
  test("always pass") {
    assert(1==1)
  }

  test("run NOP and move PC") {
    //given
    val sys1=Z80SystemController.blank
    //when
    val sys2=sys1 >>= Z80SystemController.run(1000)
    //then
    assert(sys2.get.registerController.get("PC")==1000)
  }

  test("run NOP with memory overflow") {
    //given
    val sys1=Z80SystemController.blank
    //when
    val sys2=sys1 >>= Z80SystemController.run(1) >>= Z80SystemController.run(65536)
    //then
    assert(sys2.get.registerController.get("PC")==1)
  }

  test("run LD B,0xFE") {
    //given
    val sysBlank=Z80SystemController.blank
    val mem=sysBlank.get.memoryController >>=
      MemoryController.poke(10,0x06) >>= //LD H,0xFE
      MemoryController.poke(11,0xFE)
    //when
    val sysInit=Z80SystemController(new Z80System(MemoryController(mem.get),sysBlank.get.registerController))
    val sysTest=sysInit >>= Z80SystemController.run(11)
    //then
    assert(sysTest.get.registerController.get("PC")==12)
    assert(sysTest.get.registerController.get("B")==0xFE)
  }

  test("run LD H,0xFF | LD A,H") {
    //given
    val sysBlank=Z80SystemController.blank
    val mem=sysBlank.get.memoryController >>=
      MemoryController.poke(0,0x26) >>= //LD H,0xFF
      MemoryController.poke(1,0xFF) >>=
      MemoryController.poke(2,0x7C) // LD A, H
    //when
    val sysInit=Z80SystemController(new Z80System(MemoryController(mem.get),sysBlank.get.registerController))
    val sysTest=sysInit >>= Z80SystemController.run(2)
    //then
    assert(sysTest.get.registerController.get("PC")==3)
    assert(sysTest.get.registerController.get("A")==0xFF)
    assert(sysTest.get.registerController.get("H")==0xFF)
  }
}
