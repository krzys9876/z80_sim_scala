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

  test("run LD C,(HL)") {
    //given
    val sysBlank=Z80SystemController.blank
    val mem=sysBlank.get.memoryController >>=
      MemoryController.poke(0,0x26) >>= //LD H,0x01
      MemoryController.poke(1,0x01) >>=
      MemoryController.poke(2,0x2E) >>= //LD L,0x02
      MemoryController.poke(3,0x02) >>=
      MemoryController.poke(4,0x4E) >>=
      MemoryController.poke(0x0102,0xFE) //LD C,(HL)
    //when
    val sysInit=Z80SystemController(new Z80System(MemoryController(mem.get),sysBlank.get.registerController))
    val sysTest=sysInit >>= Z80SystemController.run(3)
    //then
    assert(sysTest.get.registerController.get("PC")==5)
    assert(sysTest.get.registerController.get("H")==1)
    assert(sysTest.get.registerController.get("L")==2)
    assert(sysTest.get.memoryController.get(0x0102)==0xFE)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD (HL),n") {
    //given
    val sysBlank=Z80SystemController.blank
    val mem=sysBlank.get.memoryController >>=
      MemoryController.poke(0,0x26) >>= //LD H,0x01
      MemoryController.poke(1,0x01) >>=
      MemoryController.poke(2,0x2E) >>= //LD L,0x02
      MemoryController.poke(3,0x02) >>=
      MemoryController.poke(4,0x36) >>= //LD (HL),0xFF
      MemoryController.poke(5,0xFF)
    //when
    val sysInit=Z80SystemController(new Z80System(MemoryController(mem.get),sysBlank.get.registerController))
    val sysTest=sysInit >>= Z80SystemController.run(3)
    //then
    assert(sysTest.get.registerController.get("PC")==6)
    assert(sysTest.get.registerController.get("H")==1)
    assert(sysTest.get.registerController.get("L")==2)
    assert(sysTest.get.memoryController.get(0x0102)==0xFF)
    println(sysTest.get.memoryController.get.mem.slice(0,300))
    println(sysTest.get.registerController.get.reg)
  }
}
