package org.kr.scala.z80.test

import org.kr.scala.z80.{MemoryController, RegisterController, Z80System, Z80SystemController}
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
    val reg=sysBlank.get.registerController >>=
      RegisterController.set("H",0x01) >>=
      RegisterController.set("L",0x02)
    val mem=sysBlank.get.memoryController >>=
      MemoryController.poke(0,0x4E) >>= //LD C,(HL)
      MemoryController.poke(0x0102,0xFE) //(HL)
    //when
    val sysInit=Z80SystemController(new Z80System(MemoryController(mem.get),RegisterController(reg.get)))
    val sysTest=sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC")==1)
    assert(sysTest.get.registerController.get("C")==0xFE)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD (HL),E") {
    //given
    val sysBlank=Z80SystemController.blank
    val reg=sysBlank.get.registerController >>=
      RegisterController.set("H",0x01) >>=
      RegisterController.set("L",0x02) >>=
      RegisterController.set("E",0xFF)
    val mem=sysBlank.get.memoryController >>=
      MemoryController.poke(0,0x73) //LD (HL),E
    //when
    val sysInit=Z80SystemController(new Z80System(MemoryController(mem.get),RegisterController(reg.get)))
    val sysTest=sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC")==1)
    assert(sysTest.get.memoryController.get(0x0102)==0xFF)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD r,(IX+d) | LD r,(IY+d)") {
    //given
    val sysBlank=Z80SystemController.blank
    val reg=sysBlank.get.registerController >>=
      RegisterController.set("IX",0x0101) >>=
      RegisterController.set("IY",0x0103)
    val mem=sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0,Vector(0xDD,0x56,0x05)) >>= //LD D,(IX+5)
      MemoryController.pokeMulti(3,Vector(0xFD,0x5E,0x04)) >>= //LD E,(IY+4)
      MemoryController.poke(0x0106,0xFF) >>=
      MemoryController.poke(0x0107,0xFE)
    //when
    val sysInit=Z80SystemController(new Z80System(MemoryController(mem.get),RegisterController(reg.get)))
    val sysTest=sysInit >>= Z80SystemController.run(2)
    //then
    assert(sysTest.get.registerController.get("PC")==6)
    assert(sysTest.get.registerController.get("D")==0xFF)
    assert(sysTest.get.registerController.get("E")==0xFE)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD (IX+d),r | LD (IY+d),r") {
    //given
    val sysBlank=Z80SystemController.blank
    val reg=sysBlank.get.registerController >>=
      RegisterController.set("IX",0x0100) >>=
      RegisterController.set("IY",0x0101) >>=
      RegisterController.set("A",0x01) >>=
      RegisterController.set("B",0x02)
    val mem=sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0,Vector(0xDD,0x77,0x03)) >>= //LD (IX+3),A
      MemoryController.pokeMulti(3,Vector(0xFD,0x70,0x04)) >>= //LD (IY+4),B
      MemoryController.poke(0x0103,0x01) >>=
      MemoryController.poke(0x0105,0x02)
    //when
    val sysInit=Z80SystemController(new Z80System(MemoryController(mem.get),RegisterController(reg.get)))
    val sysTest=sysInit >>= Z80SystemController.run(2)
    //then
    assert(sysTest.get.registerController.get("PC")==6)
    assert(sysTest.get.memoryController.get(0x0103)==0x01)
    assert(sysTest.get.memoryController.get(0x0105)==0x02)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD (HL),n") {
    //given
    val sysBlank=Z80SystemController.blank
    val reg=sysBlank.get.registerController >>=
      RegisterController.set("H",0x01) >>=
      RegisterController.set("L",0x02)
    val mem=sysBlank.get.memoryController >>=
      MemoryController.poke(0,0x36) >>= //LD (HL),0xFF
      MemoryController.poke(1,0xFF)
    //when
    val sysInit=Z80SystemController(new Z80System(MemoryController(mem.get),RegisterController(reg.get)))
    val sysTest=sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC")==2)
    assert(sysTest.get.registerController.get("H")==1)
    assert(sysTest.get.registerController.get("L")==2)
    assert(sysTest.get.memoryController.get(0x0102)==0xFF)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD (IX+d),n | LD (IY+d),n") {
    //given
    val sysBlank=Z80SystemController.blank
    val reg=sysBlank.get.registerController >>=
      RegisterController.set("IX",0x0100) >>=
      RegisterController.set("IY",0x0101)
    val mem=sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0,Vector(0xDD,0x36,0x02,0xFF)) >>= //LD (IX+2),0xFF
      MemoryController.pokeMulti(4,Vector(0xFD,0x36,0x03,0xFE)) //LD (IY+3),0xFE
    //when
    val sysInit=Z80SystemController(new Z80System(MemoryController(mem.get),RegisterController(reg.get)))
    val sysTest=sysInit >>= Z80SystemController.run(2)
    //then
    assert(sysTest.get.registerController.get("PC")==8)
    assert(sysTest.get.memoryController.get(0x0102)==0xFF)
    assert(sysTest.get.memoryController.get(0x0104)==0xFE)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD A,(BC)") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("B", 0x01) >>=
      RegisterController.set("C", 0x02)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0, 0x0A) >>= //LD A,(BC)
      MemoryController.poke(0x0102,0xFE)
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), RegisterController(reg.get)))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("A") == 0xFE)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD A,(DE)") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("D", 0x01) >>=
      RegisterController.set("E", 0x03)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0, 0x1A) >>= //LD A,(DE)
      MemoryController.poke(0x0103,0xFD)
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), RegisterController(reg.get)))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("A") == 0xFD)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD (BC),A") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("B", 0x01) >>=
      RegisterController.set("C", 0x02) >>=
      RegisterController.set("A", 0xFF)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0, 0x02) //LD (BC),A
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), RegisterController(reg.get)))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.memoryController.get(0x0102) == 0xFF)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD (DE),A") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("D", 0x02) >>=
      RegisterController.set("E", 0x03) >>=
      RegisterController.set("A", 0xFE)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0, 0x12) //LD (DE),A
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), RegisterController(reg.get)))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.memoryController.get(0x0203) == 0xFE)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD A,(nn)") {
    //given
    val sysBlank = Z80SystemController.blank
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0, Vector(0x3A,0x02,0x01)) >>= //LD A,(nn)
      MemoryController.poke(0x0102, 0xFE)
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), sysBlank.get.registerController))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 3)
    assert(sysTest.get.registerController.get("A") == 0xFE)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD (nn),A") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("A", 0xFF)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0, Vector(0x32,0x02,0x01)) //LD (nn),A
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), RegisterController(reg.get)))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 3)
    assert(sysTest.get.memoryController.get(0x0102) == 0xFF)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

}
