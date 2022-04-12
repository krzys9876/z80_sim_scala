package org.kr.scala.z80.test

import org.kr.scala.z80.system.{MemoryController, OutputController, RegisterController, Z80System, Z80SystemController}
import org.scalatest.funsuite.AnyFunSuite

class OpLoad16BitTypeTest extends AnyFunSuite {
  // TEST LOAD 16-BIT

  test("run LD dd,nn") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0, Vector(0x01,0x01,0x02)) >>= //LD BC,nn
      MemoryController.pokeMulti(3, Vector(0x11,0x03,0x04)) >>= //LD DE,nn
      MemoryController.pokeMulti(6, Vector(0x21,0x05,0x06)) >>= //LD HL,nn
      MemoryController.pokeMulti(9, Vector(0x31,0x07,0x08)) >>= //LD SP,nn
      MemoryController.pokeMulti(12, Vector(0xDD,0x21,0x09,0x0A)) >>= //LD IX,nn
      MemoryController.pokeMulti(16, Vector(0xFD,0x21,0x0B,0x0C)) //LD IY,nn
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg, OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(6)
    //then
    assert(sysTest.get.registerController.get("PC") == 20)
    assert(sysTest.get.registerController.get("B") == 2)
    assert(sysTest.get.registerController.get("C") == 1)
    assert(sysTest.get.registerController.get("D") == 4)
    assert(sysTest.get.registerController.get("E") == 3)
    assert(sysTest.get.registerController.get("H") == 6)
    assert(sysTest.get.registerController.get("L") == 5)
    assert(sysTest.get.registerController.get("SP") == 0x0807)
    assert(sysTest.get.registerController.get("IX") == 0x0A09)
    assert(sysTest.get.registerController.get("IY") == 0x0C0B)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD dd,(nn)") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0, Vector(0xED,0x4B,0x01,0x02)) >>= //LD BC,(nn)
      MemoryController.pokeMulti(4, Vector(0xED,0x5B,0x03,0x04)) >>= //LD DE,(nn)
      MemoryController.pokeMulti(8, Vector(0x2A,0x05,0x06)) >>= //LD HL,(nn)
      MemoryController.pokeMulti(11, Vector(0xED,0x7B,0x07,0x08)) >>= //LD SP,(nn)
      MemoryController.pokeMulti(15, Vector(0xDD,0x2A,0x09,0x0A)) >>= //LD IX,(nn)
      MemoryController.pokeMulti(19, Vector(0xFD,0x2A,0x0B,0x0C)) >>= //LD IY,(nn)
      MemoryController.pokeMulti(0x0201, Vector(0x10,0x11)) >>=
      MemoryController.pokeMulti(0x0403, Vector(0x12,0x13)) >>=
      MemoryController.pokeMulti(0x0605, Vector(0x14,0x15)) >>=
      MemoryController.pokeMulti(0x0807, Vector(0x16,0x17)) >>=
      MemoryController.pokeMulti(0x0A09, Vector(0x18,0x19)) >>=
      MemoryController.pokeMulti(0x0C0B, Vector(0x1A,0x1B))
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(6)
    //then
    assert(sysTest.get.registerController.get("PC") == 23)
    assert(sysTest.get.registerController.get("B") == 0x11)
    assert(sysTest.get.registerController.get("C") == 0x10)
    assert(sysTest.get.registerController.get("D") == 0x13)
    assert(sysTest.get.registerController.get("E") == 0x12)
    assert(sysTest.get.registerController.get("H") == 0x15)
    assert(sysTest.get.registerController.get("L") == 0x14)
    assert(sysTest.get.registerController.get("SP") == 0x1716)
    assert(sysTest.get.registerController.get("IX") == 0x1918)
    assert(sysTest.get.registerController.get("IY") == 0x1B1A)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD SP,HL") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("H",0x01) >>=
      RegisterController.set("L",0x02)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0, 0xF9)
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("SP") == 0x0102)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD SP,IX") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("IX",0x0304)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0, Vector(0xDD,0xF9))
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IX") == 0x0304)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD SP,IY") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("IY",0x0405)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0, Vector(0xFD,0xF9))
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IY") == 0x0405)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run POP AF") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("SP",0x0102)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,0xF1 ) >>=
      MemoryController.pokeMulti(0x0102,Vector(0xF1,0xF2))
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("A") == 0xF2)
    assert(sysTest.get.registerController.get("F") == 0xF1)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run POP BC") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("SP",0x0102)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,0xC1 ) >>=
      MemoryController.pokeMulti(0x0102,Vector(0xF3,0xF4))
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("B") == 0xF4)
    assert(sysTest.get.registerController.get("C") == 0xF3)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run POP DE") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("SP",0x0102)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,0xD1 ) >>=
      MemoryController.pokeMulti(0x0102,Vector(0xF5,0xF6))
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("D") == 0xF6)
    assert(sysTest.get.registerController.get("E") == 0xF5)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run POP HL") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("SP",0x0102)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,0xE1 ) >>=
      MemoryController.pokeMulti(0x0102,Vector(0xF7,0xF8))
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("H") == 0xF8)
    assert(sysTest.get.registerController.get("L") == 0xF7)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run POP IX") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("SP",0x0102)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0,Vector(0xDD,0xE1)) >>=
      MemoryController.pokeMulti(0x0102,Vector(0xF9,0xFA))
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IX") == 0xFAF9)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run POP IY") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("SP",0x0102)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0,Vector(0xFD,0xE1)) >>=
      MemoryController.pokeMulti(0x0102,Vector(0xFB,0xFC))
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IY") == 0xFCFB)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run LD (nn),dd") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("B",0x11) >>=
      RegisterController.set("C",0x12)  >>=
      RegisterController.set("D",0x13) >>=
      RegisterController.set("E",0x14) >>=
      RegisterController.set("H",0x15) >>=
      RegisterController.set("L",0x16)  >>=
      RegisterController.set("SP",0x1817) >>=
      RegisterController.set("IX",0x1A19) >>=
      RegisterController.set("IY",0x1C1B)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0,Vector(0xED,0x43,0x02,0x01)) >>=
      MemoryController.pokeMulti(4,Vector(0xED,0x53,0x04,0x03)) >>=
      MemoryController.pokeMulti(8,Vector(0x22,0x06,0x05)) >>=
      MemoryController.pokeMulti(11,Vector(0xED,0x73,0x08,0x07)) >>=
      MemoryController.pokeMulti(15,Vector(0xDD,0x22,0x0A,0x09)) >>=
      MemoryController.pokeMulti(19,Vector(0xFD,0x22,0x0C,0x0B))
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(6)
    //then
    assert(sysTest.get.registerController.get("PC") == 23)
    assert(sysTest.get.memoryController.get(0x0102) == 0x12)
    assert(sysTest.get.memoryController.get(0x0103) == 0x11)
    assert(sysTest.get.memoryController.get(0x0304) == 0x14)
    assert(sysTest.get.memoryController.get(0x0305) == 0x13)
    assert(sysTest.get.memoryController.get(0x0506) == 0x16)
    assert(sysTest.get.memoryController.get(0x0507) == 0x15)
    assert(sysTest.get.memoryController.get(0x0708) == 0x17)
    assert(sysTest.get.memoryController.get(0x0709) == 0x18)
    assert(sysTest.get.memoryController.get(0x090A) == 0x19)
    assert(sysTest.get.memoryController.get(0x090B) == 0x1A)
    assert(sysTest.get.memoryController.get(0x0B0C) == 0x1B)
    assert(sysTest.get.memoryController.get(0x0B0D) == 0x1C)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run PUSH qq") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("SP",0x0100) >>=
      RegisterController.set("A",0x02) >>=
      RegisterController.set("F",0x03) >>=
      RegisterController.set("B",0x04) >>=
      RegisterController.set("C",0x05) >>=
      RegisterController.set("D",0x06) >>=
      RegisterController.set("E",0x07) >>=
      RegisterController.set("H",0x08) >>=
      RegisterController.set("L",0x09) >>=
      RegisterController.set("IX",0x0A0B) >>=
      RegisterController.set("IY",0x00C0D)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,0xF5) >>= // PUSH AF
      MemoryController.poke(1,0xC5) >>= // PUSH BC
      MemoryController.poke(2,0xD5) >>= // PUSH DE
      MemoryController.poke(3,0xE5) >>= // PUSH HL
      MemoryController.pokeMulti(4,Vector(0xDD,0xE5)) >>= // PUSH IX
      MemoryController.pokeMulti(6,Vector(0xFD,0xE5)) // PUSH IY
    //when
    val sysInit = Z80SystemController(new Z80System(mem,reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(6)
    //then
    assert(sysTest.get.registerController.get("PC") == 8)
    assert(sysTest.get.memoryController.get(0x00FF) == 0x02)
    assert(sysTest.get.memoryController.get(0x00FE) == 0x03)
    assert(sysTest.get.memoryController.get(0x00FD) == 0x04)
    assert(sysTest.get.memoryController.get(0x00FC) == 0x05)
    assert(sysTest.get.memoryController.get(0x00FB) == 0x06)
    assert(sysTest.get.memoryController.get(0x00FA) == 0x07)
    assert(sysTest.get.memoryController.get(0x00F9) == 0x08)
    assert(sysTest.get.memoryController.get(0x00F8) == 0x09)
    assert(sysTest.get.memoryController.get(0x00F7) == 0x0A)
    assert(sysTest.get.memoryController.get(0x00F6) == 0x0B)
    assert(sysTest.get.memoryController.get(0x00F5) == 0x0C)
    assert(sysTest.get.memoryController.get(0x00F4) == 0x0D)
    assert(sysTest.get.registerController.get("SP") == 0x00F4)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }
}
