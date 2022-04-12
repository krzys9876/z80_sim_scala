package org.kr.scala.z80.test

import org.kr.scala.z80.system.{MemoryController, OutputController, RegisterController, Z80System, Z80SystemController}
import org.scalatest.funsuite.AnyFunSuite

class OpExchangeTypeTest extends AnyFunSuite {
  test("always pass") {
    assert(1==1)
  }

  // TEST EXCHANGE
  test("run EX DE,HL") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("DE",0x0102) >>=
      RegisterController.set("HL",0x0304)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,0xEB) // EX DE,HL
    //when
    val sysInit = Z80SystemController(new Z80System(mem, reg, OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("D") == 3)
    assert(sysTest.get.registerController.get("E") == 4)
    assert(sysTest.get.registerController.get("H") == 1)
    assert(sysTest.get.registerController.get("L") == 2)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run EX AF,AF1") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("A",0x01) >>=
      RegisterController.set("F",0x02) >>=
      RegisterController.set("AF1",0x0304)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,0x08) // EX AF,AF1
    //when
    val sysInit = Z80SystemController(new Z80System(mem, reg, OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("A") == 3)
    assert(sysTest.get.registerController.get("F") == 4)
    assert(sysTest.get.registerController.get("AF1") == 0x0102)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run EXX") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("AF",0xF1F2) >>=
      RegisterController.set("BC",0xF3F4) >>=
      RegisterController.set("DE",0xF5F6) >>=
      RegisterController.set("HL",0xF7F8) >>=
      RegisterController.set("AF1",0xA1A2) >>=
      RegisterController.set("BC1",0xA3A4) >>=
      RegisterController.set("DE1",0xA5A6) >>=
      RegisterController.set("HL1",0xA7A8)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,0xD9) // EXX
    //when
    val sysInit = Z80SystemController(new Z80System(mem, reg, OutputController.blank ))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("AF") == 0xF1F2)
    assert(sysTest.get.registerController.get("AF1") == 0xA1A2)
    assert(sysTest.get.registerController.get("BC") == 0xA3A4)
    assert(sysTest.get.registerController.get("BC1") == 0xF3F4)
    assert(sysTest.get.registerController.get("DE") == 0xA5A6)
    assert(sysTest.get.registerController.get("DE1") == 0xF5F6)
    assert(sysTest.get.registerController.get("HL") == 0xA7A8)
    assert(sysTest.get.registerController.get("HL1") == 0xF7F8)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run EX (SP),HL") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("HL",0xA1A2) >>=
      RegisterController.set("SP",0x0102)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,0xE3) >>= // EX (SP),HL
      MemoryController.pokeMulti(0x0102,Vector(0xF1,0xF2))
    //when
    val sysInit = Z80SystemController(new Z80System(mem, reg, OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("HL") == 0xF2F1)
    assert(sysTest.get.registerController.get("SP") == 0x0102)
    assert(sysTest.get.memoryController.get(0x0102)==0xA2)
    assert(sysTest.get.memoryController.get(0x0103)==0xA1)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run EX (SP),IX") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("IX",0xB1B2) >>=
      RegisterController.set("SP",0x0103)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0,Vector(0xDD,0xE3)) >>= // EX (SP),IX
      MemoryController.pokeMulti(0x0103,Vector(0xE1,0xE2))
    //when
    val sysInit = Z80SystemController(new Z80System(mem, reg, OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IX") == 0xE2E1)
    assert(sysTest.get.registerController.get("SP") == 0x0103)
    assert(sysTest.get.memoryController.get(0x0103)==0xB2)
    assert(sysTest.get.memoryController.get(0x0104)==0xB1)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run EX (SP),IY") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("IY",0xB1B2) >>=
      RegisterController.set("SP",0x0103)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0,Vector(0xFD,0xE3)) >>= // EX (SP),IY
      MemoryController.pokeMulti(0x0103,Vector(0xE1,0xE2))
    //when
    val sysInit = Z80SystemController(new Z80System(mem, reg, OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IY") == 0xE2E1)
    assert(sysTest.get.registerController.get("SP") == 0x0103)
    assert(sysTest.get.memoryController.get(0x0103)==0xB2)
    assert(sysTest.get.memoryController.get(0x0104)==0xB1)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }
}
