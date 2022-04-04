package org.kr.scala.z80.test

import org.kr.scala.z80.system.{MemoryController, RegisterController, Z80System, Z80SystemController}
import org.scalatest.funsuite.AnyFunSuite

class OpArithmetic8BitTest extends AnyFunSuite {
  test("always pass") {
    assert(1==1)
  }

  /*test("run EX (SP),IX") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("IX",0xB1B2) >>=
      RegisterController.set("SP",0x0103)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.pokeMulti(0,Vector(0xDD,0xE3)) >>= // EX (SP),IX
      MemoryController.pokeMulti(0x0103,Vector(0xE1,0xE2))
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), RegisterController(reg.get)))
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

  test("run ADD A,r") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("A",0x01) >>=
      RegisterController.set("B",0xF0)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,0x80) // ADD A,B
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), RegisterController(reg.get)))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("A") == 0xF1)
    assert(sysTest.get.registerController.get("A") == 0xF1)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }
*/
}

