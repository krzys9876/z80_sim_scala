package org.kr.scala.z80.test

import org.kr.scala.z80.system.{MemoryController, RegisterController, Z80System, Z80SystemController}
import org.scalatest.funsuite.AnyFunSuite

class OpExchangeTest extends AnyFunSuite {
  test("always pass") {
    assert(1==1)
  }

  // TEST EXCHANGE
  test("run EX DE,HL") {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = sysBlank.get.registerController >>=
      RegisterController.set("D",0x01) >>=
      RegisterController.set("E",0x02) >>=
      RegisterController.set("H",0x03) >>=
      RegisterController.set("L",0x04)
    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,0xEB) // EX DE,HL
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), RegisterController(reg.get)))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("D") == 3)
    assert(sysTest.get.registerController.get("E") == 4)
    assert(sysTest.get.registerController.get("H") == 1)
    assert(sysTest.get.registerController.get("L") == 2)
    println(sysTest.get.memoryController.get.mem.slice(0,300))
    println(sysTest.get.registerController.get.reg)
  }




}
