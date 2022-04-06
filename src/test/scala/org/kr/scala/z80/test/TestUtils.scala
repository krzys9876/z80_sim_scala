package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Flag, MemoryController, Register, RegisterController, Z80System, Z80SystemController}
import org.kr.scala.z80.utils.Z80Utils

object TestUtils {
  def testFlags(reg: Register, flagsAsString: String): Unit = {
    assert(reg(Flag.S) == Z80Utils.getBitFromString(flagsAsString, Flag.S.bit))
    assert(reg(Flag.Z) == Z80Utils.getBitFromString(flagsAsString, Flag.Z.bit))
    assert(reg(Flag.H) == Z80Utils.getBitFromString(flagsAsString, Flag.H.bit))
    assert(reg(Flag.P) == Z80Utils.getBitFromString(flagsAsString, Flag.P.bit))
    assert(reg(Flag.N) == Z80Utils.getBitFromString(flagsAsString, Flag.N.bit))
    assert(reg(Flag.C) == Z80Utils.getBitFromString(flagsAsString, Flag.C.bit))
  }

  def prepareTest(regList: List[(String, Int)], memList: List[(Int, Int)]): Z80SystemController = {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = regList.foldLeft(sysBlank.get.registerController)((regC, entry) =>
      RegisterController((regC >>= RegisterController.set(entry._1, entry._2)).get))

    val mem = memList.foldLeft(sysBlank.get.memoryController)((memC, entry) =>
      MemoryController((memC >>= MemoryController.poke(entry._1, entry._2)).get))
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), RegisterController(reg.get)))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    Z80SystemController(sysTest.get)
  }
}
