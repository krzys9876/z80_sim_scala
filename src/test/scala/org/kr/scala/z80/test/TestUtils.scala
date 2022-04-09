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
    val reg = regList.foldLeft(sysBlank.get.registerController)(
      (regC, entry) => regC >>= RegisterController.set(entry._1, entry._2)
    )

    val mem = memList.foldLeft(sysBlank.get.memoryController)(
      (memC, entry) => memC >>= MemoryController.poke(entry._1, entry._2)
    )
    //when
    val sysInit = Z80SystemController(new Z80System(mem, reg))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    Z80SystemController(sysTest.get)
  }

  def testRegOrAddrWithFlags(regList: List[(String, Int)], memList: List[(Int, Int)], resultReg: String, resultAddr: Int,
                               result: Int, flagsAsString: String, pcAfter: Int = 1): Unit = {
    //given when
    val sysTest = TestUtils.prepareTest(regList, memList)
    //then
    assert(sysTest.get.registerController.get("PC") == pcAfter)
    val resultTest=
      if(resultReg!="") sysTest.get.registerController.get(resultReg)
      else sysTest.get.memoryController.get(resultAddr)
    assert(resultTest == result)
    TestUtils.testFlags(sysTest.get.registerController.get, flagsAsString)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

}