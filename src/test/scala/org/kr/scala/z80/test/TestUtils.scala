package org.kr.scala.z80.test

import org.kr.scala.z80.opcode.OpCode
import org.kr.scala.z80.system.{Flag, InputController, MemoryController, OutputController, Register, RegisterController, Z80System, Z80SystemController}
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

  def prepareTest(regList: List[(String, Int)], memList: List[(Int, Int)], steps:Int=1): Z80SystemController = {
    prepareTestWith(Z80SystemController.blank,regList,memList,steps)
  }

  def prepareTestWith(sysBlank:Z80SystemController, regList: List[(String, Int)], memList: List[(Int, Int)], steps:Int): Z80SystemController = {
    //given
    val reg = regList.foldLeft(sysBlank.get.registerController)(
      (regC, entry) => regC >>= RegisterController.set(entry._1, entry._2)
    )

    val mem = memList.foldLeft(sysBlank.get.memoryController)(
      (memC, entry) => memC >>= MemoryController.poke(entry._1, entry._2)
    )
    //when
    val sysInit = Z80SystemController(new Z80System(mem, reg,sysBlank.get.outputController, sysBlank.get.inputController))
    val sysTest = sysInit >>= Z80SystemController.run(steps)
    Z80SystemController(sysTest.get)
  }

  def testRegOrAddrWithFlags(regList: List[(String, Int)], memList: List[(Int, Int)], resultReg: String, resultAddr: Int,
                               result: Int, flagsAsString: String, pcAfter: Int = 1): Unit = {
    //given when
    val sysTest = TestUtils.prepareTest(regList, memList)
    //then
    assert(sysTest.get.registerController.get("PC") == pcAfter)
    (resultReg,resultAddr) match {
      case (reg,_) if reg!="" => assert(sysTest.get.registerController.get(resultReg) == result)
      case (_,addr) if addr!=OpCode.ANY => assert(sysTest.get.memoryController.get(resultAddr) == result)
      case _ =>
    }
    TestUtils.testFlags(sysTest.get.registerController.get, flagsAsString)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  def testRegAndAddrWordWithFlags(regList: List[(String, Int)], memList: List[(Int, Int)],
                                  resultReg: String, resultValReg: Int, resultAddr: Int, resultValMem: Int,
                                  flagsAsString: String, pcAfter: Int = 1): Unit = {
    //given when
    val sysTest = TestUtils.prepareTest(regList, memList)
    //then
    assert(sysTest.get.registerController.get("PC") == pcAfter)
    assert(sysTest.get.registerController.get(resultReg) == resultValReg)
    assert(sysTest.get.memoryController.get(resultAddr) == (resultValMem & 0x00FF))
    assert(sysTest.get.memoryController.get(resultAddr+1) == ((resultValMem & 0xFF00) >> 8))
    TestUtils.testFlags(sysTest.get.registerController.get, flagsAsString)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

}
