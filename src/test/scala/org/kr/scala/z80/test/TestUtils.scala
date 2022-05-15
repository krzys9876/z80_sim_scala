package org.kr.scala.z80.test

import org.kr.scala.z80.opcode.OpCode
import org.kr.scala.z80.system.{BaseStateMonad, Debugger, Flag, Memory, RegSymbol, Register, Regs, Z80System}
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

  def prepareTest(regList: List[(RegSymbol, Int)], memList: List[(Int, Int)], steps:Int=1)
                 (implicit debugger:Debugger): BaseStateMonad[Z80System] = {
    prepareTestWith(BaseStateMonad[Z80System](Z80System.blank),regList,memList,steps)
  }

  def prepareTestWith(sysBlank:BaseStateMonad[Z80System], regList: List[(RegSymbol, Int)], memList: List[(Int, Int)], steps:Int)
                     (implicit debugger:Debugger): BaseStateMonad[Z80System] = {
    //given
    val reg = regList.foldLeft(sysBlank.get.registerController)(
      (regC, entry) => regC >>== Register.set(entry._1, entry._2)
    )

    val mem = memList.foldLeft(sysBlank.get.memoryController)(
      (memC, entry) => memC >>== Memory.poke(entry._1, entry._2)
    )
    //when
    val sysInit = BaseStateMonad[Z80System](new Z80System(mem, reg,sysBlank.get.outputController, sysBlank.get.inputController,0))
    sysInit >>== Z80System.run(debugger)(steps.toLong)
  }

  def testRegOrAddrWithFlags(regList: List[(RegSymbol, Int)], memList: List[(Int, Int)], resultReg: RegSymbol, resultAddr: Int,
                               result: Int, flagsAsString: String, pcAfter: Int = 1)
                            (implicit debugger: Debugger): Unit = {
    //given when
    val sysTest = TestUtils.prepareTest(regList, memList)
    //then
    assert(sysTest.get.registerController.get(Regs.PC) == pcAfter)
    (resultReg,resultAddr) match {
      case (reg,_) if reg!=Regs.NONE => assert(sysTest.get.registerController.get(resultReg) == result)
      case (_,addr) if addr!=OpCode.ANY => assert(sysTest.get.memoryController.get(resultAddr) == result)
      case _ =>
    }
    TestUtils.testFlags(sysTest.get.registerController.get, flagsAsString)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  def testRegAndAddrWordWithFlags(regList: List[(RegSymbol, Int)], memList: List[(Int, Int)],
                                  resultReg: RegSymbol, resultValReg: Int, resultAddr: Int, resultValMem: Int,
                                  flagsAsString: String, pcAfter: Int = 1)
                                 (implicit debugger: Debugger): Unit = {
    //given when
    val sysTest = TestUtils.prepareTest(regList, memList)
    //then
    assert(sysTest.get.registerController.get(Regs.PC) == pcAfter)
    assert(sysTest.get.registerController.get(resultReg) == resultValReg)
    assert(sysTest.get.memoryController.get(resultAddr) == (resultValMem & 0x00FF))
    assert(sysTest.get.memoryController.get(resultAddr+1) == ((resultValMem & 0xFF00) >> 8))
    TestUtils.testFlags(sysTest.get.registerController.get, flagsAsString)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

}
