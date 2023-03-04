package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, Flag, ImmutableMemory, MemoryHandler, RegSymbol, Register, Regs, StateWatcher, Z80System}
import org.kr.scala.z80.utils.{IntValue, OptionInt, Z80Utils}

object TestUtils {
  def testFlags(reg: Register, flagsAsString: String): Unit = {
    assert(reg(Flag.S) == Z80Utils.getBitFromString(flagsAsString, Flag.S.bit))
    assert(reg(Flag.Z) == Z80Utils.getBitFromString(flagsAsString, Flag.Z.bit))
    assert(reg(Flag.H) == Z80Utils.getBitFromString(flagsAsString, Flag.H.bit))
    assert(reg(Flag.P) == Z80Utils.getBitFromString(flagsAsString, Flag.P.bit))
    assert(reg(Flag.N) == Z80Utils.getBitFromString(flagsAsString, Flag.N.bit))
    assert(reg(Flag.C) == Z80Utils.getBitFromString(flagsAsString, Flag.C.bit))
  }

  implicit val memoryHandler:MemoryHandler = ImmutableMemory

  def prepareTest(regList: List[(RegSymbol, Int)], memList: List[(Int, Int)], steps:Int=1)
                 (implicit debugger:Debugger): StateWatcher[Z80System] = {
    prepareTestWith(StateWatcher[Z80System](Z80System.blank),regList,memList,steps)
  }

  def prepareTest8BitIO(regList: List[(RegSymbol, Int)], memList: List[(Int, Int)], steps: Int = 1)
                 (implicit debugger: Debugger): StateWatcher[Z80System] = {
    prepareTestWith(StateWatcher[Z80System](Z80System.blank8BitIO), regList, memList, steps)
  }

  def prepareTestWith(sysBlank:StateWatcher[Z80System], regList: List[(RegSymbol, Int)], memList: List[(Int, Int)], steps:Int)
                     (implicit debugger:Debugger): StateWatcher[Z80System] = {
    //given
    val reg = regList.foldLeft(StateWatcher(sysBlank.get.register))(
      (regC, entry) => regC >>== Register.set(entry._1, entry._2)
    )

    val mem = memList.foldLeft(StateWatcher(sysBlank.get.memory))(
      (memC, entry) => memC >>== ImmutableMemory.poke(entry._1, entry._2)
    )
    //when
    val sysInit = StateWatcher[Z80System](new Z80System(mem.get, reg.get,sysBlank.get.output, sysBlank.get.input,
      0, sysBlank.get.portMapping, sysBlank.state.interrupt))
    sysInit >>== Z80System.run(debugger)(steps.toLong)
  }

  def testRegOrAddrWithFlagsInt(regList: List[(RegSymbol, Int)], memList: List[(Int, Int)], resultReg: RegSymbol, resultAddr: Int,
                             result: Int, flagsAsString: String, pcAfter: Int = 1)
                            (implicit debugger: Debugger): Unit =
    testRegOrAddrWithFlags(regList,memList,resultReg,IntValue(resultAddr),IntValue(result),flagsAsString,pcAfter)

    def testRegOrAddrWithFlags(regList: List[(RegSymbol, Int)], memList: List[(Int, Int)], resultReg: RegSymbol, resultAddr: OptionInt,
                               result: OptionInt, flagsAsString: String, pcAfter: Int = 1)
                            (implicit debugger: Debugger): Unit = {
    //given when
    val sysTest = TestUtils.prepareTest(regList, memList)
    //then
    assert(sysTest.get.register(Regs.PC) == pcAfter)
    (resultReg,resultAddr) match {
      case (reg,_) if reg!=Regs.NONE => assert(sysTest.get.register(resultReg) == result())
      case (_,addr:IntValue) => assert(sysTest.get.memory(resultAddr()) == result())
      case _ =>
    }
    TestUtils.testFlags(sysTest.get.register, flagsAsString)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.register.reg)
  }

  def testRegAndAddrWordWithFlags(regList: List[(RegSymbol, Int)], memList: List[(Int, Int)],
                                  resultReg: RegSymbol, resultValReg: Int, resultAddr: Int, resultValMem: Int,
                                  flagsAsString: String, pcAfter: Int = 1)
                                 (implicit debugger: Debugger): Unit = {
    //given when
    val sysTest = TestUtils.prepareTest(regList, memList)
    //then
    assert(sysTest.get.register(Regs.PC) == pcAfter)
    assert(sysTest.get.register(resultReg) == resultValReg)
    assert(sysTest.get.memory(resultAddr) == (resultValMem & 0x00FF))
    assert(sysTest.get.memory(resultAddr+1) == ((resultValMem & 0xFF00) >> 8))
    TestUtils.testFlags(sysTest.get.register, flagsAsString)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.register.reg)
  }

}
