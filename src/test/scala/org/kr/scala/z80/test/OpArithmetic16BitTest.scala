package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, RegSymbol, Regs}
import org.scalatest.funsuite.AnyFunSuite

class OpArithmetic16BitTest extends AnyFunSuite{

  implicit val debugger:Debugger=DummyDebugger

  private def testArithReg(regList: List[(RegSymbol, Int)], memList: List[(Int, Int)], resultReg: RegSymbol,
                               result: Int, flagsAsString: String, pcAfter: Int = 1): Unit = {
    //given when
    val sysTest = TestUtils.prepareTest(regList, memList)
    //then
    assert(sysTest.get.register(Regs.PC) == pcAfter)
    assert(sysTest.get.register(resultReg) == result)
    TestUtils.testFlags(sysTest.get.register, flagsAsString)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.register.reg)
  }

  test("run ADD qq,ss") {
    testArithReg(List((Regs.F, 0x00), (Regs.HL, 0x1234),(Regs.BC, 0x0001)), List((0x0000, 0x09)), Regs.HL,0x1235, "00_0_000")
    testArithReg(List((Regs.F, 0xFF), (Regs.HL, 0x1234),(Regs.DE, 0x0001)), List((0x0000, 0x19)), Regs.HL,0x1235, "11_0_100")
    testArithReg(List((Regs.F, 0x00), (Regs.HL, 0x7FFF),(Regs.SP, 0x0010)), List((0x0000, 0x39)), Regs.HL,0x800F, "00_1_000")

    testArithReg(List((Regs.F, 0x00), (Regs.IX, 0xFFFF),(Regs.BC, 0x0010)), List((0x0000, 0xDD),(0x0001,0x09)), Regs.IX,0x000F, "00_1_001",2)
    testArithReg(List((Regs.F, 0x00), (Regs.IX, 0xFFFF),(Regs.DE, 0x0010)), List((0x0000, 0xDD),(0x0001,0x19)), Regs.IX,0x000F, "00_1_001",2)
    testArithReg(List((Regs.F, 0x00), (Regs.IX, 0xFFFF),(Regs.SP, 0x0010)), List((0x0000, 0xDD),(0x0001,0x39)), Regs.IX,0x000F, "00_1_001",2)
    testArithReg(List((Regs.F, 0x00), (Regs.IY, 0xFFFF),(Regs.BC, 0x0010)), List((0x0000, 0xFD),(0x0001,0x09)), Regs.IY,0x000F, "00_1_001",2)
    testArithReg(List((Regs.F, 0x00), (Regs.IY, 0xFFFF),(Regs.DE, 0x0010)), List((0x0000, 0xFD),(0x0001,0x19)), Regs.IY,0x000F, "00_1_001",2)
    testArithReg(List((Regs.F, 0x00), (Regs.IY, 0xFFFF),(Regs.SP, 0x0010)), List((0x0000, 0xFD),(0x0001,0x39)), Regs.IY,0x000F, "00_1_001",2)

    testArithReg(List((Regs.F, 0x00), (Regs.HL, 0xFFFE)), List((0x0000, 0x29)), Regs.HL,0xFFFC, "00_1_001")
    testArithReg(List((Regs.F, 0x00), (Regs.IX, 0xFFFE)), List((0x0000, 0xDD),(0x0001,0x29)), Regs.IX,0xFFFC, "00_1_001",2)
    testArithReg(List((Regs.F, 0x00), (Regs.IY, 0xFFFE)), List((0x0000, 0xFD),(0x0001,0x29)), Regs.IY,0xFFFC, "00_1_001",2)
  }

  test("run ADC qq,ss") {
    testArithReg(List((Regs.F, 0x00), (Regs.HL, 0x1234),(Regs.BC, 0x0001)), List((0x0000, 0xED),(0x0001, 0x4A)), Regs.HL,0x1235, "00_0_000",2)
    testArithReg(List((Regs.F, 0xFF), (Regs.HL, 0x1234),(Regs.DE, 0x0001)), List((0x0000, 0xED),(0x0001, 0x5A)), Regs.HL,0x1236, "00_0_000",2)
    testArithReg(List((Regs.F, 0xFF), (Regs.HL, 0x0FFE),(Regs.DE, 0x0001)), List((0x0000, 0xED),(0x0001, 0x5A)), Regs.HL,0x1000, "00_1_000",2)
    testArithReg(List((Regs.F, 0x00), (Regs.HL, 0x7FFF),(Regs.SP, 0x0010)), List((0x0000, 0xED),(0x0001, 0x7A)), Regs.HL,0x800F, "10_1_100",2)
    testArithReg(List((Regs.F, 0x00), (Regs.HL, 0xFFFE)), List((0x0000, 0xED),(0x0001, 0x6A)), Regs.HL,0xFFFC, "10_1_001",2)
    testArithReg(List((Regs.F, 0x00), (Regs.HL, 0xFFFF),(Regs.BC, 0x0001)), List((0x0000, 0xED),(0x0001, 0x4A)), Regs.HL,0x0000, "01_1_001",2)
  }

  test("run SBC qq,ss") {
    testArithReg(List((Regs.F, 0x00), (Regs.HL, 0x1235),(Regs.BC, 0x0001)), List((0x0000, 0xED),(0x0001, 0x42)), Regs.HL,0x1234, "00_0_010",2)
    testArithReg(List((Regs.F, 0xFF), (Regs.HL, 0x1235),(Regs.DE, 0x0001)), List((0x0000, 0xED),(0x0001, 0x52)), Regs.HL,0x1233, "00_0_010",2)
    testArithReg(List((Regs.F, 0xFF), (Regs.HL, 0x1001),(Regs.DE, 0x0001)), List((0x0000, 0xED),(0x0001, 0x52)), Regs.HL,0x0FFF, "00_1_010",2)
    testArithReg(List((Regs.F, 0x00), (Regs.HL, 0x8000),(Regs.SP, 0x0010)), List((0x0000, 0xED),(0x0001, 0x72)), Regs.HL,0x7FF0, "00_1_110",2)
    testArithReg(List((Regs.F, 0x00), (Regs.HL, 0xFFFF)), List((0x0000, 0xED),(0x0001, 0x62)), Regs.HL,0x0000, "01_0_010",2)
    testArithReg(List((Regs.F, 0x00), (Regs.HL, 0x1234),(Regs.BC, 0x1235)), List((0x0000, 0xED),(0x0001, 0x42)), Regs.HL,0xFFFF, "10_1_011",2)
  }

  test("run INC qq") {
    testArithReg(List((Regs.F, 0x00), (Regs.BC, 0x0000)), List((0x0000, 0x03)), Regs.BC,0x0001, "00_0_000")
    testArithReg(List((Regs.F, 0xFF), (Regs.DE, 0xFFFF)), List((0x0000, 0x13)), Regs.DE,0x0000, "11_1_111")
    testArithReg(List((Regs.F, 0xFF), (Regs.HL, 0x7FFF)), List((0x0000, 0x23)), Regs.HL,0x8000, "11_1_111")
    testArithReg(List((Regs.F, 0x00), (Regs.SP, 0x7FFF)), List((0x0000, 0x33)), Regs.SP,0x8000, "00_0_000")
    testArithReg(List((Regs.F, 0x00), (Regs.IX, 0x7FFF)), List((0x0000, 0xDD),(0x0001, 0x23)), Regs.IX,0x8000, "00_0_000",2)
    testArithReg(List((Regs.F, 0x00), (Regs.IY, 0x7FFF)), List((0x0000, 0xFD),(0x0001, 0x23)), Regs.IY,0x8000, "00_0_000",2)
  }

  test("run DEC qq") {
    testArithReg(List((Regs.F, 0x00), (Regs.BC, 0x0001)), List((0x0000, 0x0B)), Regs.BC,0x0000, "00_0_000")
    testArithReg(List((Regs.F, 0xFF), (Regs.DE, 0x0000)), List((0x0000, 0x1B)), Regs.DE,0xFFFF, "11_1_111")
    testArithReg(List((Regs.F, 0xFF), (Regs.HL, 0x8000)), List((0x0000, 0x2B)), Regs.HL,0x7FFF, "11_1_111")
    testArithReg(List((Regs.F, 0x00), (Regs.SP, 0x8000)), List((0x0000, 0x3B)), Regs.SP,0x7FFF, "00_0_000")
    testArithReg(List((Regs.F, 0x00), (Regs.IX, 0x8000)), List((0x0000, 0xDD),(0x0001, 0x2B)), Regs.IX,0x7FFF, "00_0_000",2)
    testArithReg(List((Regs.F, 0x00), (Regs.IY, 0x8000)), List((0x0000, 0xFD),(0x0001, 0x2B)), Regs.IY,0x7FFF, "00_0_000",2)
  }

}
