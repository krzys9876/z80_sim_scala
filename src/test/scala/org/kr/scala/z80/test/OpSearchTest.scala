package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, Regs}
import org.kr.scala.z80.utils.Z80Utils
import org.scalatest.funsuite.AnyFunSuite

class OpSearchTest extends AnyFunSuite {

  implicit val debugger: Debugger = DummyDebugger

  test("run CPI") {
    //value not found, counter>0
    testCPx(0xA1,0xAC,0x0010,0xAB,0x0002,"00_0_111",1)
    //value not found, counter=0
    testCPx(0xA1,0xAC,0x0010,0xAB,0x0001,"00_0_011",1)
    //value found, counter>0
    testCPx(0xA1,0xAC,0x0010,0xAC,0x0002,"01_0_111",1)
    //value found, counter=0
    testCPx(0xA1,0xAC,0x0010,0xAC,0x0001,"01_0_011",1)
  }
  test("run CPD") {
    //value not found, counter>0
    testCPx(0xA9, 0xAB, 0x0011, 0xAC, 0x0002, "10_1_111", -1)
    //value not found, counter=0
    testCPx(0xA9, 0xAC, 0x0011, 0xAB, 0x0001, "00_0_011", -1)
    //value found, counter>0
    testCPx(0xA9, 0xAC, 0x0011, 0xAC, 0x0002, "01_0_111", -1)
    //value found, counter=0
    testCPx(0xA9, 0xAC, 0x0011, 0xAC, 0x0001, "01_0_011", -1)
  }
  test("run CPIR") {
    //value not found
    testCPxR(0xB1,0xBB,0x0010,List(0x12,0x34,0x56),0x0003,"00_0_011",1,3)
    //value found before end
    testCPxR(0xB1,0xBB,0x0010,List(0x12,0xBB,0xAA),0x0003,"01_0_111",1,2)
    //value found at end
    testCPxR(0xB1,0xAA,0x0010,List(0x12,0x12,0xAA),0x0003,"01_0_011",1,3)
  }
  test("run CPDR") {
    //value not found
    testCPxR(0xB9, 0xBB, 0x0012, List(0x12, 0x34, 0x56), 0x0003, "00_0_011", -1, 3)
    //value found before end
    testCPxR(0xB9, 0xBB, 0x0012, List(0x12, 0xBB, 0xAA), 0x0003, "01_0_111", -1, 2)
    //value found at end
    testCPxR(0xB9, 0xAA, 0x0012, List(0x12, 0x12, 0xAA), 0x0003, "01_0_011", -1, 3)
  }

  private def testCPx(opCode:Int,valA:Int,valHL:Int,memHL:Int,valBC:Int,flagsAsString:String,increment:Int):Unit = {
    //given
    //when
    val sysTest = TestUtils.prepareTest(List((Regs.HL, valHL), (Regs.A, valA), (Regs.BC, valBC), (Regs.F, 0xFF)),
      List((0x0000, 0xED), (0x0001, opCode),
        (valHL, memHL)))
    //then
    assert(sysTest.get.register(Regs.PC) == 2)
    assert(sysTest.get.register(Regs.HL) == Z80Utils.add16bit(valHL,increment))
    assert(sysTest.get.register(Regs.BC) == Z80Utils.add16bit(valBC,-1))
    TestUtils.testFlags(sysTest.get.register, flagsAsString)
  }

  private def testCPxR(opCode:Int,valA:Int,valHL:Int,memHL:List[Int],valBC:Int,flagsAsString:String,increment:Int,steps:Int):Unit = {
    //given
    //when
    val memList=(0 until valBC).map(i=>(Z80Utils.add16bit(valHL,i*increment),memHL(i)))
    val sysTest = TestUtils.prepareTest(List((Regs.HL, valHL), (Regs.A, valA), (Regs.BC, valBC), (Regs.F, 0xFF)),
      List((0x0000, 0xED), (0x0001, opCode))++memList,steps)
    //then
    assert(sysTest.get.register(Regs.PC) == 2)
    assert(sysTest.get.register(Regs.HL) == Z80Utils.add16bit(valHL, steps*increment))
    assert(sysTest.get.register(Regs.BC) == Z80Utils.add16bit(valBC, -steps))
    TestUtils.testFlags(sysTest.get.register, flagsAsString)
  }
}
