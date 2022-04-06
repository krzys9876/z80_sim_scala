package org.kr.scala.z80.test

import org.scalatest.funsuite.AnyFunSuite

class OpArithmetic16BitTest extends AnyFunSuite{

  private def testArithReg(regList: List[(String, Int)], memList: List[(Int, Int)], resultReg: String,
                               result: Int, flagsAsString: String, pcAfter: Int = 1): Unit = {
    //given when
    val sysTest = TestUtils.prepareTest(regList, memList)
    //then
    assert(sysTest.get.registerController.get("PC") == pcAfter)
    assert(sysTest.get.registerController.get(resultReg) == result)
    TestUtils.testFlags(sysTest.get.registerController.get, flagsAsString)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run ADD qq,ss") {
    testArithReg(List(("F", 0x00), ("HL", 0x1234),("BC", 0x0001)), List((0x0000, 0x09)), "HL",0x1235, "00_0_000")
    testArithReg(List(("F", 0xFF), ("HL", 0x1234),("DE", 0x0001)), List((0x0000, 0x19)), "HL",0x1235, "11_0_100")
    testArithReg(List(("F", 0x00), ("HL", 0x7FFF),("SP", 0x0010)), List((0x0000, 0x39)), "HL",0x800F, "00_1_000")

    testArithReg(List(("F", 0x00), ("IX", 0xFFFF),("BC", 0x0010)), List((0x0000, 0xDD),(0x0001,0x09)), "IX",0x000F, "00_1_001",2)
    testArithReg(List(("F", 0x00), ("IX", 0xFFFF),("DE", 0x0010)), List((0x0000, 0xDD),(0x0001,0x19)), "IX",0x000F, "00_1_001",2)
    testArithReg(List(("F", 0x00), ("IX", 0xFFFF),("SP", 0x0010)), List((0x0000, 0xDD),(0x0001,0x39)), "IX",0x000F, "00_1_001",2)
    testArithReg(List(("F", 0x00), ("IY", 0xFFFF),("BC", 0x0010)), List((0x0000, 0xFD),(0x0001,0x09)), "IY",0x000F, "00_1_001",2)
    testArithReg(List(("F", 0x00), ("IY", 0xFFFF),("DE", 0x0010)), List((0x0000, 0xFD),(0x0001,0x19)), "IY",0x000F, "00_1_001",2)
    testArithReg(List(("F", 0x00), ("IY", 0xFFFF),("SP", 0x0010)), List((0x0000, 0xFD),(0x0001,0x39)), "IY",0x000F, "00_1_001",2)

    testArithReg(List(("F", 0x00), ("HL", 0xFFFE)), List((0x0000, 0x29)), "HL",0xFFFC, "00_1_001")
    testArithReg(List(("F", 0x00), ("IX", 0xFFFE)), List((0x0000, 0xDD),(0x0001,0x29)), "IX",0xFFFC, "00_1_001",2)
    testArithReg(List(("F", 0x00), ("IY", 0xFFFE)), List((0x0000, 0xFD),(0x0001,0x29)), "IY",0xFFFC, "00_1_001",2)
  }

  test("run ADC qq,ss") {
    testArithReg(List(("F", 0x00), ("HL", 0x1234),("BC", 0x0001)), List((0x0000, 0xED),(0x0001, 0x4A)), "HL",0x1235, "00_0_000",2)
    testArithReg(List(("F", 0xFF), ("HL", 0x1234),("DE", 0x0001)), List((0x0000, 0xED),(0x0001, 0x5A)), "HL",0x1236, "00_0_000",2)
    testArithReg(List(("F", 0x00), ("HL", 0x7FFF),("SP", 0x0010)), List((0x0000, 0xED),(0x0001, 0x7A)), "HL",0x800F, "10_1_100",2)
    testArithReg(List(("F", 0x00), ("HL", 0xFFFE)), List((0x0000, 0xED),(0x0001, 0x6A)), "HL",0xFFFC, "10_1_001",2)
    testArithReg(List(("F", 0x00), ("HL", 0xFFFF),("BC", 0x0001)), List((0x0000, 0xED),(0x0001, 0x4A)), "HL",0x0000, "01_1_001",2)
  }

  test("run SBC qq,ss") {
    testArithReg(List(("F", 0x00), ("HL", 0x1235),("BC", 0x0001)), List((0x0000, 0xED),(0x0001, 0x42)), "HL",0x1234, "00_0_010",2)
    testArithReg(List(("F", 0xFF), ("HL", 0x1235),("DE", 0x0001)), List((0x0000, 0xED),(0x0001, 0x52)), "HL",0x1233, "00_0_010",2)
    testArithReg(List(("F", 0x00), ("HL", 0x8000),("SP", 0x0010)), List((0x0000, 0xED),(0x0001, 0x72)), "HL",0x7FF0, "00_1_110",2)
    testArithReg(List(("F", 0x00), ("HL", 0xFFFF)), List((0x0000, 0xED),(0x0001, 0x62)), "HL",0x0000, "01_0_010",2)
    testArithReg(List(("F", 0x00), ("HL", 0x1234),("BC", 0x1235)), List((0x0000, 0xED),(0x0001, 0x42)), "HL",0xFFFF, "10_1_011",2)
  }

}
