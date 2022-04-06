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
  }

}
