package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Flag, MemoryController, RegisterController, Z80System, Z80SystemController}
import org.kr.scala.z80.utils.Z80Utils
import org.scalatest.funsuite.AnyFunSuite

class OpArithmetic8BitTest extends AnyFunSuite {
  test("always pass") {
    assert(1==1)
  }

  private def testArithReg(opcode:Int, regList:List[(String,Int)], result:Int,
                       flagsAsString:String):Unit = {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = /*sysBlank.get.registerController >>=
      RegisterController.set("A",inputA) >>=
      RegisterController.set(regSymbol,inputR)*/
    regList.foldLeft(sysBlank.get.registerController)((regC,entry)=>RegisterController((regC >>= RegisterController.set(entry._1,entry._2)).get))


    val mem = sysBlank.get.memoryController >>=
      MemoryController.poke(0,opcode)
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), RegisterController(reg.get)))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("A") == result)
    assert(sysTest.get.registerController.get(Flag.S) == Z80Utils.getBitFromString(flagsAsString,Flag.S.bit))
    assert(sysTest.get.registerController.get(Flag.Z) == Z80Utils.getBitFromString(flagsAsString,Flag.Z.bit))
    assert(sysTest.get.registerController.get(Flag.H) == Z80Utils.getBitFromString(flagsAsString,Flag.H.bit))
    assert(sysTest.get.registerController.get(Flag.P) == Z80Utils.getBitFromString(flagsAsString,Flag.P.bit))
    assert(sysTest.get.registerController.get(Flag.N) == Z80Utils.getBitFromString(flagsAsString,Flag.N.bit))
    assert(sysTest.get.registerController.get(Flag.C) == Z80Utils.getBitFromString(flagsAsString,Flag.C.bit))
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)

  }

  test("run ADD A,r") {
    // based on "real" Z80 emulator
    testArithReg(0x80,List(("A",0x25),("B",0x3E)),0x63,"00_1_000")
    testArithReg(0x81,List(("A",0x63),("C",0x3E)),0xA1,"10_1_100")
    testArithReg(0x82,List(("A",0xA1),("D",0x3E)),0xDF,"10_0_000")
    testArithReg(0x83,List(("A",0xDF),("E",0x3E)),0x1D,"00_1_001")

    testArithReg(0x84,List(("A",0x00),("H",0x40)),0x40,"00_0_000")
    testArithReg(0x85,List(("A",0x40),("L",0x40)),0x80,"10_0_100")
    testArithReg(0x80,List(("A",0x80),("B",0x40)),0xC0,"10_0_000")
    testArithReg(0x81,List(("A",0xC0),("C",0x40)),0x00,"01_0_001")

    testArithReg(0x82,List(("A",0x20),("D",0xF0)),0x10,"00_0_001")
    testArithReg(0x83,List(("A",0x10),("E",0xF0)),0x00,"01_0_001")
    testArithReg(0x84,List(("A",0x00),("H",0xF0)),0xF0,"10_0_000")
    testArithReg(0x87,List(("A",0xF0),("A",0xF0)),0xE0,"10_0_001")

    testArithReg(0x80,List(("A",0x80),("B",0xF0)),0x70,"00_0_101")
  }

  test("run SUB A,r") {
    // based on "real" Z80 emulator
    testArithReg(0x90,List(("A",0x1D),("B",0x3E)),0xDF,"10_1_011")
    testArithReg(0x91,List(("A",0xDF),("C",0x3E)),0xA1,"10_0_010")
    testArithReg(0x92,List(("A",0xA1),("D",0x3E)),0x63,"00_1_110")
    testArithReg(0x93,List(("A",0x63),("E",0x3E)),0x25,"00_1_010")

    testArithReg(0x94,List(("A",0x00),("H",0x40)),0xC0,"10_0_011")
    testArithReg(0x95,List(("A",0xC0),("L",0x40)),0x80,"10_0_010")
    testArithReg(0x90,List(("A",0x80),("B",0x40)),0x40,"00_0_110")
    testArithReg(0x91,List(("A",0x40),("C",0x40)),0x00,"01_0_010")

    testArithReg(0x92,List(("A",0xE0),("D",0xF0)),0xF0,"10_0_011")
    testArithReg(0x97,List(("A",0xF0),("A",0xF0)),0x00,"01_0_010")
    testArithReg(0x94,List(("A",0x00),("H",0xF0)),0x10,"00_0_011")
    testArithReg(0x95,List(("A",0x10),("L",0xF0)),0x20,"00_0_011")

    testArithReg(0x90,List(("A",0x70),("B",0xF0)),0x80,"10_0_111")
  }
}

