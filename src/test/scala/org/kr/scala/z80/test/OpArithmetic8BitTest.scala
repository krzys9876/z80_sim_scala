package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Flag, MemoryController, RegisterController, Z80System, Z80SystemController}
import org.kr.scala.z80.utils.Z80Utils
import org.scalatest.funsuite.AnyFunSuite

class OpArithmetic8BitTest extends AnyFunSuite {
  test("always pass") {
    assert(1==1)
  }

  private def testArith(regList:List[(String,Int)],memList:List[(Int,Int)], result:Int,
                       flagsAsString:String,pcAfter:Int=1):Unit = {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = regList.foldLeft(sysBlank.get.registerController)((regC,entry)=>
      RegisterController((regC >>= RegisterController.set(entry._1,entry._2)).get))

    val mem = memList.foldLeft(sysBlank.get.memoryController)((memC,entry)=>
      MemoryController((memC >>= MemoryController.poke(entry._1,entry._2)).get))
    //when
    val sysInit = Z80SystemController(new Z80System(MemoryController(mem.get), RegisterController(reg.get)))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    //then
    assert(sysTest.get.registerController.get("PC") == pcAfter)
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
    testArith(List(("A",0x25),("B",0x3E)),List((0x0000,0x80)),0x63,"00_1_000")
    testArith(List(("A",0x63),("HL",0x0102)),List((0x0000,0x86),(0x0102,0x3E)),0xA1,"10_1_100")
    testArith(List(("A",0xA1),("IX",0x0202)),List((0x0000,0xDD),(0x0001,0x86),(0x0002,0x01),(0x0203,0x3E)),0xDF,"10_0_000",3)
    testArith(List(("A",0xDF),("IY",0x0108)),List((0x0000,0xFD),(0x0001,0x86),(0x0002,0xFE),(0x0106,0x3E)),0x1D,"00_1_001",3)

    testArith(List(("A",0x00)),List((0x0000,0xC6),(0x0001,0x40)),0x40,"00_0_000",2)
    testArith(List(("A",0x40),("L",0x40)),List((0x0000,0x85)),0x80,"10_0_100")
    testArith(List(("A",0x80),("B",0x40)),List((0x0000,0x80)),0xC0,"10_0_000")
    testArith(List(("A",0xC0),("C",0x40)),List((0x0000,0x81)),0x00,"01_0_001")

    testArith(List(("A",0x20),("D",0xF0)),List((0x0000,0x82)),0x10,"00_0_001")
    testArith(List(("A",0x10),("E",0xF0)),List((0x0000,0x83)),0x00,"01_0_001")
    testArith(List(("A",0x00),("H",0xF0)),List((0x0000,0x84)),0xF0,"10_0_000")
    testArith(List(("A",0xF0),("A",0xF0)),List((0x0000,0x87)),0xE0,"10_0_001")

    testArith(List(("A",0x80),("B",0xF0)),List((0x0000,0x80)),0x70,"00_0_101")
  }

  test("run SUB A,r") {
    // based on "real" Z80 emulator
    testArith(List(("A",0x1D),("B",0x3E)),List((0x0000,0x90)),0xDF,"10_1_011")
    testArith(List(("A",0xDF),("IX",0x0302)),List((0x0000,0xDD),(0x0001,0x96),(0x0002,0x01),(0x0303,0x3E)),0xA1,"10_0_010",3)
    testArith(List(("A",0xA1),("IY",0x0405)),List((0x0000,0xFD),(0x0001,0x96),(0x0002,0xFD),(0x0402,0x3E)),0x63,"00_1_110",3)
    testArith(List(("A",0x63),("HL",0x0503)),List((0x0000,0x96),(0x0503,0x3E)),0x25,"00_1_010")

    testArith(List(("A",0x00)),List((0x0000,0xD6),(0x0001,0x40)),0xC0,"10_0_011",2)
    testArith(List(("A",0xC0),("L",0x40)),List((0x0000,0x95)),0x80,"10_0_010")
    testArith(List(("A",0x80),("B",0x40)),List((0x0000,0x90)),0x40,"00_0_110")
    testArith(List(("A",0x40),("C",0x40)),List((0x0000,0x91)),0x00,"01_0_010")

    testArith(List(("A",0xE0),("D",0xF0)),List((0x0000,0x92)),0xF0,"10_0_011")
    testArith(List(("A",0xF0),("A",0xF0)),List((0x0000,0x97)),0x00,"01_0_010")
    testArith(List(("A",0x00),("H",0xF0)),List((0x0000,0x94)),0x10,"00_0_011")
    testArith(List(("A",0x10),("L",0xF0)),List((0x0000,0x95)),0x20,"00_0_011")

    testArith(List(("A",0x70),("B",0xF0)),List((0x0000,0x90)),0x80,"10_0_111")
  }
}

