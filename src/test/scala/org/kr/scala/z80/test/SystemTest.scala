package org.kr.scala.z80.test

import org.kr.scala.z80.system.{MemoryChangeByte, MemoryChangeWord, RegisterChange, RegisterChangeRelative, Regs, Z80SystemController}
import org.scalatest.funsuite.AnyFunSuite

class SystemTest extends AnyFunSuite {

  test("change memory (byte)") {
    //given
    val systemController=Z80SystemController.blank
    //when
    val afterState=systemController >>= Z80SystemController.change(new MemoryChangeByte(0x1234,0xFE))
    //then
    assert(afterState.get.memoryController.get(0x1234)==0xFE)
  }

  test("change memory (word)") {
    //given
    val systemController=Z80SystemController.blank
    //when
    val afterState=systemController >>= Z80SystemController.change(new MemoryChangeWord(0x1234,0xFEFD))
    //then
    assert(afterState.get.memoryController.get(0x1234)==0xFD)
    assert(afterState.get.memoryController.get(0x1235)==0xFE)
  }

  test("change register") {
    //given
    val systemController=Z80SystemController.blank
    //when
    val afterState=systemController >>= Z80SystemController.change(new RegisterChange(Regs.AF,0xFEFD)) >>=
      Z80SystemController.change(new RegisterChange(Regs.SP,0xA1B2))
    //then
    assert(afterState.get.registerController.get(Regs.A)==0xFE)
    assert(afterState.get.registerController.get(Regs.F)==0xFD)
    assert(afterState.get.registerController.get(Regs.SP)==0xA1B2)
  }

  test("change register relative") {
    //given
    val systemControllerInit=Z80SystemController.blank
    val systemController=systemControllerInit >>= Z80SystemController.change(new RegisterChange(Regs.SP,0x1002))
    //when
    val afterState=systemController >>= Z80SystemController.change(new RegisterChangeRelative(Regs.SP,0xF0))
    //then
    assert(afterState.get.registerController.get(Regs.SP)==0x10F2)
  }

  test("change list") {
    //given
    val systemController=Z80SystemController.blank
    //when
    val chgList=List(
      new MemoryChangeByte(0xABCD,0xA1),
      new MemoryChangeWord(0x3210,0xB1C2),
      new RegisterChange(Regs.HL,0xFEDC))
    val afterState=systemController >>= Z80SystemController.changeList(chgList)
    //then
    assert(afterState.get.registerController.get(Regs.HL)==0xFEDC)
    assert(afterState.get.memoryController.get(0xABCD)==0xA1)
    assert(afterState.get.memoryController.get(0x3210)==0xC2)
    assert(afterState.get.memoryController.get(0x3211)==0xB1)
  }

}
