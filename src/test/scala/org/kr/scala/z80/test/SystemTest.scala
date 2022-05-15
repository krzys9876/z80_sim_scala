package org.kr.scala.z80.test

import org.kr.scala.z80.system.{StateWatcher, MemoryChangeByte, MemoryChangeWord, RegisterChange, RegisterChangeRelative, Regs, Z80System}
import org.scalatest.funsuite.AnyFunSuite

class SystemTest extends AnyFunSuite {

  test("change memory (byte)") {
    //given
    val systemController=StateWatcher[Z80System](Z80System.blank)
    //when
    val afterState=(systemController >>== Z80System.change(new MemoryChangeByte(0x1234,0xFE))).get
    //then
    assert(afterState.memoryController.get(0x1234)==0xFE)
  }

  test("change memory (word)") {
    //given
    val systemController=StateWatcher[Z80System](Z80System.blank)
    //when
    val afterState=(systemController >>== Z80System.change(new MemoryChangeWord(0x1234,0xFEFD))).get
    //then
    assert(afterState.memoryController.get(0x1234)==0xFD)
    assert(afterState.memoryController.get(0x1235)==0xFE)
  }

  test("change register") {
    //given
    val system=Z80System.blank
    //when
    val afterState=(StateWatcher[Z80System](system) >>==
      Z80System.change(new RegisterChange(Regs.AF,0xFEFD)) >>==
      Z80System.change(new RegisterChange(Regs.SP,0xA1B2))).get
    //then
    assert(afterState.registerController.get(Regs.A)==0xFE)
    assert(afterState.registerController.get(Regs.F)==0xFD)
    assert(afterState.registerController.get(Regs.SP)==0xA1B2)
  }

  test("change register relative") {
    //given
    val initSystem=Z80System.blank
    val systemC=StateWatcher[Z80System](initSystem) >>== Z80System.change(new RegisterChange(Regs.SP,0x1002))
    //when
    val afterState=(systemC >>== Z80System.change(new RegisterChangeRelative(Regs.SP,0xF0))).get
    //then
    assert(afterState.registerController.get(Regs.SP)==0x10F2)
  }

  test("change list") {
    //given
    val system=Z80System.blank
    //when
    val chgList=List(
      new MemoryChangeByte(0xABCD,0xA1),
      new MemoryChangeWord(0x3210,0xB1C2),
      new RegisterChange(Regs.HL,0xFEDC))
    val afterState=(StateWatcher[Z80System](system) >>== Z80System.changeList(chgList)).get
    //then
    assert(afterState.registerController.get(Regs.HL)==0xFEDC)
    assert(afterState.memoryController.get(0xABCD)==0xA1)
    assert(afterState.memoryController.get(0x3210)==0xC2)
    assert(afterState.memoryController.get(0x3211)==0xB1)
  }

}
