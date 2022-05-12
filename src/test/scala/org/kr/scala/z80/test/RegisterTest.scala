package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Flag, RegisterController, Regs}
import org.scalatest.funsuite.AnyFunSuite

class RegisterTest extends AnyFunSuite {
  test("always pass") {
    1==1
  }

  test("init blank register") {
    //given
    val registerController=RegisterController.blank
    //when
    //then
    assert(registerController.get(Regs.A).equals(0))
    assert(registerController.get(Regs.PC).equals(0))
    assert(registerController.get(Regs.F).equals(0xFF))
  }

  test("set register") {
    //given
    val registerController=RegisterController.blank
    //when
    val afterState=registerController >>= RegisterController.set(Regs.IX,255) >>= RegisterController.set(Regs.IY,56)
    //then
    assert(afterState.get(Regs.IX).equals(255))
    assert(afterState.get(Regs.IY).equals(56))
    assert(afterState.get(Regs.PC).equals(0))
  }

  test("set register direct function declaration") {
    //given
    val registerController=RegisterController.blank
    //when
    val afterState=registerController >>= (reg=>RegisterController(reg.set(Regs.B,36)))
    //then
    assert(afterState.get(Regs.B).equals(36))
    assert(afterState.get(Regs.A).equals(0))
  }

  test("set register pairs") {
    //given
    val registerController=RegisterController.blank
    //when
    val afterState=registerController >>=
      RegisterController.set(Regs.AF,0x0102) >>= RegisterController.set(Regs.BC,0x0304) >>=
      RegisterController.set(Regs.DE,0x0506) >>= RegisterController.set(Regs.HL,0x0708) >>=
      RegisterController.set(Regs.AF1,0xF1F2) >>= RegisterController.set(Regs.BC1,0xF3F4) >>=
      RegisterController.set(Regs.DE1,0xF5F6) >>= RegisterController.set(Regs.HL1,0xF7F8)
    //then
    assert(afterState.get(Regs.A).equals(1))
    assert(afterState.get(Regs.F).equals(2))
    assert(afterState.get(Regs.B).equals(3))
    assert(afterState.get(Regs.C).equals(4))
    assert(afterState.get(Regs.D).equals(5))
    assert(afterState.get(Regs.E).equals(6))
    assert(afterState.get(Regs.H).equals(7))
    assert(afterState.get(Regs.L).equals(8))
    assert(afterState.get(Regs.AF1).equals(0xF1F2))
    assert(afterState.get(Regs.BC1).equals(0xF3F4))
    assert(afterState.get(Regs.DE1).equals(0xF5F6))
    assert(afterState.get(Regs.HL1).equals(0xF7F8))
  }

  test("get register pairs") {
    //given
    val registerController=RegisterController.blank
    //when
    val afterState=registerController >>=
      RegisterController.set(Regs.A,1) >>= RegisterController.set(Regs.F,2) >>=
      RegisterController.set(Regs.B,3) >>= RegisterController.set(Regs.C,4) >>=
      RegisterController.set(Regs.D,5) >>= RegisterController.set(Regs.E,6) >>=
      RegisterController.set(Regs.H,7) >>= RegisterController.set(Regs.L,8)
    //then
    assert(afterState.get(Regs.AF).equals(0x0102))
    assert(afterState.get(Regs.BC).equals(0x0304))
    assert(afterState.get(Regs.DE).equals(0x0506))
    assert(afterState.get(Regs.HL).equals(0x0708))
  }

  test("check flags 1") {
    //given
    val registerController=RegisterController.blank
    //when
    val afterState=registerController >>=
      RegisterController.set(Regs.F,0xAA)
    //then
    assert(afterState.get(Regs.F)==0xAA)
    assert(afterState.get(Flag.S))
    assert(!afterState.get(Flag.Z))
    assert(!afterState.get(Flag.H))
    assert(!afterState.get(Flag.P))
    assert(afterState.get(Flag.N))
    assert(!afterState.get(Flag.C))
  }

  test("check flags 2") {
    //given
    val registerController=RegisterController.blank
    //when
    val afterState=registerController >>=
      RegisterController.set(Regs.F,0x55)
    //then
    assert(afterState.get(Regs.F)==0x55)
    assert(!afterState.get(Flag.S))
    assert(afterState.get(Flag.Z))
    assert(afterState.get(Flag.H))
    assert(afterState.get(Flag.P))
    assert(!afterState.get(Flag.N))
    assert(afterState.get(Flag.C))
  }
}
