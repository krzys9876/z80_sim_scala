package org.kr.scala.z80.test

import org.kr.scala.z80.system.{BaseStateMonad, Flag, Register, Regs}
import org.scalatest.funsuite.AnyFunSuite

class RegisterTest extends AnyFunSuite {
  test("always pass") {
    1==1
  }

  test("init blank register") {
    //given
    val registerController=BaseStateMonad[Register](Register.blank)
    //when
    //then
    assert(registerController.get(Regs.A).equals(0))
    assert(registerController.get(Regs.PC).equals(0))
    assert(registerController.get(Regs.F).equals(0xFF))
  }

  test("set register") {
    //given
    val registerController=BaseStateMonad[Register](Register.blank)
    //when
    val afterState=registerController >>== Register.set(Regs.IX,255) >>== Register.set(Regs.IY,56)
    //then
    assert(afterState.get(Regs.IX).equals(255))
    assert(afterState.get(Regs.IY).equals(56))
    assert(afterState.get(Regs.PC).equals(0))
  }

  test("set register direct function declaration") {
    //given
    val registerController=BaseStateMonad[Register](Register.blank)
    //when
    val afterState=registerController >>= (reg=>BaseStateMonad[Register](reg.set(Regs.B,36)))
    //then
    assert(afterState.get(Regs.B).equals(36))
    assert(afterState.get(Regs.A).equals(0))
  }

  test("set register pairs") {
    //given
    val registerController=BaseStateMonad[Register](Register.blank)
    //when
    val afterState=registerController >>==
      Register.set(Regs.AF,0x0102) >>== Register.set(Regs.BC,0x0304) >>==
      Register.set(Regs.DE,0x0506) >>== Register.set(Regs.HL,0x0708) >>==
      Register.set(Regs.AF1,0xF1F2) >>== Register.set(Regs.BC1,0xF3F4) >>==
      Register.set(Regs.DE1,0xF5F6) >>== Register.set(Regs.HL1,0xF7F8)
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
    val registerController=BaseStateMonad[Register](Register.blank)
    //when
    val afterState=registerController >>==
      Register.set(Regs.A,1) >>== Register.set(Regs.F,2) >>==
      Register.set(Regs.B,3) >>== Register.set(Regs.C,4) >>==
      Register.set(Regs.D,5) >>== Register.set(Regs.E,6) >>==
      Register.set(Regs.H,7) >>== Register.set(Regs.L,8)
    //then
    assert(afterState.get(Regs.AF).equals(0x0102))
    assert(afterState.get(Regs.BC).equals(0x0304))
    assert(afterState.get(Regs.DE).equals(0x0506))
    assert(afterState.get(Regs.HL).equals(0x0708))
  }

  test("check flags 1") {
    //given
    val registerController=BaseStateMonad[Register](Register.blank)
    //when
    val afterState=registerController >>== Register.set(Regs.F,0xAA)
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
    val registerController=BaseStateMonad[Register](Register.blank)
    //when
    val afterState=registerController >>== Register.set(Regs.F,0x55)
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
