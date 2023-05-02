package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, OpCodeBitManipulation, OpCodeSize, OpCodeSourceLocation, OpCodeTCycles}
import org.kr.scala.z80.system.{Debugger, Flag, Regs, Z80System}
import org.kr.scala.z80.utils.Z80Utils

sealed abstract class BitOperation

object BitOpType {
  case object Test extends BitOperation
  case object Reset extends BitOperation
  case object Set extends BitOperation
  case object None extends BitOperation
}

object BitManipulation extends OpCodeHandler {
  //Z80 manual p.55 - NOTE error in opCode: 0xCB, not 0xC8!
  override def handle(code:OpCode)(implicit system:Z80System, debugger:Debugger):(Z80System,Int, Int) = {
    val actualCode=castType[OpCode with OpCodeBitManipulation with OpCodeSourceLocation with OpCodeSize with OpCodeTCycles](code)
    val loc=actualCode.source

    val (value, flags) =
      handleBitManipulation(
        actualCode.operation,
        actualCode.bit,
        system.getValueFromLocation(loc),
        system.getFlags())

    (system
      .putValueToLocation2(loc,value)
      .changeRegister(Regs.F, flags)
        ,actualCode.size,actualCode.t)
  }

  private def handleBitManipulation(oper: BitOperation, bit: Int, prevValue: Int, prevFlags:Int):(Int,Int)={
    oper match {
      case BitOpType.Test =>
        val newZ= !Z80Utils.getBit(prevValue,bit)
        val newF=new Flag(prevFlags).set(Flag.Z,newZ).set(Flag.H).reset(Flag.N)()
        (prevValue,newF)
      case BitOpType.Reset => (Z80Utils.resetBit(prevValue,bit),prevFlags)
      case BitOpType.Set => (Z80Utils.setBit(prevValue,bit),prevFlags)
      case BitOpType.None => (prevValue,prevFlags)
    }
  }
}

