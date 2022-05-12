package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, OpCodeBitManipulation, OpCodeSize, OpCodeSourceLocation}
import org.kr.scala.z80.system.{Debugger, Flag, RegisterChange, Regs, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

sealed abstract class BitOperation(val name: String)

object BitOpType {
  case object Test extends BitOperation("TEST")
  case object Reset extends BitOperation("RESET")
  case object Set extends BitOperation("SET")
  case object None extends BitOperation("NONE")
}

object BitManipulation extends OpCodeHandler {
  //Z80 manual p.55 - NOTE error in opCode: 0xCB, not 0xC8!
  override def handle(code:OpCode)(implicit system:Z80System, debugger:Debugger):(List[SystemChangeBase],Int) = {
    val actualCode=castType[OpCode with OpCodeBitManipulation with OpCodeSourceLocation with OpCodeSize](code)
    val loc=actualCode.source

    val (value, flags) =
      handleBitManipulation(
        actualCode.operation,
        actualCode.bit,
        system.getValueFromLocation(loc),
        system.getFlags())
    val change=List(system.putValueToLocation(loc,value),new RegisterChange(Regs.F, flags))

    (change,actualCode.size)
  }

  private def handleBitManipulation(oper: BitOperation, bit: Int, prevValue: Int, prevFlags:Int):(Int,Int)={
    oper match {
      case BitOpType.Test =>
        val newZ= !Z80Utils.getBit(prevValue,bit)
        val newF=new Flag(prevFlags).set(Flag.Z,newZ).set(Flag.H).reset(Flag.N)()
        (prevValue,newF)
      case BitOpType.Reset => (Z80Utils.resetBit(prevValue,bit),prevFlags)
      case BitOpType.Set => (Z80Utils.setBit(prevValue,bit),prevFlags)
    }
  }
}

