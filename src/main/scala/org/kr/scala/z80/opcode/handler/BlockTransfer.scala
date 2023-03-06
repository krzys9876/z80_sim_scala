package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{FlagCalculatorBase, OpCode, OpCodeExchangeLocation, OpCodeSize, OpCodeTCycles, RegisterAddrLocation, RegisterLocation, TransferDirection, TransferRepeatable}
import org.kr.scala.z80.system.{Debugger, Flag, MemoryChangeByte, MemoryChangeWord, RegSymbol, RegisterChange, Regs, SystemChange, Z80System}
import org.kr.scala.z80.utils.{IntValue, Z80Utils}

object BlockTransfer extends OpCodeHandler {
  override def handle(code:OpCode)(implicit system:Z80System, debugger:Debugger):(List[SystemChange],Int, Int) = {
    val actualCode=castType[OpCode with TransferDirection with TransferRepeatable with OpCodeSize with OpCodeTCycles](code)
    val sourceAddrLoc = RegisterLocation(Regs.HL)
    val sourceValueLoc = RegisterAddrLocation(Regs.HL)
    val sourceAddr=system.getValueFromLocation(sourceAddrLoc)
    val newSource=if(actualCode.increase) Z80Utils.add16bit(sourceAddr,1) else Z80Utils.add16bit(sourceAddr,-1)
    val sourceValue = system.getValueFromLocation(sourceValueLoc)
    val destAddrLoc=RegisterLocation(Regs.DE)
    val destValueLoc=RegisterAddrLocation(Regs.DE)
    val destAddr=system.getValueFromLocation(destAddrLoc)
    val newDest=if(actualCode.increase) Z80Utils.add16bit(destAddr,1) else Z80Utils.add16bit(destAddr,-1)
    val counter=RegisterLocation(Regs.BC)
    val counterValue=system.getValueFromLocation(counter)
    val newCounterValue=Z80Utils.add16bit(counterValue,-1)
    val baseFlags=system.getFlags.reset(Flag.H).reset(Flag.N)
    val newFlags=if(newCounterValue==0) baseFlags.set(Flag.P) else baseFlags.reset(Flag.P)


    //TODO: repeat
    val chgList=
      List(system.putValueToLocation(destValueLoc,sourceValue),
        new RegisterChange(Regs.HL,newSource),
        new RegisterChange(Regs.DE,newDest),
        new RegisterChange(Regs.BC, newCounterValue),
        new RegisterChange(Regs.F, newFlags.value))

    (chgList,actualCode.size,actualCode.t)
  }
}
