package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, OpCodeSize, OpCodeTCycles, RegisterAddrLocation, RegisterLocation, TransferDirection, TransferRepeatable}
import org.kr.scala.z80.system.{Debugger, Flag, RegisterChange, Regs, SystemChange, Z80System}
import org.kr.scala.z80.utils.Z80Utils

object BlockTransfer extends OpCodeHandler {
  override def handle(code:OpCode)(implicit system:Z80System, debugger:Debugger):(List[SystemChange],Int, Int) = {
    val actualCode=castType[OpCode with TransferDirection with TransferRepeatable with OpCodeSize with OpCodeTCycles](code)
    // Source
    val sourceAddr=system.getValueFromLocation(RegisterLocation(Regs.HL))
    val newSourceAddr=Z80Utils.add16bit(sourceAddr,actualCode.increment)
    val sourceValue = system.getValueFromLocation(RegisterAddrLocation(Regs.HL))
    // Destination
    val destValueLoc=RegisterAddrLocation(Regs.DE)
    val destAddr=system.getValueFromLocation(RegisterLocation(Regs.DE))
    val newDestAddr=Z80Utils.add16bit(destAddr,actualCode.increment)
    // Counter
    val counterValue=system.getValueFromLocation(RegisterLocation(Regs.BC))
    val newCounterValue=Z80Utils.add16bit(counterValue,-1)
    // Flags
    val baseFlags=system.getFlags.reset(Flag.H).reset(Flag.N)
    val newFlags=if(newCounterValue==0) baseFlags.set(Flag.P) else baseFlags.reset(Flag.P)

    val chgList=
      List(system.putValueToLocation(destValueLoc,sourceValue), // (HL)->(DE)
        new RegisterChange(Regs.HL,newSourceAddr), new RegisterChange(Regs.DE,newDestAddr), //HL+1, DE+1
        new RegisterChange(Regs.BC, newCounterValue), //BC-1
        new RegisterChange(Regs.F, newFlags.value)) // flags

    //NOTE: returning forwardPC=0 effectively means repeating the same instruction,
    // which is what is required here until new counter is 0
    val forwardPC=if(actualCode.repeat && newCounterValue>0) 0 else actualCode.size
    //NOTE: for last iteration (or single execution) it takes 16 cycles, otherwise 21 cycles. t=21, tConditional=-5
    val tCycles=actualCode.t + (if(newCounterValue==0) actualCode.tConditional else 0)
    (chgList,forwardPC,tCycles)
  }
}
