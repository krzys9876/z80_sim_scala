package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system._
import org.kr.scala.z80.utils.{IntValue, OptionInt, Z80Utils}

object Search extends OpCodeHandler {
  override def handle(code:OpCode)(implicit system:Z80System, debugger:Debugger):(Z80System,Int, Int) = {
    val actualCode=castType[OpCode with TransferDirection with TransferRepeatable with OpCodeSize with OpCodeTCycles](code)

    val searchedLoc = RegisterAddrLocation(Regs.HL)
    val searchedValue = system.getOptionalValueFromLocation(searchedLoc)
    val prevFlags = system.getFlags
    val referenceLocation = RegisterLocation(Regs.A)
    val referenceValue = system.getValueFromLocation(referenceLocation)
    // Use CP operation as it does almost exactly the same
    val (_, flags) = Comp8b.calcAll(ArithmeticOpInput(referenceValue, searchedValue, prevFlags))

    // Searched address
    val searchedAddr = system.getValueFromLocation(RegisterLocation(Regs.HL))
    val newSearchedAddr = Z80Utils.add16bit(searchedAddr, actualCode.increment)
    // Counter
    val counterValue = system.getValueFromLocation(RegisterLocation(Regs.BC))
    val newCounterValue = Z80Utils.add16bit(counterValue, -1)
    // Flags
    // revert flag C (CPxx do not affect C flag, CP does)
    val flagsWithC = flags.set(Flag.C,prevFlags(Flag.C))
    // set P flag if new BC is 0
    val newFlags = flagsWithC.set(Flag.P,newCounterValue != 0)

    val chgSystem = system
      .changeRegister(Regs.HL, newSearchedAddr) //HL+1
      .changeRegister(Regs.BC, newCounterValue) //BC-1
      .changeRegister(Regs.F, newFlags.value) // flags

    //NOTE: returning forwardPC=0 effectively means repeating the same instruction,
    // which is what is required here until new counter is 0
    val forwardPC=if(actualCode.repeat && newCounterValue>0 && !newFlags(Flag.Z)) 0 else actualCode.size
    //NOTE: for last iteration (or single execution) it takes 16 cycles, otherwise 21 cycles. t=21, tConditional=-5
    val tCycles=actualCode.t + (if(newCounterValue==0) actualCode.tConditional else 0)
    (chgSystem,forwardPC,tCycles)
  }
}
