package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, OpCodeExchangeLocation, OpCodeSize, OpCodeTCycles, RegisterAddrLocation}
import org.kr.scala.z80.system.{Debugger, RegSymbol, Regs, Z80System}
import org.kr.scala.z80.utils.IntValue

class ExchangeLocationBase(val reg1: RegSymbol, val reg2: RegSymbol)

object ExchangeLocationBase {
  val empty: ExchangeLocationBase = new ExchangeLocationBase(Regs.NONE, Regs.NONE)
}

class ExchangeLocation(override val reg1:RegSymbol,override val reg2: RegSymbol) extends ExchangeLocationBase(reg1,reg2)
class ExchangeLocationIndirect(override val reg1:RegSymbol,override val reg2: RegSymbol) extends ExchangeLocationBase(reg1,reg2)

object Exchange extends OpCodeHandler {
  override def handle(code:OpCode)(implicit system:Z80System, debugger:Debugger):(Z80System,Int, Int) = {
    val actualCode=castType[OpCode with OpCodeExchangeLocation with OpCodeSize with OpCodeTCycles](code)
    val exchangeLocList=actualCode.exchange
    val chgSystem=exchangeLocList.foldLeft(system)((sys,entry)=>
      entry match {
        case loc: ExchangeLocation =>
          val reg1Value=sys.getRegValue(entry.reg1)
          val reg2Value=sys.getRegValue(entry.reg2)
          sys
            .changeRegister(loc.reg1, reg2Value)
            .changeRegister(loc.reg2, reg1Value)
        case loc: ExchangeLocationIndirect =>
          val addressLoc1 = system.getRegValue(loc.reg1)
          val memLoc1 = system.getValueFromLocation(RegisterAddrLocation(loc.reg1, isWord = true))
          sys.changeMemoryWord(addressLoc1, system.getRegValue(loc.reg2)).changeRegister(loc.reg2, memLoc1)
      })
    (chgSystem,actualCode.size,actualCode.t)
  }
}
