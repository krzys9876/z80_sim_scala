package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{Location, OpCode, OpCodeExchangeLocation, OpCodeSize, OpCodeTCycles}
import org.kr.scala.z80.system.{Debugger, MemoryChangeWord, RegSymbol, RegisterChange, Regs, SystemChangeBase, Z80System}

class ExchangeLocationBase(val reg1: RegSymbol, val reg2: RegSymbol)

object ExchangeLocationBase {
  val empty: ExchangeLocationBase = new ExchangeLocationBase(Regs.NONE, Regs.NONE)
}

class ExchangeLocation(override val reg1:RegSymbol,override val reg2: RegSymbol) extends ExchangeLocationBase(reg1,reg2)
class ExchangeLocationIndirect(override val reg1:RegSymbol,override val reg2: RegSymbol) extends ExchangeLocationBase(reg1,reg2)

object Exchange extends OpCodeHandler {
  override def handle(code:OpCode)(implicit system:Z80System, debugger:Debugger):(List[SystemChangeBase],Int, Int) = {
    val actualCode=castType[OpCode with OpCodeExchangeLocation with OpCodeSize with OpCodeTCycles](code)
    val exchangeLocList=actualCode.exchange

    val chgList=exchangeLocList.flatMap(entry=>{
      entry match {
        case loc : ExchangeLocation =>
          List(new RegisterChange(loc.reg1,system.getRegValue(entry.reg2)),
            new RegisterChange(loc.reg2,system.getRegValue(entry.reg1)))
        case loc : ExchangeLocationIndirect =>
          val addressLoc1=system.getRegValue(loc.reg1)
          val memLoc1=system.getValueFromLocation(Location.registerAddr(loc.reg1,isWord=true))
          List(new MemoryChangeWord(addressLoc1,system.getRegValue(loc.reg2)),
            new RegisterChange(loc.reg2,memLoc1))
      }
    })
    (chgList,actualCode.size,actualCode.t)
  }
}
