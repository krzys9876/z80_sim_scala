package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{Location, OpCode, OpCodeExchangeLocation, OpCodeSize}
import org.kr.scala.z80.system.{Debugger, MemoryChangeWord, RegisterChange, SystemChangeBase, Z80System}

class ExchangeLocationBase(val reg1: String, val reg2: String)

object ExchangeLocationBase {
  val empty: ExchangeLocationBase = new ExchangeLocationBase("", "")
}

class ExchangeLocation(override val reg1:String,override val reg2: String) extends ExchangeLocationBase(reg1,reg2)
class ExchangeLocationIndirect(override val reg1:String,override val reg2: String) extends ExchangeLocationBase(reg1,reg2)

object Exchange extends OpCodeHandler {
  override def handle(code:OpCode)(implicit system:Z80System, debugger:Debugger):(List[SystemChangeBase],Int) = {
    val actualCode=castType[OpCode with OpCodeExchangeLocation with OpCodeSize](code)
    val exchangeLocList=actualCode.exchange
    val instrSize=actualCode.size

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
    (chgList,instrSize)
  }
}
