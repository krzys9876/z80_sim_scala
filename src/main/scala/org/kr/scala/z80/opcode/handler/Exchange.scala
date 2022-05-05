package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, OpCodeMap, OpCodes, OperationSpec, Location}
import org.kr.scala.z80.system.{MemoryChangeWord, RegisterChange, SystemChangeBase, Z80System}

class ExchangeLocationBase(val reg1: String, val reg2: String)

object ExchangeLocationBase {
  val empty: ExchangeLocationBase = new ExchangeLocationBase("", "")
}


class ExchangeLocation(override val reg1:String,override val reg2: String) extends ExchangeLocationBase(reg1,reg2)
class ExchangeLocationIndirect(override val reg1:String,override val reg2: String) extends ExchangeLocationBase(reg1,reg2)



object Exchange extends OperationSpec with OpCodeHandler {
  lazy val exchangeLoc: OpCodeMap[List[ExchangeLocationBase]] = new OpCodeMap(OpCodes.exchangeMap, List(ExchangeLocationBase.empty))
  override lazy val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)

  override def handle(opcode:OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int) = {
    val exchangeLocList=Exchange.exchangeLoc.find(opcode)
    val instrSize=Exchange.instSize.find(opcode)

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
