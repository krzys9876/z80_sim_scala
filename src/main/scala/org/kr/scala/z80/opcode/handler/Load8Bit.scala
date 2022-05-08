package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, SystemChangeBase, Z80System}

sealed abstract class Load8BitOpType(val name:String)

object Load8BitOpType {
  case object Load extends Load8BitOpType("LOAD")
  case object None extends Load8BitOpType("NONE")
}

object Load8Bit extends LoadSpec with OpCodeHandler {
  //TODO: evaluate if limiting map size improves speed
  lazy val handledBy:List[OpCode]=OpCodes.handlerMap.m.filter({case(_,handler)=>handler.equals(this)}).keys.toList

  override lazy val sourceLoc: OpCodeMap[Location] = new OpCodeMap(OpCodes.sourceMap.filter({case(op,_)=>handledBy.contains(op)}), Location.empty)
  override lazy val destLoc: OpCodeMap[Location] = new OpCodeMap(OpCodes.destinationMap.filter({case(op,_)=>handledBy.contains(op)}), Location.empty)
  override lazy val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap.filter({case(op,_)=>handledBy.contains(op)}), 0)

  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChangeBase], Int) = {
    doHandle(code.asInstanceOf[OpCode with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize])

    //val value = system.getValueFromLocation(sourceLoc.find(code))
    //(List(system.putValueToLocation(destLoc.find(code), value)), instSize.find(code))
  }

  def doHandle(code: OpCode with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize)
                     (implicit system: Z80System, debugger:Debugger): (List[SystemChangeBase], Int) = {
    val value = system.getValueFromLocation(code.source)
    (List(system.putValueToLocation(code.destination, value)), code.size)
  }
}
