package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, DummyChange, InputRefreshChange, OutputChange, SystemChangeBase, Z80System}

object InputOutput extends OperationSpec with OpCodeHandler {
  // requires lazy initialization
  lazy val portLocation: OpCodeMap[Location] = new OpCodeMap(OpCodes.destinationMap, Location.empty)
  lazy val valueLocation: OpCodeMap[Location] = new OpCodeMap(OpCodes.sourceMap, Location.empty)
  lazy val operation: OpCodeMap[InOutOperation] = new OpCodeMap(OpCodes.inOutOperationMap, InOutOpType.None)
  override lazy val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)

  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChangeBase], Int) = {
    val port = system.getValueFromLocation(portLocation.find(code))
    val chgList = operation.find(code).handle(system, port, valueLocation.find(code))
    (chgList, instSize.find(code))
  }
}

sealed abstract class InOutOperation(val name:String) {
  def handle(system:Z80System,port:Int,location:Location)(implicit debugger:Debugger):List[SystemChangeBase]
}

object InOutOpType {
  case object In extends InOutOperation("IN") {
    override def handle(system:Z80System,port:Int,location:Location)(implicit debugger:Debugger):List[SystemChangeBase]= {
      List(system.putValueToLocation(location,system.readPort(port)),
      new InputRefreshChange(port))
    }
  }
  case object Out extends InOutOperation("OUT") {
    override def handle(system:Z80System,port:Int,location:Location)(implicit debugger:Debugger):List[SystemChangeBase]=
      List(new OutputChange(port,system.getValueFromLocation(location)))
  }
  case object None extends InOutOperation("NONE") {
    override def handle(system:Z80System,port:Int,location:Location)(implicit debugger:Debugger):List[SystemChangeBase]=
      List(new DummyChange())
  }
}

