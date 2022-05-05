package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{DummyChange, OutputChange, SystemChangeBase, Z80System}

object InputOutput extends OperationSpec with OpCodeHandler {
  val portLocation: OpCodeMap[Location] = new OpCodeMap(OpCodes.destinationMap, Location.empty)
  val valueLocation: OpCodeMap[Location] = new OpCodeMap(OpCodes.sourceMap, Location.empty)
  val operation: OpCodeMap[InOutOperation] = new OpCodeMap(OpCodes.inOutOperationMap, InOutOpType.None)
  override val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)
  override lazy val isOper: OpCode => Boolean = opcode => operation.contains(opcode)

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    val port = system.getValueFromLocation(portLocation.find(code))
    val chgList = operation.find(code).handle(system, port, valueLocation.find(code))
    (List(chgList), instSize.find(code))
  }
}

sealed abstract class InOutOperation(val name:String) {
  def handle(system:Z80System,port:Int,location:Location):SystemChangeBase
}

object InOutOpType {
  case object In extends InOutOperation("IN") {
    override def handle(system:Z80System,port:Int,location:Location):SystemChangeBase=
      system.putValueToLocation(location,system.readPort(port))
  }
  case object Out extends InOutOperation("OUT") {
    override def handle(system:Z80System,port:Int,location:Location):SystemChangeBase=
      new OutputChange(port,system.getValueFromLocation(location))
  }
  case object None extends InOutOperation("NONE") {
    override def handle(system:Z80System,port:Int,location:Location):SystemChangeBase=
      new DummyChange()
  }
}

