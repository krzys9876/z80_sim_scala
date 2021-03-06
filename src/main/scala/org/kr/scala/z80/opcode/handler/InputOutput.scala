package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, DummyChange, InputRefreshChange, OutputChange, SystemChange, Z80System}
import org.kr.scala.z80.utils.IntValue

object InputOutput extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChange], Int, Int) = {
    val actualCode=castType[OpCode with OpCodeInOut with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize with OpCodeTCycles](code)
    val port = system.getValueFromLocation(actualCode.destination)
    val chgList = actualCode.operation.handle(system, port, actualCode.source)
    (chgList, actualCode.size, actualCode.t)
  }
}

sealed abstract class InOutOperation(val name:String) {
  def handle(system:Z80System,port:Int,location:Location)(implicit debugger:Debugger):List[SystemChange]
}

object InOutOpType {
  case object In extends InOutOperation("IN") {
    override def handle(system:Z80System,port:Int,location:Location)(implicit debugger:Debugger):List[SystemChange]= {
      List(system.putValueToLocation(location,system.readPort(port)),
      new InputRefreshChange(port))
    }
  }
  case object Out extends InOutOperation("OUT") {
    override def handle(system:Z80System,port:Int,location:Location)(implicit debugger:Debugger):List[SystemChange]=
      List(new OutputChange(port,system.getValueFromLocation(location)))
  }
  case object None extends InOutOperation("NONE") {
    override def handle(system:Z80System,port:Int,location:Location)(implicit debugger:Debugger):List[SystemChange]=
      List(new DummyChange())
  }
}

