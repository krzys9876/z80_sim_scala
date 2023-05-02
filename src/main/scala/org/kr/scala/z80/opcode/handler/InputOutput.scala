package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, DummyChange, InputRefreshChange, OutputChange, PortID, PortIDWithUpper, Regs, SystemChange, Z80System}

object InputOutput extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (Z80System,List[SystemChange], Int, Int) = {
    val actualCode=castType[OpCode with OpCodeInOut with OpCodeSourceLocation with OpCodeDestLocation with UpperAddressRegister with OpCodeSize with OpCodeTCycles](code)
    val port = system.getValueFromLocation(actualCode.destination)
    val upperPort = system.getValueFromLocation(actualCode.upperAddressSource)
    val actualPort = PortIDWithUpper(port,upperPort)
    val (chgSystem,chgList) = actualCode.operation.handle(system, actualPort, actualCode.source)
    (chgSystem,chgList, actualCode.size, actualCode.t)
  }
}

sealed abstract class InOutOperation(val name:String) {
  def handle(system:Z80System,port:PortIDWithUpper,location:Location)(implicit debugger:Debugger):(Z80System,List[SystemChange])
}

object InOutOpType {
  case object In extends InOutOperation("IN") {
    override def handle(system:Z80System, port:PortIDWithUpper, location:Location)(implicit debugger:Debugger):(Z80System,List[SystemChange])= {
      (system
        .putValueToLocation2(location,system.readPort(port))
        .refreshInput(port),
        DummyChange.blank
      )
    }
  }
  case object Out extends InOutOperation("OUT") {
    override def handle(system:Z80System,port:PortIDWithUpper,location:Location)(implicit debugger:Debugger):(Z80System,List[SystemChange])=
      (system
        .outputByte(port,system.getValueFromLocation(location)),
        DummyChange.blank
      )
  }
  case object None extends InOutOperation("NONE") {
    override def handle(system:Z80System,port:PortIDWithUpper,location:Location)(implicit debugger:Debugger):(Z80System,List[SystemChange])=
      (system,List(new DummyChange()))
  }
}

