package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, PortID, PortIDWithUpper, Regs, Z80System}

object InputOutput extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (Z80System, Int, Int) = {
    val actualCode=castType[OpCode with OpCodeInOut with OpCodeSourceLocation with OpCodeDestLocation with UpperAddressRegister with OpCodeSize with OpCodeTCycles](code)
    val port = system.getValueFromLocation(actualCode.destination)
    val upperPort = system.getValueFromLocation(actualCode.upperAddressSource)
    val actualPort = PortIDWithUpper(port,upperPort)
    val chgSystem = actualCode.operation.handle(system, actualPort, actualCode.source)
    (chgSystem,actualCode.size, actualCode.t)
  }
}

sealed abstract class InOutOperation(val name:String) {
  def handle(system:Z80System,port:PortIDWithUpper,location:Location)(implicit debugger:Debugger):Z80System
}

object InOutOpType {
  case object In extends InOutOperation("IN") {
    override def handle(system:Z80System, port:PortIDWithUpper, location:Location)(implicit debugger:Debugger):Z80System=
      system
        .putValueToLocation(location,system.readPort(port))
        .refreshInput(port)
  }
  case object Out extends InOutOperation("OUT") {
    override def handle(system:Z80System,port:PortIDWithUpper,location:Location)(implicit debugger:Debugger):Z80System=
      system.outputByte(port,system.getValueFromLocation(location))
  }
  case object None extends InOutOperation("NONE") {
    override def handle(system:Z80System,port:PortIDWithUpper,location:Location)(implicit debugger:Debugger):Z80System= system
  }
}

