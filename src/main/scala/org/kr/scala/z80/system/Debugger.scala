package org.kr.scala.z80.system

trait Debugger {
  def info[Watched](before:Watched,after:Watched):Unit
  def error[Watched](message:String):Unit = println(message)
}

object DummyDebugger extends Debugger {
  override def info[Watched](before:Watched,after:Watched):Unit={}
}

object ConsoleDebugger extends Debugger {
  override def info[Watched](before:Watched,after:Watched):Unit= {
    after match {
      case outFile:OutputFile=>print(outFile.lastValue.toChar)
      case _ =>
    }
  }
}

object ConsoleDetailedDebugger extends Debugger {
  override def info[Watched](before:Watched,after:Watched):Unit= {
    (before,after) match {
      case (_:OutputFile,aft:OutputFile) =>print(f" | OUT port: 0x${aft.lastPort.num}%02X value: 0x${aft.lastValue}%02X |")
      case (bef:Z80System,aft:Z80System) =>
        println(f"PC:0x${bef.register(Regs.PC)}%04X | ${bef.currentOpCode} | before: ${bef.register.toString}")
        println(f" after: ${aft.register.toString}")
      case _ =>
    }
  }
}