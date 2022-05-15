package org.kr.scala.z80.system

trait Debugger {
  def before[Watched](watched:Watched):Unit= {}
  def after[Watched](watched:Watched):Unit= {}
}

object DummyDebugger extends Debugger {
}

object ConsoleDebugger extends Debugger {
  override def after[Watched](watched:Watched):Unit= {
    watched match {
      case outFile:OutputFile=>print(outFile.lastValue.toChar)
      case _ =>
    }
  }
}

object ConsoleDetailedDebugger extends Debugger {
  override def before[Watched](watched:Watched):Unit= {
    watched match {
      case system:Z80System=>
        val pc=system.register(Regs.PC)
        val opCode=system.currentOpCode
        val regs=system.register.toString
        println(f"PC:0x$pc%04X | $opCode | before: $regs")
      case _ =>
    }
  }

  override def after[Watched](watched:Watched):Unit= {
    watched match {
      case outFile:OutputFile=>print(f" | OUT port: 0x${outFile.lastPort}%02X value: 0x${outFile.lastValue}%02X |")
      case system:Z80System=>
        val regs=system.register.toString
        println(f" after: $regs")
      case _ =>
    }
  }
}