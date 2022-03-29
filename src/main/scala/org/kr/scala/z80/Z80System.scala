package org.kr.scala.z80

class Z80System(val memoryController: MemoryController, val registerController: RegisterController) {
  def step:Z80System= {
    val PC = registerController.get("PC")
    val oper = memoryController.get(PC)
    oper match {
      case 0 =>
        val newPC = Z80Utils.add16bit(PC, 1)
        val newRegisters = registerController >>= RegisterController.set("PC", newPC)
        new Z80System(memoryController, RegisterController(newRegisters.get))
    }
  }
}

object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank)
}
