package org.kr.scala.z80

class Z80System(val memoryController: MemoryController, val registerController: RegisterController) {
  def step:Z80System= {
    val PC = registerController.get("PC")
    val oper = memoryController.get(PC)
    oper match {
      //NOP
      case 0 =>
        makeNew(1)
      // LD r,r1 : 01xxxyyyy, yyy->xxx - register code
      case x if (x & 0x40)==0x40  =>
        val regFrom=Register.getRegCode(x & 0x07)
        val regTo=Register.getRegCode((x >> 3) & 0x07)
        makeNew(
          registerController >>= RegisterController.set(regTo,registerController.get(regFrom)),
          1)
      // LD r,n : 00xxx110, xxx - register code
      case x if (x & 0x06)==0x06  =>
        val reg=Register.getRegCode((x >> 3) & 0x07)
        makeNew(
          registerController >>= RegisterController.set(reg,memoryController.get(Z80Utils.add16bit(PC,1))),
          2)
      case _ => throw new UnknownOperationException(f"Unknown operation $oper at $PC")
    }
  }
  def makeNew(forwardPC:Int):Z80System =
    new Z80System(memoryController,RegisterController(registerController.get.movePC(forwardPC)))
  def makeNew(newRegister:BaseStateMonad[Register], forwardPC:Int):Z80System =
    new Z80System(memoryController,RegisterController(newRegister.get.movePC(forwardPC)))
}

object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank)
  def apply(source:Z80System):Z80System=new Z80System(source.memoryController,source.registerController)
}

class UnknownOperationException(message : String) extends Exception(message) {}
