package org.kr.scala.z80

class Z80System(val memoryController: MemoryController, val registerController: RegisterController) {
  def step:Z80System= {
    val PC = registerController.get("PC")
    val oper = memoryController.get(PC)
    oper match {
      //NOP
      case 0 =>
        returnNewNOP
      // LD r,r1 : 01xxxyyy, yyy->xxx - register code
      case x if (x & 0xC0)==0x40 && List(7,0,1,2,3,4,5).contains((x>>3) & 7) && List(7,0,1,2,3,4,5).contains(x & 7) =>
        val regFrom=Register.getRegCode(x & 0x07)
        val regTo=Register.getRegCode((x >> 3) & 0x07)
        returnNewReg(
          newRegister(regTo,registerController.get(regFrom)),
          1)
      // LD r,n : 00xxx110, xxx - register code
      case x if (x & 0xC6)==0x06 && List(7,0,1,2,3,4,5).contains((x>>3) & 7) =>
        val reg=Register.getRegCode((x >> 3) & 0x07)
        returnNewReg(
          newRegister(reg,memoryController.get(PC,1)),
          2)
      // LD r,(HL) : 01xxx110, xxx - register code
      case x if (x & 0xC7)==0x46 && List(7,0,1,2,3,4,5).contains((x>>3) & 7) =>
        val reg=Register.getRegCode((x >> 3) & 0x07)
        val regH=registerController.get("H")
        val regL=registerController.get("L")
        returnNewReg(
          newRegister(reg,memoryController.get(regH*0x100+regL)),
          1)
      // LD (HL),n
      case x if x==0x36 =>
        val regH=registerController.get("H")
        val regL=registerController.get("L")
        returnNewMem(
          newMemory(regH*0x100+regL,memoryController.get(PC,1)),
          2)
      case _ => throw new UnknownOperationException(f"Unknown operation $oper at $PC")
    }
  }

  private def newRegister(symbol:String,value:Int):RegisterController=
    RegisterController((registerController >>= RegisterController.set(symbol,value)).get)
  private def newMemory(address:Int,value:Int):MemoryController=
    MemoryController((memoryController >>= MemoryController.poke(address,value)).get)

  private def returnNewNOP:Z80System = returnNewReg(registerController,1)
  private def returnNewReg(newRegister:BaseStateMonad[Register], forwardPC:Int):Z80System =
    new Z80System(memoryController,RegisterController(newRegister.get.movePC(forwardPC)))
  private def returnNewMem(newMemory:BaseStateMonad[Memory], forwardPC:Int):Z80System =
    new Z80System(MemoryController(newMemory.get),RegisterController(registerController.get.movePC(forwardPC)))
}

object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank)
  def apply(source:Z80System):Z80System=new Z80System(source.memoryController,source.registerController)
}

class UnknownOperationException(message : String) extends Exception(message) {}
