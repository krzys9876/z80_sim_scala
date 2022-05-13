package org.kr.scala.z80.system

import org.kr.scala.z80.utils.Z80Utils

import scala.annotation.tailrec

class Z80SystemController(override val state:Z80System) extends BaseStateMonad[Z80System](state) {
  def >>= (fChangeState: Z80System=>Z80SystemController):Z80SystemController=fChangeState(state)
}

object Z80SystemController {
  def apply(state: Z80System):Z80SystemController = new Z80SystemController(state)
  def blank:Z80SystemController = new Z80SystemController(Z80System.blank)

  def run(implicit debug:Debugger):Long=>Z80System=>Z80SystemController=
    toGo=>system=>Z80SystemController(steps(Z80SystemController(system),toGo).state)

  @tailrec
  private def steps(start:Z80SystemController, toGo:Long)(implicit debugger: Debugger):Z80SystemController={
    toGo match {
      case 0 => Z80SystemController(start.state)
      case _ => steps(start >>= step(debugger)(),toGo-1)
    }
  }

  private def step(implicit debugger:Debugger): () => Z80System => Z80SystemController = () => system =>
    Z80SystemController(system.step)

  def changeRegister:(RegSymbol,Int) => Z80System => Z80SystemController = (regSymbol, value) => system => {
    val newReg=system.registerController >>= RegisterController.set(regSymbol,value)
    Z80SystemController(system.replaceRegister(newReg))
  }

  def changePCAndCycles:(Int,Int) => Z80System => Z80SystemController = (pc,cycles) => system => {
    val newReg=system.registerController >>= RegisterController.setRelative(Regs.PC,pc)
    Z80SystemController(system.replaceRegisterAndCycles(newReg,system.elapsedTCycles+cycles))
  }

  def changeRegisterRelative:(RegSymbol,Int) => Z80System => Z80SystemController = (regSymbol, value) => system => {
    val newReg=system.registerController >>= RegisterController.setRelative(regSymbol,value)
    Z80SystemController(system.replaceRegister(newReg))
  }

  def changeMemoryByte:(Int,Int) => Z80System => Z80SystemController = (address, value) => system => {
    val newMem=system.memoryController >>= MemoryController.poke(address,value)
    Z80SystemController(system.replaceMemory(newMem))
  }

  def changeMemoryWord:(Int,Int) => Z80System => Z80SystemController = (address, value) => system => {
    val newMem=system.memoryController >>= MemoryController.poke(address,Z80Utils.getL(value)) >>=
       MemoryController.poke(address+1,Z80Utils.getH(value))
    Z80SystemController(system.replaceMemory(newMem))
  }

  def outputByte(implicit debugger:Debugger):(Int,Int) => Z80System => Z80SystemController = (port, value) => system => {
    val newOut=system.outputController >>= OutputController.out(debugger)(port,value)
    Z80SystemController(system.replaceOutput(newOut))
  }

  def refreshInput:Int => Z80System => Z80SystemController = port => system => {
    val newIn=system.inputController >>= InputController.refreshPort(port)
    Z80SystemController(system.replaceInput(newIn))
  }

  def attachPort:(Int,InputPort) => Z80System => Z80SystemController = (port,inPort) => system => {
    val newIn=system.inputController >>= InputController.attachPort(port,inPort)
    Z80SystemController(system.replaceInput(newIn))
  }

  def change:SystemChangeBase => Z80System => Z80SystemController = change => system =>
    change.handle(Z80SystemController(system))

  def changeList:List[SystemChangeBase] => Z80System => Z80SystemController = list => initSystem => {
    list.foldLeft(Z80SystemController(initSystem))((systemC,oneChange)=>
      systemC >>= Z80SystemController.change(oneChange))
  }
}
