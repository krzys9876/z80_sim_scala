package org.kr.scala.z80.system

import org.kr.scala.z80.utils.Z80Utils

import scala.annotation.tailrec

class Z80SystemController(override val state:Z80System) extends BaseStateMonad[Z80System](state) {
  def >>= (fChangeState: Z80System=>Z80SystemController):Z80SystemController=fChangeState(state)
}

object Z80SystemController {
  def apply(state: Z80System):Z80SystemController = new Z80SystemController(state)
  def blank:Z80SystemController = new Z80SystemController(Z80System.blank)

  def run(implicit debug:Debugger=DummyDebugger):Int=>Z80System=>Z80SystemController=
    toGo=>system=>Z80SystemController(steps(Z80SystemController(system),toGo).state)

  @tailrec
  private def steps(start:Z80SystemController, toGo:Int)(implicit debugger: Debugger):Z80SystemController={
    toGo match {
      case 0 => Z80SystemController(start.state)
      case _ => steps(start >>= step(debugger)(),toGo-1)
    }
  }

  private def step(implicit debugger:Debugger): () => Z80System => Z80SystemController = () => system =>
    Z80SystemController(system.step)

  def changeRegister:(String,Int) => Z80System => Z80SystemController = (regSymbol, value) => system => {
    val newReg=system.registerController >>= RegisterController.set(regSymbol,value)
    Z80SystemController(new Z80System(system.memoryController,RegisterController(newReg.get),
      system.outputController,system.inputController))
    }

  def changeRegisterRelative:(String,Int) => Z80System => Z80SystemController = (regSymbol, value) => system => {
    val newReg=system.registerController >>= RegisterController.setRelative(regSymbol,value)
    Z80SystemController(new Z80System(system.memoryController,RegisterController(newReg.get),
      system.outputController,system.inputController))
  }

  def changeMemoryByte:(Int,Int) => Z80System => Z80SystemController = (address, value) => system => {
    val newMem=system.memoryController >>= MemoryController.poke(address,value)
    Z80SystemController(new Z80System(newMem,system.registerController,
      system.outputController,system.inputController))
  }

  def changeMemoryWord:(Int,Int) => Z80System => Z80SystemController = (address, value) => system => {
    val newMem=system.memoryController >>= MemoryController.poke(address,Z80Utils.getL(value)) >>=
       MemoryController.poke(address+1,Z80Utils.getH(value))
    Z80SystemController(new Z80System(newMem,system.registerController,
      system.outputController,system.inputController))
  }

  def outputByte:(Int,Int) => Z80System => Z80SystemController = (port, value) => system => {
    val newOut=system.outputController >>= OutputController.out(port,value)
    Z80SystemController(new Z80System(system.memoryController,system.registerController,newOut,
      system.inputController))
  }

  def refreshInput:Int => Z80System => Z80SystemController = port => system => {
    val newIn=system.inputController >>= InputController.refreshPort(port)
    Z80SystemController(new Z80System(system.memoryController,system.registerController,system.outputController,
      newIn))
  }

  def attachPort:(Int,InputPort) => Z80System => Z80SystemController = (port,inPort) => system => {
    val newIn=system.inputController >>= InputController.attachPort(port,inPort)
    Z80SystemController(new Z80System(system.memoryController,system.registerController,
      system.outputController,newIn))
  }

  def change:SystemChangeBase => Z80System => Z80SystemController = change => system =>
    change.handle(Z80SystemController(system))

  def changeList:List[SystemChangeBase] => Z80System => Z80SystemController = list => initSystem => {
    list.foldLeft(Z80SystemController(initSystem))((systemC,oneChange)=>
      systemC >>= Z80SystemController.change(oneChange))
  }
}
