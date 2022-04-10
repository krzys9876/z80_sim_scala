package org.kr.scala.z80.system

import org.kr.scala.z80.utils.Z80Utils

import scala.annotation.tailrec

class Z80SystemController(override val state:Z80System) extends BaseStateMonad[Z80System](state) {
  def >>= (fChangeState: Z80System=>Z80SystemController):Z80SystemController=fChangeState(state)
}

object Z80SystemController {
  def apply(state: Z80System):Z80SystemController = new Z80SystemController(state)
  def blank:Z80SystemController = new Z80SystemController(Z80System.blank)

  def run:Int=>Z80System=>Z80SystemController=
    toGo=>system=>Z80SystemController(steps(Z80SystemController(system),toGo).state)

  @tailrec
  private def steps(start:Z80SystemController, toGo:Int):Z80SystemController={
    toGo match {
      case 0 => Z80SystemController(start.state)
      case _ => steps(start >>= step(),toGo-1)
    }
  }

  private def step: () => Z80System => Z80SystemController = () => system =>
    Z80SystemController(system.step)

  def changeRegister:(String,Int) => Z80System => Z80SystemController = (regSymbol, value) => system => {
    val newReg=system.registerController >>= RegisterController.set(regSymbol,value)
    Z80SystemController(new Z80System(system.memoryController,RegisterController(newReg.get)))
    }

  def changeRegisterRelative:(String,Int) => Z80System => Z80SystemController = (regSymbol, value) => system => {
    val newReg=system.registerController >>= RegisterController.setRelative(regSymbol,value)
    Z80SystemController(new Z80System(system.memoryController,RegisterController(newReg.get)))
  }

  def changeMemoryByte:(Int,Int) => Z80System => Z80SystemController = (address, value) => system => {
    val newMem=system.memoryController >>= MemoryController.poke(address,value)
    Z80SystemController(new Z80System(newMem,system.registerController))
  }

  def changeMemoryWord:(Int,Int) => Z80System => Z80SystemController = (address, value) => system => {
    val newMem=system.memoryController >>= MemoryController.poke(address,Z80Utils.getL(value)) >>=
       MemoryController.poke(address+1,Z80Utils.getH(value))
    Z80SystemController(new Z80System(newMem,system.registerController))
  }

  def change:SystemChangeBase => Z80System => Z80SystemController = change => system => {
    change match {
      case ch : RegisterChange =>
        Z80SystemController(system) >>= Z80SystemController.changeRegister(ch.regSymbol,ch.value)
      case ch : RegisterChangeRelative =>
        Z80SystemController(system) >>= Z80SystemController.changeRegisterRelative(ch.regSymbol,ch.value)
      case ch : MemoryChangeByte =>
        Z80SystemController(system) >>= Z80SystemController.changeMemoryByte(ch.address,ch.value)
      case ch : MemoryChangeWord =>
        Z80SystemController(system) >>= Z80SystemController.changeMemoryWord(ch.address,ch.value)
    }
  }

  def changeList:List[SystemChangeBase] => Z80System => Z80SystemController = list => initSystem => {
    val newSystem=list.foldLeft(initSystem)((system,oneChange)=>
      (Z80SystemController(system) >>= Z80SystemController.change(oneChange)).get)
    Z80SystemController(newSystem)
  }
}

abstract class SystemChangeBase(val value: Int)
class RegisterChange(val regSymbol: String, override val value: Int) extends SystemChangeBase(value)
class RegisterChangeRelative(val regSymbol: String, override val value: Int) extends SystemChangeBase(value)
class MemoryChangeByte(val address: Int, override val value: Int) extends SystemChangeBase(value)
class MemoryChangeWord(val address: Int, override val value: Int) extends SystemChangeBase(value)

