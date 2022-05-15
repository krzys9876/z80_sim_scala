package org.kr.scala.z80.system

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.utils.Z80Utils

import scala.annotation.tailrec

class Z80System(val memory: Memory, val register: Register,
                val outputController: StateWatcher[OutputFile],
                val inputController: StateWatcher[InputFile],
                val elapsedTCycles:Long)(implicit debugger:Debugger) {
  lazy val currentOpCode:OpCode with OpCodeHandledBy=OpCodes.getOpCodeObject(getCurrentOpCode)

  def step(implicit debugger:Debugger):Z80System=
    handleCurrent

  private def getCurrentOpCode:OpCode={
    val pc=register(Regs.PC)
    OpCode(
      memory(pc),
      memory(pc,1),
      memory(pc,3))
  }

  private def handleCurrent(implicit debugger:Debugger):Z80System={
    implicit val system:Z80System=this
    val (change,forwardPC,forwardCycles)=currentOpCode.handler.handle(currentOpCode)
    returnAfterChange(change,forwardPC,forwardCycles)
  }

  def getRegValue(symbol:RegSymbol):Int=register(symbol)
  def getFlags:Flag=new Flag(register(Regs.F))

  private def getByteFromMemoryAtPC(offset:Int):Int = getByteFromMemoryAtReg(Regs.PC,offset)
  private def getWordFromMemoryAtPC(offset:Int):Int = getWordFromMemoryAtReg(Regs.PC,offset)
  private def getAddressFromReg(symbol:RegSymbol,offset:Int):Int= getRegValue(symbol)+offset
  private def getByteFromMemoryAtReg(symbol:RegSymbol,offset:Int):Int = getByte(getAddressFromReg(symbol,offset))
  private def getWordFromMemoryAtReg(symbol:RegSymbol,offset:Int):Int =
    Z80Utils.makeWord(getByte(getAddressFromReg(symbol,offset)+1),getByte(getAddressFromReg(symbol,offset)))
  private def getByte(address:Int):Int = memory(address)
  private def getWord(address:Int):Int = Z80Utils.makeWord(memory(address+1),memory(address))

  private def returnAfterChange(chgList:List[SystemChangeBase],forwardPC:Int=0,forwardTCycles:Int=0)(implicit debugger:Debugger):Z80System = {
    val chgListAfterPC=chgList ++ (if(forwardPC!=0 || forwardTCycles!=0) List(new PCChange(forwardPC,forwardTCycles)) else List())
    (StateWatcherSilent[Z80System](this) >>== Z80System.changeList(debugger)(chgListAfterPC)).get
  }

  def readPort(port:Int):Int=inputController.get.read(port)

  def getValueFromLocation(loc:Location):Int =
    loc match {
      case l if l==Location.empty => OpCode.ANY
      case Location(r,_,_,_,_,_,_) if r!=Regs.NONE => getRegValue(r)
      case Location(_,i,_,_,_,_,_) if i!=OpCode.ANY => i
      case Location(_,_,pco,_,_,_,isWord) if pco!=OpCode.ANY =>
        if(isWord) getWord(getWordFromMemoryAtPC(pco)) else getByte(getWordFromMemoryAtPC(pco))
      case Location(_,_,_,r,dirO,indirO,isWord) if r!=Regs.NONE =>
        (dirO,indirO,isWord) match {
          case (OpCode.ANY,OpCode.ANY,_) => if(isWord) getWordFromMemoryAtReg(r,0) else getByteFromMemoryAtReg(r,0)
          case (o,OpCode.ANY,isWord) => if(isWord) getWordFromMemoryAtReg(r,o) else getByteFromMemoryAtReg(r,o)
          case (OpCode.ANY,off2Compl,_) => getByteFromMemoryAtReg(r,Z80Utils.rawByteTo2Compl(getByteFromMemoryAtPC(off2Compl)))
          case (_,_,_) => throw new IncorrectLocation(f"incorrect location: ${loc.toString}")
        }
      case Location(_,_,_,_,_,_,_) => throw new IncorrectLocation(f"incorrect location: ${loc.toString}")
    }

  private def putValueToMemory(address:Int, value:Int, isWord:Boolean):SystemChangeBase =
    if(isWord) new MemoryChangeWord(address,value)
    else new MemoryChangeByte(address,value)

  def putValueToLocation(location:Location, value:Int, isWord:Boolean=false):SystemChangeBase =
    location match {
      case Location(r,_,_,_,_,_,_) if r!=Regs.NONE => new RegisterChange(r,value)
      case Location(_,_,pco,_,_,_,_) if pco!=OpCode.ANY => putValueToMemory(getWordFromMemoryAtPC(pco),value,isWord)
      case Location(_,_,_,r,dirO,indirO,_) if r!=Regs.NONE =>
        (dirO,indirO) match {
          case (dirO,OpCode.ANY) if dirO!=OpCode.ANY => putValueToMemory(getAddressFromReg(r,dirO),value,isWord)
          case (OpCode.ANY,OpCode.ANY) => putValueToMemory(getAddressFromReg(r,0),value,isWord)
          case (OpCode.ANY,indirOff2Compl) =>
            putValueToMemory(getAddressFromReg(r,Z80Utils.rawByteTo2Compl(getByteFromMemoryAtPC(indirOff2Compl))),value,isWord)
        }
      case l if l==Location.empty => new DummyChange
      case Location(_,_,_,_,_,_,_) => throw new IncorrectLocation(f"incorrect location: ${location.toString}")
    }

  def replaceRegister(newReg:Register):Z80System=
    new Z80System(memory,newReg,outputController,inputController,elapsedTCycles)

  def replaceRegisterAndCycles(newReg:Register, newTCycles:Long):Z80System=
    new Z80System(memory,newReg,outputController,inputController,newTCycles)

  def replaceMemory(newMem:Memory):Z80System=
    new Z80System(newMem,register,outputController,inputController,elapsedTCycles)

  def replaceOutput(newOut:StateWatcher[OutputFile]):Z80System=
    new Z80System(memory,register,newOut,inputController,elapsedTCycles)

  def replaceInput(newIn:StateWatcher[InputFile]):Z80System=
    new Z80System(memory,register,outputController,newIn,elapsedTCycles)
}

object Z80System {
  def blank(implicit debugger:Debugger):Z80System=new Z80System(Memory.blank(0x10000),Register.blank,
    StateWatcher[OutputFile](OutputFile.blank), StateWatcher[InputFile](InputFile.blank),0)

  // functions changing state (Z80System=>Z80System)
  def changeList(implicit debugger:Debugger):List[SystemChangeBase] => Z80System => Z80System = list => system =>
    list.foldLeft(system)((changedSystem,oneChange)=>oneChange.handle(changedSystem))

  def change(implicit debugger:Debugger):SystemChangeBase => Z80System => Z80System = change => system => change.handle(system)

  def changeRegister(implicit debugger:Debugger):(RegSymbol,Int) => Z80System => Z80System = (regSymbol, value) => system => {
    val newReg=(StateWatcherSilent(system.register) >>== Register.set(regSymbol,value)).get
    system.replaceRegister(newReg)
  }

  def changeRegisterRelative(implicit debugger:Debugger):(RegSymbol,Int) => Z80System => Z80System = (regSymbol, value) => system => {
    val newReg=(StateWatcherSilent(system.register) >>== Register.setRelative(regSymbol,value)).get
    system.replaceRegister(newReg)
  }

  def changePCAndCycles(implicit debugger:Debugger):(Int,Int) => Z80System => Z80System = (pc,cycles) => system => {
    val newReg=(StateWatcherSilent(system.register) >>== Register.setRelative(Regs.PC,pc)).get
    system.replaceRegisterAndCycles(newReg,system.elapsedTCycles+cycles)
  }

  def changeMemoryByte(implicit debugger:Debugger):(Int,Int) => Z80System => Z80System = (address, value) => system => {
    val newMem=(StateWatcherSilent(system.memory) >>== Memory.poke(address,value)).get
    system.replaceMemory(newMem)
  }

  def changeMemoryWord(implicit debugger:Debugger):(Int,Int) => Z80System => Z80System = (address, value) => system => {
    val newMem=StateWatcherSilent(system.memory) >>==
      Memory.poke(address,Z80Utils.getL(value)) >>==
      Memory.poke(address+1,Z80Utils.getH(value))
    system.replaceMemory(newMem.get)
  }

  def outputByte(implicit debugger:Debugger):(Int,Int) => Z80System => Z80System = (port, value) => system => {
    val newOut=system.outputController >>== OutputFile.out(debugger)(port,value)
    system.replaceOutput(newOut)
  }

  def attachPort(implicit debugger:Debugger):(Int,InputPort) => Z80System => Z80System = (port,inPort) => system => {
    val newIn=system.inputController >>== InputFile.attachPort(port,inPort)
    system.replaceInput(newIn)
  }

  def refreshInput(implicit debugger:Debugger):Int => Z80System => Z80System = port => system => {
    val newIn=system.inputController >>== InputFile.refreshPort(port)
    system.replaceInput(newIn)
  }

  // run - main functions changing state of the system
  def run(implicit debug:Debugger):Long=>Z80System=>Z80System=
    toGo=>system=>steps(StateWatcher[Z80System](system),toGo).state

  @tailrec
  private def steps(start:StateWatcher[Z80System], toGo:Long)(implicit debugger: Debugger):StateWatcher[Z80System]={
    toGo match {
      case 0 => StateWatcher[Z80System](start.state)
      case _ => steps(start >>== Z80System.step(debugger)(),toGo-1)
    }
  }

  private def step(implicit debugger:Debugger): () => Z80System => Z80System = () => system => system.step
}