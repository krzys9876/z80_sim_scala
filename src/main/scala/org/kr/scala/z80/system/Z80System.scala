package org.kr.scala.z80.system

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.utils.Z80Utils

import scala.annotation.tailrec

class Z80System(val memory: Memory, val register: Register,
                val output: OutputFile, val input: InputFile,
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

  def readPort(port:Int):Int=input.read(port)

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

  private def putValueToMemory(address:Int, value:Int, isWord:Boolean):SystemChange =
    if(isWord) new MemoryChangeWord(address,value)
    else new MemoryChangeByte(address,value)

  def putValueToLocation(location:Location, value:Int, isWord:Boolean=false):SystemChange =
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

  // functions changing state
  private def returnAfterChange(chgList:List[SystemChange], forwardPC:Int=0, forwardTCycles:Int=0)(implicit debugger:Debugger):Z80System = {
    val chgListAfterPC=addPCAndTCyclesChange(chgList,forwardPC,forwardTCycles)
    changeList(chgListAfterPC)
  }

  private def addPCAndTCyclesChange(chgList:List[SystemChange], forwardPC:Int, forwardTCycles:Int):List[SystemChange] =
    chgList ++ (if(forwardPC!=0 || forwardTCycles!=0) List(new PCChange(forwardPC,forwardTCycles)) else List())

  def changeList(list:List[SystemChange]):Z80System =
    list.foldLeft(this)((changedSystem,oneChange)=>oneChange.handle(changedSystem))

  def changeRegister(regSymbol:RegSymbol, value:Int):Z80System = {
    val newReg=(StateWatcherSilent(register) >>== Register.set(regSymbol,value)).get
    replaceRegister(newReg)
  }

  def changeRegisterRelative(regSymbol:RegSymbol, value:Int):Z80System = {
    val newReg=(StateWatcherSilent(register) >>== Register.setRelative(regSymbol,value)).get
    replaceRegister(newReg)
  }

  def changePCAndCycles(pc:Int, cycles:Int):Z80System = {
    val newReg=(StateWatcherSilent(register) >>== Register.setRelative(Regs.PC,pc)).get
    replaceRegisterAndCycles(newReg,elapsedTCycles+cycles)
  }

  def changeMemoryByte(address:Int, value:Int):Z80System = {
    val newMem=(StateWatcherSilent(memory) >>== Memory.poke(address,value)).get
    replaceMemory(newMem)
  }

  def changeMemoryWord(address:Int, value:Int):Z80System = {
    val newMem=StateWatcherSilent(memory) >>==
      Memory.poke(address,Z80Utils.getL(value)) >>==
      Memory.poke(address+1,Z80Utils.getH(value))
    replaceMemory(newMem.get)
  }

  def outputByte(port:Int, value:Int):Z80System = {
    val newOut=(StateWatcher(output) >>== OutputFile.out(debugger)(port,value)).get
    replaceOutput(newOut)
  }

  def attachPort(port:Int, inPort:InputPort):Z80System = {
    val newIn=(StateWatcherSilent(input) >>== InputFile.attachPort(port,inPort)).get
    replaceInput(newIn)
  }

  def refreshInput(port:Int):Z80System = {
    val newIn=(StateWatcherSilent(input) >>== InputFile.refreshPort(port)).get
    replaceInput(newIn)
  }

  private def replaceRegister(newReg:Register):Z80System= new Z80System(memory,newReg,output,input,elapsedTCycles)
  private def replaceRegisterAndCycles(newReg:Register, newTCycles:Long):Z80System= new Z80System(memory,newReg,output,input,newTCycles)
  private def replaceMemory(newMem:Memory):Z80System= new Z80System(newMem,register,output,input,elapsedTCycles)
  private def replaceOutput(newOut:OutputFile):Z80System= new Z80System(memory,register,newOut,input,elapsedTCycles)
  private def replaceInput(newIn:InputFile):Z80System= new Z80System(memory,register,output,newIn,elapsedTCycles)
}

object Z80System {
  def blank(implicit debugger:Debugger):Z80System=new Z80System(Memory.blank(0x10000),Register.blank,
    OutputFile.blank, InputFile.blank,0)

  // run - main function changing state of the system
  def run(implicit debug:Debugger):Long=>Z80System=>Z80System=
    toGo=>system=>steps(StateWatcher(system),toGo).state

  @tailrec
  private def steps(start:StateWatcher[Z80System], toGo:Long)(implicit debugger: Debugger):StateWatcher[Z80System]={
    toGo match {
      case 0 => StateWatcher(start.state)
      case _ => steps(start >>== Z80System.step(debugger)(),toGo-1)
    }
  }

  private def step(implicit debugger:Debugger): () => Z80System => Z80System = () => system => system.step
}