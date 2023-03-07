package org.kr.scala.z80.system

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.utils.{AnyInt, IntValue, OptionInt, Z80Utils}

import scala.annotation.tailrec

class Z80System(val memory: MemoryContents, val register: Register,
                val output: OutputFile, val input: InputFile,
                val elapsedTCycles:Long,
                val interrupt: InterruptInfo)(implicit debugger:Debugger,memoryHandler:MemoryHandler) {
  lazy val currentOpCode:OpCode with OpCodeHandledBy=OpCodes.getOpCodeObject(getCurrentOpCode)

  private def step(implicit debugger:Debugger):Z80System= handleCurrent

  private def getCurrentOpCode:OpCode={
    val pc=register(Regs.PC)
    OpCode.c3(
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
  private def getFromMemoryAtReg(symbol:RegSymbol,offset:Int,isWord:Boolean):Int =
    if(isWord) getWordFromMemoryAtReg(symbol,offset) else getByteFromMemoryAtReg(symbol,offset)
  private def getByteFromMemoryAtReg(symbol:RegSymbol,offset:Int):Int = getByte(getAddressFromReg(symbol,offset))
  private def getWordFromMemoryAtReg(symbol:RegSymbol,offset:Int):Int =
    Z80Utils.makeWord(getByte(getAddressFromReg(symbol,offset)+1),getByte(getAddressFromReg(symbol,offset)))
  private def getByteOrWord(address:Int,isWord:Boolean):Int =
    if(isWord) getWord(address) else getByte(address)
  private def getByte(address:Int):Int = memory(address)
  private def getWord(address:Int):Int = Z80Utils.makeWord(memory(address+1),memory(address))

  def readPort(port:PortIDWithUpper):Int=input.read(port.lower,port.upperNum)

  def getOptionalValueFromLocation(location:Location):OptionInt =
    location match {
      case EmptyLocation => AnyInt
      case loc => IntValue(getValueFromLocation(loc))
    }

  def getValueFromLocation(location:Location):Int =
    location match {
      case loc:RegisterLocation => getRegValue(loc.register)
      case loc:ImmediateLocation => loc.immediate
      case loc:IndirectAddrLocation =>
        getByteOrWord(getWordFromMemoryAtPC(loc.offsetPC),loc.isWord)
      case loc:RegisterAddrLocation => getFromMemoryAtReg(loc.addressReg,0,loc.isWord)
      case loc:RegisterAddrDirOffsetLocation => getFromMemoryAtReg(loc.addressReg,loc.directOffset,loc.isWord)
      case loc:RegisterAddrIndirOffsetLocation =>
        getByteFromMemoryAtReg(loc.addressReg,Z80Utils.rawByteTo2Compl(getByteFromMemoryAtPC(loc.indirectOffset2Compl)))
      case loc => throw new IncorrectLocation(f"incorrect location: ${loc.toString}")
    }

  private def putValueToMemory(address:Int, value:Int, isWord:Boolean):SystemChange =
    if(isWord) new MemoryChangeWord(address,value)
    else new MemoryChangeByte(address,value)

  def putValueToLocation(location:Location, value:Int, isWord:Boolean=false):SystemChange = {
    location match {
      case loc:RegisterLocation => new RegisterChange(loc.register, value)
      case loc:IndirectAddrLocation => putValueToMemory(getWordFromMemoryAtPC(loc.offsetPC), value, isWord)
      case loc:RegisterAddrLocation => putValueToMemory(getAddressFromReg(loc.addressReg, 0), value, isWord)
      case loc:RegisterAddrDirOffsetLocation => putValueToMemory(getAddressFromReg(loc.addressReg, loc.directOffset), value, isWord)
      case loc:RegisterAddrIndirOffsetLocation =>
        putValueToMemory(getAddressFromReg(loc.addressReg, Z80Utils.rawByteTo2Compl(getByteFromMemoryAtPC(loc.indirectOffset2Compl))), value, isWord)
      case loc => throw new IncorrectLocation(f"incorrect location: ${loc.toString}")
    }
  }
  // functions changing state
  private def returnAfterChange(chgList:List[SystemChange], forwardPC:Int=0, forwardTCycles:Int=0)(implicit debugger:Debugger):Z80System = {
    val chgListAfterPC=addPCAndTCyclesChange(chgList,forwardPC,forwardTCycles)
    changeList(chgListAfterPC)
      .handleInterrupt
  }

  private def handleInterrupt:Z80System = {
    (if(interrupt.trigger(this)) doHandleInterrupt() else this)
      .refreshInterrupt
  }

  private def doHandleInterrupt(): Z80System = {
    assume(getRegValue(Regs.IM) == 1) // handle ONLY IM 1
    changeList(interruptIM1)
  }

  private lazy val interruptIM1: List[SystemChange] = {
    val isInHalt = currentOpCode.mainOnly == HALT.mainOnly
    // Note: PC is already set to the return address
    // only for Halt the return address is the next instruction
    val returnPC = getRegValue(Regs.PC) + (if(isInHalt) 1 else 0)
    List(
      new RegisterChange(Regs.PC, 0x0038),             // IM1: call static address 0x0038
      new MemoryChangeWord(getRegValue(Regs.SP)-2, returnPC), // put PC on stack
      new RegisterChangeRelative(Regs.SP, -2),         // decrease stack
      new PCChange(0, 11 + 2)               // add T cycles (11 for RST 38 + 2 extra wait cycles)
    )
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
    val newMem=(StateWatcherSilent(memory) >>== memoryHandler.poke(address,value)).get
    replaceMemory(newMem)
  }

  def changeMemoryWord(address:Int, value:Int):Z80System = {
    val newMem=(StateWatcherSilent(memory) >>== memoryHandler.pokeW(address,value)).get
    replaceMemory(newMem)
  }

  def outputByte(port:PortIDWithUpper, value:Int):Z80System = {
    val newOut=(StateWatcher(output) >>== OutputFile.out(debugger)(port.lower,value)).get
    replaceOutput(newOut)
  }

  def attachPort(port:PortID, inPort:InputPort):Z80System = {
    val newIn=(StateWatcherSilent(input) >>== InputFile.attachPort(port,inPort)).get
    replaceInput(newIn)
  }

  def refreshInput(port:PortIDWithUpper):Z80System = {
    val newIn=(StateWatcherSilent(input) >>== InputFile.refreshPort(port.lower)).get
    replaceInput(newIn)
  }

  def refreshInterrupt:Z80System = replaceInterrupt(interrupt.refresh(this))

  private def replaceRegister(newReg:Register):Z80System= new Z80System(memory,newReg,output,input,elapsedTCycles,interrupt)
  private def replaceRegisterAndCycles(newReg:Register, newTCycles:Long):Z80System= new Z80System(memory,newReg,output,input,newTCycles,interrupt)
  private def replaceMemory(newMem:MemoryContents):Z80System= new Z80System(newMem,register,output,input,elapsedTCycles,interrupt)
  private def replaceOutput(newOut:OutputFile):Z80System= new Z80System(memory,register,newOut,input,elapsedTCycles,interrupt)
  private def replaceInput(newIn:InputFile):Z80System= new Z80System(memory,register,output,newIn,elapsedTCycles,interrupt)
  private def replaceInterrupt(newInterrupt:InterruptInfo):Z80System= new Z80System(memory,register,output,input,elapsedTCycles,newInterrupt)
}

object Z80System {
  def blank(implicit debugger:Debugger,memoryHandler:MemoryHandler):Z80System=new Z80System(memoryHandler.blank(0x10000),Register.blank,
    OutputFile.blank, InputFile.blank,0, NoInterrupt())

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

  // clock parameters
  val REFERENCE_HZ:Long=3686400
  val REFERENCE_CYCLES_20ms:Long=REFERENCE_HZ / 50
}

trait InterruptInfo {
  def trigger(system:Z80System):Boolean
  def refresh(system:Z80System):InterruptInfo
}

case class NoInterrupt() extends InterruptInfo {
  override def trigger(system: Z80System): Boolean = false
  override def refresh(system: Z80System): NoInterrupt = this
}

case class CyclicInterrupt(everyTCycles:Long,toGo:Long,lastTCycles:Long) extends InterruptInfo {
  // trigger only if interrupts are enabled
  override def trigger(system:Z80System):Boolean = system.getRegValue(Regs.IFF)==1 && toGo<0
  // always cycle - even if interrupts are disabled (normally interrupts are generated externally to the system)
  override def refresh(system:Z80System):CyclicInterrupt = {
    val step=system.elapsedTCycles - lastTCycles
    val newToGo = if(toGo<0) toGo - step + everyTCycles else toGo - step
    new CyclicInterrupt(everyTCycles,newToGo,system.elapsedTCycles)
  }
}

object CyclicInterrupt {
  def apply(everyTCycles:Long):CyclicInterrupt = new CyclicInterrupt(everyTCycles, everyTCycles, 0)
  def every20ms:CyclicInterrupt = apply(Z80System.REFERENCE_CYCLES_20ms)
}
