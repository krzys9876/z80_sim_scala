package org.kr.scala.z80.system

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.utils.{AnyInt, IntValue, OptionInt, Z80Utils}

import scala.annotation.tailrec

class Z80System(val memory: MemoryContents, val register: RegisterBase,
                val output: OutputFile, val input: InputFile,
                val elapsedTCycles:TCycleCounter,
                val interrupt: InterruptInfo)(implicit debugger:Debugger,memoryHandler:MemoryHandler,
                                              registerHandler:RegisterHandler) {
  private def pc=register(Regs.PC)

  private def step(implicit debugger:Debugger):Z80System= handleCurrent

  private def handleCurrent(implicit debugger:Debugger):Z80System={
    implicit val system:Z80System=this
    val pcValue=pc
    val opCode=OpCodes.getOpCodeObject(memory(pcValue),memory(pcValue,1),memory(pcValue,3))
    val (changedSystem,forwardPC,forwardCycles)=opCode.handler.handle(opCode)
    changedSystem.returnAfterChange(forwardPC,forwardCycles)
  }

  def getRegValue(symbol:RegSymbol):Int=register(symbol)
  def getFlags:Flag=new Flag(register(Regs.F))

  private def getByteFromMemoryAtPC(offset:Int):Int = memory(pc,offset)
  private def getWordFromMemoryAtPC(offset:Int):Int = memory.word(pc,offset)
  private def getAddressFromReg(symbol:RegSymbol,offset:Int):Int= getRegValue(symbol)+offset
  private def getFromMemoryAtReg(symbol:RegSymbol,offset:Int,isWord:Boolean):Int =
    if(isWord) getWordFromMemoryAtReg(symbol,offset) else getByteFromMemoryAtReg(symbol,offset)
  private def getByteFromMemoryAtReg(symbol:RegSymbol,offset:Int):Int = memory(getAddressFromReg(symbol,offset))
  private def getWordFromMemoryAtReg(symbol:RegSymbol,offset:Int):Int = memory.word(getAddressFromReg(symbol,offset))
  private def getByteOrWord(address:Int,isWord:Boolean):Int = if(isWord) memory.word(address) else memory(address)

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

  private def putValueToMemory(address: Int, value: Int, isWord: Boolean): Z80System =
    if (isWord) changeMemoryWord(address, value)
    else changeMemoryByte(address, value)

  def putValueToLocation(location: Location, value: Int, isWord: Boolean = false): Z80System = {
    location match {
      case loc: RegisterLocation => changeRegister(loc.register, value)
      case loc: IndirectAddrLocation => putValueToMemory(getWordFromMemoryAtPC(loc.offsetPC), value, isWord)
      case loc: RegisterAddrLocation => putValueToMemory(getAddressFromReg(loc.addressReg, 0), value, isWord)
      case loc: RegisterAddrDirOffsetLocation => putValueToMemory(getAddressFromReg(loc.addressReg, loc.directOffset), value, isWord)
      case loc: RegisterAddrIndirOffsetLocation =>
        putValueToMemory(getAddressFromReg(loc.addressReg, Z80Utils.rawByteTo2Compl(getByteFromMemoryAtPC(loc.indirectOffset2Compl))), value, isWord)
      case loc => throw new IncorrectLocation(f"incorrect location: ${loc.toString}")
    }
  }

  // functions changing state
  private def returnAfterChange(forwardPC:Int=0, forwardTCycles:Int=0)(implicit debugger:Debugger):Z80System = {
    updatePCAndTCycles(forwardPC, forwardTCycles)
      .handleInterrupt
      .refreshInterrupt
  }

  private def updatePCAndTCycles(forwardPC:Int, forwardTCycles:Int):Z80System =
    if(forwardPC!=0 || forwardTCycles!=0) changePCAndCycles(forwardPC,forwardTCycles) else this

  private def handleInterrupt:Z80System = if(interrupt.trigger(this)) doHandleInterrupt() else this

  private def doHandleInterrupt(): Z80System = {
    assume(getRegValue(Regs.IM) == 1) // handle ONLY IM 1
    interruptIM1
      .updatePCAndTCycles(0, 11+2) // add T cycles (11 for RST 38 + 2 extra wait cycles)
  }

  private def interruptIM1: Z80System = {
    val isHaltNext = memory(pc) == HALT.main
    // Note: PC is already set to the return address
    // only for Halt the return address is the next instruction (as interrupt wakes the CPU up from halt state)
    val returnPC = pc + (if(isHaltNext) 1 else 0)
    changeRegister(Regs.PC, 0x0038)             // IM1: call static address 0x0038
      .changeMemoryWord(getRegValue(Regs.SP)-2, returnPC) // put PC on stack
      .changeRegisterRelative(Regs.SP, -2)         // decrease stack
  }

  /* NOTE: We could be watching memory changes through StateWatcher, but it adds some overhead */
  def changeRegister(regSymbol:RegSymbol, value:Int):Z80System = {
    //val newReg=(StateWatcherSilent(register) >>== registerHandler.set(regSymbol,value)).get
    val newReg=register.set(regSymbol,value)
    replaceRegister(newReg)
  }

  def changeRegisterRelative(regSymbol:RegSymbol, value:Int):Z80System = {
    //val newReg=(StateWatcherSilent(register) >>== registerHandler.relative(regSymbol,value)).get
    val newReg=register.relative(regSymbol,value)
    replaceRegister(newReg)
  }

  def changePCAndCycles(pc:Int, cycles:Int):Z80System = {
    //val newReg=(StateWatcherSilent(register) >>== registerHandler.relative(Regs.PC,pc)).get
    val newReg=register.relative(Regs.PC,pc)
    replaceRegisterAndCycles(newReg,elapsedTCycles.add(cycles))
  }

  def changeMemoryByte(address:Int, value:Int):Z80System = {
    //val newMem=(StateWatcherSilent(memory) >>== memoryHandler.poke(address,value)).get
    val newMem=memoryHandler.poke(address, value)(memory)
    replaceMemory(newMem)
  }

  def changeMemoryWord(address:Int, value:Int):Z80System = {
    //val newMem=(StateWatcherSilent(memory) >>== memoryHandler.pokeW(address,value)).get
    val newMem=memoryHandler.pokeW(address, value)(memory)
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

  private def refreshInterrupt:Z80System = replaceInterrupt(interrupt.refresh(this))

  private def replaceRegister(newReg:RegisterBase):Z80System=
    if(newReg ne register) new Z80System(memory,newReg,output,input,elapsedTCycles,interrupt) else this
  private def replaceRegisterAndCycles(newReg:RegisterBase, newTCycles:TCycleCounter):Z80System=
    if((newReg ne register) || (newTCycles ne elapsedTCycles)) new Z80System(memory,newReg,output,input,newTCycles,interrupt) else this
  private def replaceMemory(newMem:MemoryContents):Z80System=
    if(newMem ne memory) new Z80System(newMem,register,output,input,elapsedTCycles,interrupt) else this
  private def replaceOutput(newOut:OutputFile):Z80System=
    if(newOut ne output) new Z80System(memory,register,newOut,input,elapsedTCycles,interrupt) else this
  private def replaceInput(newIn:InputFile):Z80System=
    if(newIn ne input) new Z80System(memory,register,output,newIn,elapsedTCycles,interrupt) else this
  private def replaceInterrupt(newInterrupt:InterruptInfo):Z80System=
    if(newInterrupt ne interrupt) new Z80System(memory,register,output,input,elapsedTCycles,newInterrupt) else this
}

object Z80System {
  def blank(implicit debugger:Debugger,memoryHandler:MemoryHandler,registerHandler:RegisterHandler,
            tCycleHandler:TCycleCounterHandler):Z80System=
    new Z80System(memoryHandler.blank(0x10000),registerHandler.blank,
      OutputFile.blank, InputFile.blank,tCycleHandler.blank, NoInterrupt())

  // run - main function changing state of the system
  def run(implicit debug:Debugger,watcherHandler:StateWatcherHandlerBase[Z80System]):Long=>Z80System=>Z80System=
    toGo=>system=>steps(watcherHandler.createNewWatcher(system),toGo).state

  @tailrec
  private def steps(start:StateWatcherBase[Z80System], toGo:Long)(implicit debugger: Debugger):StateWatcherBase[Z80System]={
    toGo match {
      case 0 => start
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
    val step=system.elapsedTCycles.cycles - lastTCycles
    val newToGo = if(toGo<0) toGo - step + everyTCycles else toGo - step
    new CyclicInterrupt(everyTCycles,newToGo,system.elapsedTCycles.cycles)
  }
}

object CyclicInterrupt {
  def apply(everyTCycles:Long):CyclicInterrupt = new CyclicInterrupt(everyTCycles, everyTCycles, 0)
  def every20ms:CyclicInterrupt = apply(Z80System.REFERENCE_CYCLES_20ms)
}

class CyclicInterruptMutable(val everyTCycles:Long) extends InterruptInfo {
  private var toGo:Long = everyTCycles
  private var lastTCycles:Long = 0
  // trigger only if interrupts are enabled
  override def trigger(system:Z80System):Boolean = system.getRegValue(Regs.IFF)==1 && toGo<0
  // always cycle - even if interrupts are disabled (normally interrupts are generated externally to the system)
  override def refresh(system:Z80System):CyclicInterruptMutable = {
    val step=system.elapsedTCycles.cycles - lastTCycles
    toGo = if(toGo<0) toGo - step + everyTCycles else toGo - step
    lastTCycles = system.elapsedTCycles.cycles
    this
  }
}

object CyclicInterruptMutable {
  def apply(everyTCycles:Long):CyclicInterruptMutable = new CyclicInterruptMutable(everyTCycles)
  def every20ms:CyclicInterruptMutable = apply(Z80System.REFERENCE_CYCLES_20ms)
}

trait TCycleCounter {
  def cycles:Long
  def add(cyclesToAdd:Int):TCycleCounter
  def instructionsBySize(size: Int): Long
  def addInstruction(size:Int):TCycleCounter

}

case class TCycleCounterImmutable(override val cycles:Long, instructionsBySizeSrc:Vector[Long]=Vector(0,0,0,0)) extends TCycleCounter {
  override def add(cyclesToAdd:Int):TCycleCounter = copy(cycles=cycles+cyclesToAdd)
  override def instructionsBySize(size:Int):Long = instructionsBySizeSrc(size)
  override def addInstruction(size: Int): TCycleCounter =
    copy(instructionsBySizeSrc=instructionsBySizeSrc.updated(size,instructionsBySizeSrc(size)+1))
}

class TCycleCounterMutable(private var c:Long,private val instructionsBySizeSrc:Array[Long]=Array(0,0,0,0)) extends TCycleCounter {
  def cycles:Long = c
  override def add(cyclesToAdd:Int):TCycleCounter = {
    c+=cyclesToAdd
    this
  }
  override def instructionsBySize(size:Int):Long = instructionsBySizeSrc(size)
  override def addInstruction(size: Int): TCycleCounter = {
    instructionsBySizeSrc(size)+=1
    this
  }
}

trait TCycleCounterHandler {
  def blank:TCycleCounter
}

class TCycleCounterHandlerImmutable extends TCycleCounterHandler {
  override def blank:TCycleCounterImmutable = TCycleCounterImmutable(0)
}

class TCycleCounterHandlerMutable extends TCycleCounterHandler{
  override def blank:TCycleCounterMutable = new TCycleCounterMutable(0)
}