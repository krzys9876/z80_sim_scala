package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, FlagSymbol, MemoryChangeWord, RegisterChange, RegisterChangeRelative, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

case class JumpCondition(flag:FlagSymbol,register:String,value:Int) {
  lazy val flagValue:Boolean = value!=0
  lazy val isFlag:Boolean = flag!=Flag.None
  lazy val isRegister:Boolean = register!=""
  lazy val isEmpty:Boolean = this.equals(JumpCondition.empty)
}

object JumpCondition {
  def flag(flag:FlagSymbol,value:Boolean):JumpCondition=JumpCondition(flag,"",if(value) 1 else 0)
  def register(register:String,value:Int):JumpCondition=JumpCondition(Flag.None,register,value)
  val empty:JumpCondition=JumpCondition(Flag.None,"",OpCode.ANY)
}

class JumpConditionChecker(val condition: JumpCondition)(implicit system: Z80System) {
  lazy val decRegValue:Int =
    condition match {
      case c if c.isRegister => Z80Utils.add8bit(system.getRegValue(c.register),-1)
      case _ => OpCode.ANY
    }

  lazy val isMet: Boolean =
    condition match {
      case c if c.isEmpty => true
      case c if c.isFlag => new Flag(system.getRegValue("F")).flagValue(condition.flag) == condition.value
      case c if c.isRegister => decRegValue != c.value
    }
}

sealed abstract class JumpOperation(val name:String)

object JumpType {
  case object Jump extends JumpOperation("JUMP")
  case object JumpR extends JumpOperation("JUMP_RELATIVE")
  case object DJumpR extends JumpOperation("DEC_JUMP_RELATIVE")
  case object Call extends JumpOperation("CALL")
  case object Return extends JumpOperation("RETURN")
  case object None extends JumpOperation("NONE")
}

object JumpCallReturn extends OperationSpec with OpCodeHandler {
  //Z80 manual p.59
  val conditionListMap: Map[List[OpCode],JumpCondition] = Map(
    List(OpCode(0xC3),OpCode(0xE9),OpCode(0xDD,0xE9),OpCode(0xFD,0xE9),OpCode(0x18),OpCode(0xCD),
      OpCode(0xC9),OpCode(0xED,0x4D),
      OpCode(0xC7),OpCode(0xCF),OpCode(0xD7),OpCode(0xDF),OpCode(0xE7),OpCode(0xEF),
      OpCode(0xF7),OpCode(0xFF))->JumpCondition.empty,
    List(OpCode(0xDA),OpCode(0x38),OpCode(0xDC),OpCode(0xD8))->JumpCondition.flag(Flag.C,value=true),
    List(OpCode(0xD2),OpCode(0x30),OpCode(0xD4),OpCode(0xD0))->JumpCondition.flag(Flag.C,value=false),
    List(OpCode(0xCA),OpCode(0x28),OpCode(0xCC),OpCode(0xC8))->JumpCondition.flag(Flag.Z,value=true),
    List(OpCode(0xC2),OpCode(0x20),OpCode(0xC4),OpCode(0xC0))->JumpCondition.flag(Flag.Z,value=false),
    List(OpCode(0xEA),OpCode(0xEC),OpCode(0xE8))->JumpCondition.flag(Flag.P,value=true),
    List(OpCode(0xE2),OpCode(0xE4),OpCode(0xE0))->JumpCondition.flag(Flag.P,value=false),
    List(OpCode(0xFA),OpCode(0xFC),OpCode(0xF8))->JumpCondition.flag(Flag.S,value=true),
    List(OpCode(0xF2),OpCode(0xF4),OpCode(0xF0))->JumpCondition.flag(Flag.S,value=false),
    List(OpCode(0x10))->JumpCondition.register("B",0)
  )

  val condition: OpCodeMap[JumpCondition] = new OpCodeMap(conditionListMap, JumpCondition.empty)

  val operationListMap: Map[List[OpCode],JumpOperation] = Map(
    List(OpCode(0xC3),OpCode(0xDA),OpCode(0xD2),OpCode(0xCA),OpCode(0xC2),OpCode(0xEA),OpCode(0xE2),
      OpCode(0xFA),OpCode(0xF2),OpCode(0xE9),OpCode(0xDD,0xE9),OpCode(0xFD,0xE9))->JumpType.Jump,
    List(OpCode(0x18),OpCode(0x38),OpCode(0x30),OpCode(0x28),OpCode(0x20))->JumpType.JumpR,
    List(OpCode(0x10))->JumpType.DJumpR,
    List(OpCode(0xCD),OpCode(0xDC),OpCode(0xD4),OpCode(0xCC),OpCode(0xC4),OpCode(0xEC),OpCode(0xE4),
      OpCode(0xFC),OpCode(0xF4),
      OpCode(0xC7),OpCode(0xCF),OpCode(0xD7),OpCode(0xDF),OpCode(0xE7),OpCode(0xEF),
      OpCode(0xF7),OpCode(0xFF))->JumpType.Call,
    List(OpCode(0xC9),OpCode(0xD8),OpCode(0xD0),OpCode(0xC8),OpCode(0xC0),OpCode(0xE8),OpCode(0xE0),
      OpCode(0xF8),OpCode(0xF0),
      OpCode(0xED,0x4D))->JumpType.Return
  )

  val operation: OpCodeMap[JumpOperation] = new OpCodeMap(operationListMap, JumpType.None)

  val locationListMap: Map[List[OpCode],LoadLocation] = Map(
    List(OpCode(0xC3),OpCode(0xDA),OpCode(0xD2),OpCode(0xCA),OpCode(0xC2),OpCode(0xEA),OpCode(0xE2),
      OpCode(0xFA),OpCode(0xF2))->LoadLocation.registerAddrDirOffset("PC",1,isWord = true),
    List(OpCode(0xE9))->LoadLocation.register("HL"),
    List(OpCode(0xDD,0xE9))->LoadLocation.register("IX"),
    List(OpCode(0xFD,0xE9))->LoadLocation.register("IY"),
    List(OpCode(0x18),OpCode(0x38),OpCode(0x30),OpCode(0x28),OpCode(0x20),
      OpCode(0x10))->LoadLocation.registerAddrDirOffset("PC",1),
    List(OpCode(0xCD),OpCode(0xDC),OpCode(0xD4),OpCode(0xCC),OpCode(0xC4),OpCode(0xEC),OpCode(0xE4),
      OpCode(0xFC),OpCode(0xF4))->LoadLocation.registerAddrDirOffset("PC",1,isWord = true),
    List(OpCode(0xC9),OpCode(0xD8),OpCode(0xD0),OpCode(0xC8),OpCode(0xC0),OpCode(0xE8),OpCode(0xE0),OpCode(0xF8),OpCode(0xF0),
      OpCode(0xED,0x4D))->LoadLocation.registerAddr("SP",isWord = true),
    List(OpCode(0xC7))->LoadLocation.immediate(0x0000),
    List(OpCode(0xCF))->LoadLocation.immediate(0x0008),
    List(OpCode(0xD7))->LoadLocation.immediate(0x0010),
    List(OpCode(0xDF))->LoadLocation.immediate(0x0018),
    List(OpCode(0xE7))->LoadLocation.immediate(0x0020),
    List(OpCode(0xEF))->LoadLocation.immediate(0x0028),
    List(OpCode(0xF7))->LoadLocation.immediate(0x0030),
    List(OpCode(0xFF))->LoadLocation.immediate(0x0038)
  )

  val location: OpCodeMap[LoadLocation] = new OpCodeMap(locationListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0xC3),OpCode(0xDA),OpCode(0xD2),OpCode(0xCA),OpCode(0xC2),OpCode(0xEA),OpCode(0xE2),
      OpCode(0xFA),OpCode(0xF2),
      OpCode(0xCD),OpCode(0xDC),OpCode(0xD4),OpCode(0xCC),OpCode(0xC4),OpCode(0xEC),OpCode(0xE4),
      OpCode(0xFC),OpCode(0xF4))->3,
    List(OpCode(0xE9),
      OpCode(0xC9),OpCode(0xD8),OpCode(0xD0),OpCode(0xC8),OpCode(0xC0),OpCode(0xE8),OpCode(0xE0),
      OpCode(0xF8),OpCode(0xF0),
      OpCode(0xC7),OpCode(0xCF),OpCode(0xD7),OpCode(0xDF),OpCode(0xE7),OpCode(0xEF),
      OpCode(0xF7),OpCode(0xFF))->1,
    List(OpCode(0xDD,0xE9),OpCode(0xFD,0xE9),OpCode(0x18),OpCode(0x38),OpCode(0x30),OpCode(0x28),OpCode(0x20),
      OpCode(0xED,0x4D),
      OpCode(0x10))->2
  )

  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code:OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int)={
    val oper = operation.find(code)
    val instrSize = instSize.find(code)
    val checker = new JumpConditionChecker(condition.find(code))

    val changePC=
      handleJump(
        oper,
        condition.find(code),
        checker,
        location.find(code),
        instrSize)
    val changeStack=handleStack(checker.isMet,oper,instrSize)

    (changePC++changeStack,0)
  }

  private def calcAddress(oper:JumpOperation,value:Int,prevPC:Int):Int=
    oper match {
      case JumpType.JumpR | JumpType.DJumpR => calcRelativeAddress(prevPC,value)
      case _ => value
    }

  // Jump relative - relative operand is 2's complement and must be incremented by 2
  private def calcRelativeAddress(pc:Int,relative:Int):Int=Z80Utils.word2ComplToRaw(pc+2+Z80Utils.rawByteTo2Compl(relative))

  private def handleJump(oper:JumpOperation,cond:JumpCondition,checker:JumpConditionChecker,location:LoadLocation,instrSize:Int)
                        (implicit system:Z80System):List[SystemChangeBase]= {
    val prevPC=system.getRegValue("PC")
    val address=calcAddress(oper,system.getValueFromLocation(location),prevPC)
    val registerDecrValue=checker.decRegValue
    val newPC = chooseAddress(prevPC, address, checker)
    val changePC=List(new RegisterChange("PC", newPC + (if (!checker.isMet) instrSize else 0)))
    val changeReg=(oper,cond) match {
      case (JumpType.DJumpR,c) if c.isRegister => List(new RegisterChange(c.register, registerDecrValue))
      case _ => List()
    }
    changePC ++ changeReg
  }

  private def chooseAddress(prevPC:Int,address:Int,checker:JumpConditionChecker):Int={
    if(checker.isMet) address else prevPC
  }

  private def handleStack(shouldJump:Boolean,oper:JumpOperation,instrSize:Int)(implicit system:Z80System):List[SystemChangeBase]=
    (shouldJump,oper) match {
      case (true,JumpType.Call) => List(
        new MemoryChangeWord(system.getRegValue("SP")-2,system.getRegValue("PC")+instrSize),
        new RegisterChangeRelative("SP",-2)
      )
      case (true,JumpType.Return) => List(
        new RegisterChangeRelative("SP",2)
      )
      case _ => List()
    }
}
