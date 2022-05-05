package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, FlagSymbol, MemoryChangeWord, RegisterChange, RegisterChangeRelative, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

case class JumpCondition(flag:FlagSymbol,register:String,value:Int) {
  lazy val flagValue:Boolean = value!=0
  lazy val isFlag:Boolean = flag!=Flag.None
  lazy val isRegister:Boolean = register!=""
  lazy val isEmpty:Boolean = this.equals(JumpCondition.empty)

  override def toString:String=if(isEmpty) "empty" else f"$flag/${if(register.nonEmpty) register else "-"}=$value"
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
      case c if c.isFlag => system.getFlags.flagValue(condition.flag) == condition.value
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
  val condition: OpCodeMap[JumpCondition] = new OpCodeMap(OpCodes.jumpConditionMap, JumpCondition.empty)
  val operation: OpCodeMap[JumpOperation] = new OpCodeMap(OpCodes.jumpOperationMap, JumpType.None)
  val location: OpCodeMap[Location] = new OpCodeMap(OpCodes.sourceMap, Location.empty)
  override val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)
  override lazy val isOper: OpCode => Boolean = opcode => operation.contains(opcode)

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

  private def handleJump(oper:JumpOperation, cond:JumpCondition, checker:JumpConditionChecker, location:Location, instrSize:Int)
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

  private def chooseAddress(prevPC:Int,address:Int,checker:JumpConditionChecker):Int=
    if(checker.isMet) address else prevPC

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
