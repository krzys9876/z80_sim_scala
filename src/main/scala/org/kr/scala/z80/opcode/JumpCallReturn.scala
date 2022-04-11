package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, FlagSymbol}

case class JumpCondition(flag:FlagSymbol,value:Boolean)

sealed abstract class JumpOperation(val name:String)

object JumpType {
  case object Jump extends JumpOperation("JUMP")
  case object JumpR extends JumpOperation("JUMP_RELATIVE")
  case object Call extends JumpOperation("CALL")
  case object Return extends JumpOperation("RETURN")
  case object None extends JumpOperation("NONE")
}

object JumpCallReturn extends OperationSpec{
  //Z80 manual p.59
  val conditionListMap: Map[List[OpCode],JumpCondition] = Map(
    List(OpCode(0xC3))->JumpCondition(Flag.None,value=false),
    List(OpCode(0xDA))->JumpCondition(Flag.C,value=true),
    List(OpCode(0xD2))->JumpCondition(Flag.C,value=false),
    List(OpCode(0xCA))->JumpCondition(Flag.Z,value=true),
    List(OpCode(0xC2))->JumpCondition(Flag.Z,value=false),
    List(OpCode(0xEA))->JumpCondition(Flag.P,value=true),
    List(OpCode(0xE2))->JumpCondition(Flag.P,value=false),
    List(OpCode(0xFA))->JumpCondition(Flag.S,value=true),
    List(OpCode(0xF2))->JumpCondition(Flag.S,value=false),
    List(OpCode(0xE9))->JumpCondition(Flag.None,value=false),
    List(OpCode(0xDD,0xE9))->JumpCondition(Flag.None,value=false),
    List(OpCode(0xFD,0xE9))->JumpCondition(Flag.None,value=false),
    List(OpCode(0x18))->JumpCondition(Flag.None,value=false),
    List(OpCode(0x38))->JumpCondition(Flag.C,value=true),
    List(OpCode(0x30))->JumpCondition(Flag.C,value=false),
    List(OpCode(0x28))->JumpCondition(Flag.Z,value=true),
    List(OpCode(0x20))->JumpCondition(Flag.Z,value=false),
    List(OpCode(0xCD))->JumpCondition(Flag.None,value=false),
    List(OpCode(0xDC))->JumpCondition(Flag.C,value=true),
    List(OpCode(0xD4))->JumpCondition(Flag.C,value=false),
    List(OpCode(0xCC))->JumpCondition(Flag.Z,value=true),
    List(OpCode(0xC4))->JumpCondition(Flag.Z,value=false),
    List(OpCode(0xEC))->JumpCondition(Flag.P,value=true),
    List(OpCode(0xE4))->JumpCondition(Flag.P,value=false),
    List(OpCode(0xFC))->JumpCondition(Flag.S,value=true),
    List(OpCode(0xF4))->JumpCondition(Flag.S,value=false),
    List(OpCode(0xC9))->JumpCondition(Flag.None,value=false),
    List(OpCode(0xD8))->JumpCondition(Flag.C,value=true),
    List(OpCode(0xD0))->JumpCondition(Flag.C,value=false),
    List(OpCode(0xC8))->JumpCondition(Flag.Z,value=true),
    List(OpCode(0xC0))->JumpCondition(Flag.Z,value=false),
    List(OpCode(0xE8))->JumpCondition(Flag.P,value=true),
    List(OpCode(0xE0))->JumpCondition(Flag.P,value=false),
    List(OpCode(0xF8))->JumpCondition(Flag.S,value=true),
    List(OpCode(0xF0))->JumpCondition(Flag.S,value=false)
  )

  val condition: OpCodeMap[JumpCondition] = new OpCodeMap(conditionListMap, JumpCondition(Flag.None,value=false))

  val operationListMap: Map[List[OpCode],JumpOperation] = Map(
    List(OpCode(0xC3),OpCode(0xDA),OpCode(0xD2),OpCode(0xCA),OpCode(0xC2),OpCode(0xEA),OpCode(0xE2),
      OpCode(0xFA),OpCode(0xF2),OpCode(0xE9),OpCode(0xDD,0xE9),OpCode(0xFD,0xE9))->JumpType.Jump,
    List(OpCode(0x18),OpCode(0x38),OpCode(0x30),OpCode(0x28),OpCode(0x20))->JumpType.JumpR,
    List(OpCode(0xCD),OpCode(0xDC),OpCode(0xD4),OpCode(0xCC),OpCode(0xC4),OpCode(0xEC),OpCode(0xE4),
      OpCode(0xFC),OpCode(0xF4))->JumpType.Call,
    List(OpCode(0xC9),OpCode(0xD8),OpCode(0xD0),OpCode(0xC8),OpCode(0xC0),OpCode(0xE8),OpCode(0xE0),
      OpCode(0xF8),OpCode(0xF0))->JumpType.Return
  )

  val operation: OpCodeMap[JumpOperation] = new OpCodeMap(operationListMap, JumpType.None)

  val locationListMap: Map[List[OpCode],LoadLocation] = Map(
    List(OpCode(0xC3),OpCode(0xDA),OpCode(0xD2),OpCode(0xCA),OpCode(0xC2),OpCode(0xEA),OpCode(0xE2),
      OpCode(0xFA),OpCode(0xF2))->LoadLocation.registerAddrDirOffset("PC",1,isWord = true),
    List(OpCode(0xE9))->LoadLocation.register("HL"),
    List(OpCode(0xDD,0xE9))->LoadLocation.register("IX"),
    List(OpCode(0xFD,0xE9))->LoadLocation.register("IY"),
    List(OpCode(0x18),OpCode(0x38),OpCode(0x30),OpCode(0x28),OpCode(0x20))->LoadLocation.registerAddrDirOffset("PC",1),
    List(OpCode(0xCD),OpCode(0xDC),OpCode(0xD4),OpCode(0xCC),OpCode(0xC4),OpCode(0xEC),OpCode(0xE4),
      OpCode(0xFC),OpCode(0xF4))->LoadLocation.registerAddrDirOffset("PC",1,isWord = true),
    List(OpCode(0xC9),OpCode(0xD8),OpCode(0xD0),OpCode(0xC8),OpCode(0xC0),OpCode(0xE8),OpCode(0xE0),
      OpCode(0xF8),OpCode(0xF0))->LoadLocation.registerAddr("SP",isWord = true)
  )

  val location: OpCodeMap[LoadLocation] = new OpCodeMap(locationListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0xC3),OpCode(0xDA),OpCode(0xD2),OpCode(0xCA),OpCode(0xC2),OpCode(0xEA),OpCode(0xE2),
      OpCode(0xFA),OpCode(0xF2),
      OpCode(0xCD),OpCode(0xDC),OpCode(0xD4),OpCode(0xCC),OpCode(0xC4),OpCode(0xEC),OpCode(0xE4),
      OpCode(0xFC),OpCode(0xF4))->3,
    List(OpCode(0xE9),
      OpCode(0xC9),OpCode(0xD8),OpCode(0xD0),OpCode(0xC8),OpCode(0xC0),OpCode(0xE8),OpCode(0xE0),
      OpCode(0xF8),OpCode(0xF0))->1,
    List(OpCode(0xDD,0xE9),OpCode(0xFD,0xE9),OpCode(0x18),OpCode(0x38),OpCode(0x30),OpCode(0x28),OpCode(0x20))->2
  )

  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)
}
