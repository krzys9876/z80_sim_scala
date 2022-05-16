package org.kr.scala.z80.opcode

import org.kr.scala.z80.opcode.handler.{Add16b, Add8b, AddC16b, AddC8b, And8b, Arithmetic16Bit, Arithmetic8Bit, BitManipulation, BitOpType, BitOperation, Ccf8b, Comp8b, Cpl8b, Dec16b, Dec8b, EmptyJumpCondition, Exchange, ExchangeLocation, ExchangeLocationBase, ExchangeLocationIndirect, FlagJumpCondition, InOutOpType, InOutOperation, Inc16b, Inc8b, InputOutput, JumpCallReturn, JumpConditionBase, JumpOperation, JumpType, Load16Bit, Load8Bit, Neg8b, Nop, OpCodeHandler, Or8b, RegisterJumpCondition, RotShRl, RotShRla, RotShRlc, RotShRlca, RotShRr, RotShRra, RotShRrc, RotShRrca, RotShSla, RotShSra, RotShSrl, RotateDL, RotateDR, RotateDigit, RotateShift, Scf8b, Sub8b, SubC16b, SubC8b, Unknown, Xor8b}
import org.kr.scala.z80.system.{Flag, Regs}
import org.kr.scala.z80.utils.IntValue

trait Label {
  val label:String
  override def toString:String=label
}

trait OpCodeHandledBy {
  val handler:OpCodeHandler
}
trait HandleNop extends OpCodeHandledBy {override val handler:OpCodeHandler=Nop}
trait HandleLoad8Bit extends OpCodeHandledBy {override val handler:OpCodeHandler=Load8Bit}
trait HandleLoad16Bit extends OpCodeHandledBy {override val handler:OpCodeHandler=Load16Bit}
trait HandleExchange extends OpCodeHandledBy {override val handler:OpCodeHandler=Exchange}
trait HandleArithmetic8Bit extends OpCodeHandledBy {override val handler:OpCodeHandler=Arithmetic8Bit}
trait HandleArithmetic16Bit extends OpCodeHandledBy {override val handler:OpCodeHandler=Arithmetic16Bit}
trait HandleRotateShift extends OpCodeHandledBy {override val handler:OpCodeHandler=RotateShift}
trait HandleRotateDigit extends OpCodeHandledBy {override val handler:OpCodeHandler=RotateDigit}
trait HandleBitManipulation extends OpCodeHandledBy {override val handler:OpCodeHandler=BitManipulation}
trait HandleJump extends OpCodeHandledBy {override val handler:OpCodeHandler=JumpCallReturn}
trait HandleInOut extends OpCodeHandledBy {override val handler:OpCodeHandler=InputOutput}
trait HandleUnknown extends OpCodeHandledBy {override val handler:OpCodeHandler=Unknown}


//Building blocks for OpCode definition
trait OpCodeSourceLocation {
  val source:Location
}
trait SourceA extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.A)}
trait SourceB extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.B)}
trait SourceC extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.C)}
trait SourceD extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.D)}
trait SourceE extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.E)}
trait SourceH extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.H)}
trait SourceL extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.L)}
trait SourceHLra extends OpCodeSourceLocation {override val source:Location=RegisterAddrLocation(Regs.HL)}
trait SourceBC extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.BC)}
trait SourceDE extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.DE)}
trait SourceHL extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.HL)}
trait SourceSP extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.SP)}
trait SourceIX extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.IX)}
trait SourceIY extends OpCodeSourceLocation {override val source:Location=RegisterLocation(Regs.IY)}
trait SourceIXd extends OpCodeSourceLocation {override val source:Location=RegisterAddrIndirOffsetLocation(Regs.IX, 2)}
trait SourceIYd extends OpCodeSourceLocation {override val source:Location=RegisterAddrIndirOffsetLocation(Regs.IY, 2)}
trait SourceN extends OpCodeSourceLocation {override val source:Location=RegisterAddrDirOffsetLocation(Regs.PC, 1)}
trait SourceNw extends OpCodeSourceLocation {override val source:Location=RegisterAddrDirOffsetLocation(Regs.PC, 1, isWord = true)}
trait SourceStack extends OpCodeSourceLocation {override val source:Location=RegisterAddrLocation(Regs.SP,isWord = true)}
trait Source00h extends OpCodeSourceLocation {override val source:Location=ImmediateLocation(0x0000,isWord=true)}
trait Source08h extends OpCodeSourceLocation {override val source:Location=ImmediateLocation(0x0008,isWord=true)}
trait Source10h extends OpCodeSourceLocation {override val source:Location=ImmediateLocation(0x0010,isWord=true)}
trait Source18h extends OpCodeSourceLocation {override val source:Location=ImmediateLocation(0x0018,isWord=true)}
trait Source20h extends OpCodeSourceLocation {override val source:Location=ImmediateLocation(0x0020,isWord=true)}
trait Source28h extends OpCodeSourceLocation {override val source:Location=ImmediateLocation(0x0028,isWord=true)}
trait Source30h extends OpCodeSourceLocation {override val source:Location=ImmediateLocation(0x0030,isWord=true)}
trait Source38h extends OpCodeSourceLocation {override val source:Location=ImmediateLocation(0x0038,isWord=true)}

trait OpCodeDestLocation {
  val destination:Location
}
trait DestinationA extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.A)}
trait DestinationB extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.B)}
trait DestinationC extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.C)}
trait DestinationD extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.D)}
trait DestinationE extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.E)}
trait DestinationH extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.H)}
trait DestinationL extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.L)}
trait DestinationHLra extends OpCodeDestLocation {override val destination:Location=RegisterAddrLocation(Regs.HL)}
trait DestinationBC extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.BC)}
trait DestinationDE extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.DE)}
trait DestinationHL extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.HL)}
trait DestinationSP extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.SP)}
trait DestinationIX extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.IX)}
trait DestinationIY extends OpCodeDestLocation {override val destination:Location=RegisterLocation(Regs.IY)}
trait DestinationIXd extends OpCodeDestLocation {override val destination:Location=RegisterAddrIndirOffsetLocation(Regs.IX, 2)}
trait DestinationIYd extends OpCodeDestLocation {override val destination:Location=RegisterAddrIndirOffsetLocation(Regs.IY, 2)}
trait DestinationN extends OpCodeDestLocation {override val destination:Location=RegisterAddrDirOffsetLocation(Regs.PC, 1)}
trait DestinationEmpty extends OpCodeDestLocation {override val destination:Location=EmptyLocation()}

trait SourceDestA extends SourceA with DestinationA
trait SourceDestB extends SourceB with DestinationB
trait SourceDestC extends SourceC with DestinationC
trait SourceDestD extends SourceD with DestinationD
trait SourceDestE extends SourceE with DestinationE
trait SourceDestH extends SourceH with DestinationH
trait SourceDestL extends SourceL with DestinationL
trait SourceDestHLra extends SourceHLra with DestinationHLra
trait SourceDestBC extends SourceBC with DestinationBC
trait SourceDestDE extends SourceDE with DestinationDE
trait SourceDestHL extends SourceHL with DestinationHL
trait SourceDestSP extends SourceSP with DestinationSP
trait SourceDestIX extends SourceIX with DestinationIX
trait SourceDestIY extends SourceIY with DestinationIY
trait SourceDestIXd extends SourceIXd with DestinationIXd
trait SourceDestIYd extends SourceIYd with DestinationIYd

// copies destination to source - useful for INC and DEC
trait SourceFromDestination extends OpCodeSourceLocation with OpCodeDestLocation {
  override val source:Location=destination
}

trait OpCodeArithmetic8b {
  val operation:ArithmeticOperation
}
trait Arith8bAdd extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=Add8b}
trait Arith8bAddC extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=AddC8b}
trait Arith8bSub extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=Sub8b}
trait Arith8bSubC extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=SubC8b}
trait Arith8bAnd extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=And8b}
trait Arith8bXor extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=Xor8b}
trait Arith8bOr extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=Or8b}
trait Arith8bCp extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=Comp8b}
trait Arith8bInc extends OpCodeArithmetic8b {override val operation:ArithmeticOperation=Inc8b}
trait Arith8bDec extends OpCodeArithmetic8b {override val operation:ArithmeticOperation=Dec8b}
trait Arith8bCpl extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Cpl8b}
trait Arith8bNeg extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Neg8b}
trait Arith8bCcf extends OpCodeArithmetic8b with SourceA with DestinationEmpty {override val operation:ArithmeticOperation=Ccf8b}
trait Arith8bScf extends OpCodeArithmetic8b with SourceA with DestinationEmpty {override val operation:ArithmeticOperation=Scf8b}

trait OpCodeRotateDigit extends SourceA with DestinationHLra {
  val operation:ArithmeticOperation
}
trait RotateDigitLeft extends OpCodeRotateDigit {override val operation:ArithmeticOperation=RotateDL}
trait RotateDigitRight extends OpCodeRotateDigit {override val operation:ArithmeticOperation=RotateDR}

trait OpCodeSize {
  val size:Int
}

trait Size1 extends OpCodeSize {override val size:Int=1}
trait Size2 extends OpCodeSize {override val size:Int=2}
trait Size3 extends OpCodeSize {override val size:Int=3}
trait Size4 extends OpCodeSize {override val size:Int=4}

trait OpCodeTCycles {
  val t:Int
  val tConditional:Int=0
}

trait T4 extends OpCodeTCycles {override val t:Int=4}
trait T5 extends OpCodeTCycles {override val t:Int=5}
trait T5T11 extends OpCodeTCycles {
  override val t:Int=5
  override val tConditional:Int=6
}
trait T6 extends OpCodeTCycles {override val t:Int=6}
trait T7 extends OpCodeTCycles {override val t:Int=7}
trait T7T12 extends OpCodeTCycles {
  override val t:Int=7
  override val tConditional:Int=5
}
trait T8 extends OpCodeTCycles {override val t:Int=8}
trait T10 extends OpCodeTCycles {override val t:Int=10}
trait T10T17 extends OpCodeTCycles {
  override val t:Int=10
  override val tConditional:Int=7
}
trait T11 extends OpCodeTCycles {override val t:Int=11}
trait T12 extends OpCodeTCycles {override val t:Int=12}
trait T13T8 extends OpCodeTCycles {
  override val t:Int=13
  override val tConditional:Int= -5
}
trait T14 extends OpCodeTCycles {override val t:Int=14}
trait T15 extends OpCodeTCycles {override val t:Int=15}
trait T17 extends OpCodeTCycles {override val t:Int=17}
trait T18 extends OpCodeTCycles {override val t:Int=18}
trait T19 extends OpCodeTCycles {override val t:Int=19}
trait T23 extends OpCodeTCycles {override val t:Int=23}


trait OpCodeArithmetic16b {
  val operation:ArithmeticOperation
}
trait Arith16bAdd extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=Add16b}
trait Arith16bAddC extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=AddC16b}
trait Arith16bSubC extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=SubC16b}
trait Arith16bInc extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=Inc16b}
trait Arith16bDec extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=Dec16b}

trait OpCodeExchangeLocation {
  val exchange:List[ExchangeLocationBase]
}
trait ExchangeDEHL extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocation(Regs.DE,Regs.HL))}
trait ExchangeAF1 extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocation(Regs.AF,Regs.AF1))}
trait ExchangeAll1 extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=
  List(new ExchangeLocation(Regs.BC,Regs.BC1),new ExchangeLocation(Regs.DE,Regs.DE1),new ExchangeLocation(Regs.HL,Regs.HL1))}
trait ExchangeSPHL extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocationIndirect(Regs.SP,Regs.HL))}
trait ExchangeSPIX extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocationIndirect(Regs.SP,Regs.IX))}
trait ExchangeSPIY extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocationIndirect(Regs.SP,Regs.IY))}

trait OpCodeBitManipulation {
  val operation:BitOperation
  val bit:Int
}
trait BitTest extends OpCodeBitManipulation {override val operation:BitOperation=BitOpType.Test}
trait BitReset extends OpCodeBitManipulation {override val operation:BitOperation=BitOpType.Reset}
trait BitSet extends OpCodeBitManipulation {override val operation:BitOperation=BitOpType.Set}

trait OpStackChange  {
  val stackChange:Int
}
trait PushStack extends OpStackChange {override val stackChange:Int= -2}
trait PopStack extends OpStackChange {override val stackChange:Int= 2}
trait DoNotUseStack extends OpStackChange {override val stackChange:Int=0}

trait OpCodeRotateShift {
  val operation:ArithmeticOperation
}
trait RotateShiftRlc extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRlc}
trait RotateShiftRlca extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRlca}
trait RotateShiftRrc extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRrc}
trait RotateShiftRrca extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRrca}
trait RotateShiftRl extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRl}
trait RotateShiftRla extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRla}
trait RotateShiftRr extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRr}
trait RotateShiftRra extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShRra}
trait RotateShiftSla extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShSla}
trait RotateShiftSra extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShSra}
trait RotateShiftSrl extends OpCodeRotateShift {override val operation:ArithmeticOperation=RotShSrl}

trait OpCodeJump {
  val operation:JumpOperation
}
trait JumpOper extends OpCodeJump {override val operation:JumpOperation=JumpType.Jump}
trait JumpRelativeOper extends OpCodeJump {override val operation:JumpOperation=JumpType.JumpR}
trait JumpRelativeDecOper extends OpCodeJump {override val operation:JumpOperation=JumpType.DJumpR}
trait DecrJumpRelativeOper extends OpCodeJump {override val operation:JumpOperation=JumpType.DJumpR}
trait CallOper extends OpCodeJump {override val operation:JumpOperation=JumpType.Call}
trait ReturnOper extends OpCodeJump {override val operation:JumpOperation=JumpType.Return}

trait OpCodeJumpCondition {
  val condition:JumpConditionBase
}
trait JumpUnconditional extends OpCodeJumpCondition {override val condition:JumpConditionBase=EmptyJumpCondition()}
trait JumpC extends OpCodeJumpCondition {override val condition:JumpConditionBase=FlagJumpCondition(Flag.C,boolValue=true)}
trait JumpNC extends OpCodeJumpCondition {override val condition:JumpConditionBase=FlagJumpCondition(Flag.C,boolValue=false)}
trait JumpZ extends OpCodeJumpCondition {override val condition:JumpConditionBase=FlagJumpCondition(Flag.Z,boolValue=true)}
trait JumpNZ extends OpCodeJumpCondition {override val condition:JumpConditionBase=FlagJumpCondition(Flag.Z,boolValue=false)}
trait JumpB0 extends OpCodeJumpCondition {override val condition:JumpConditionBase=RegisterJumpCondition(Regs.B,IntValue(0))}

trait OpCodeInOut {
  val operation:InOutOperation
}
trait InOper extends OpCodeInOut {override val operation:InOutOperation=InOutOpType.In}
trait OutOper extends OpCodeInOut {override val operation:InOutOperation=InOutOpType.Out}


