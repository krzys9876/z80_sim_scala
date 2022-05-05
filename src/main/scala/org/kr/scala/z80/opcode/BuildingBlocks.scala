package org.kr.scala.z80.opcode

import org.kr.scala.z80.opcode.handler.{Add16b, Add8b, AddC16b, AddC8b, And8b, Arithmetic16Bit, Arithmetic8Bit, BitOpType, BitOperation, Ccf8b, Comp8b, Cpl8b, Dec16b, Dec8b, ExchangeLocation, ExchangeLocationBase, ExchangeLocationIndirect, InOutOpType, InOutOperation, Inc16b, Inc8b, InputOutput, JumpCallReturn, JumpCondition, JumpOperation, JumpType, Load16Bit, Load16BitOpType, Load8Bit, Load8BitOpType, Neg8b, Nop, OpCodeHandler, Or8b, RotShRl, RotShRla, RotShRlc, RotShRlca, RotShRr, RotShRra, RotShRrc, RotShRrca, RotShSla, RotShSra, RotShSrl, RotateDL, RotateDR, RotateDigit, RotateShift, Scf8b, Sub8b, SubC16b, SubC8b, Xor8b}
import org.kr.scala.z80.system.Flag

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
trait HandleArithmetic8Bit extends OpCodeHandledBy {override val handler:OpCodeHandler=Arithmetic8Bit}
trait HandleArithmetic16Bit extends OpCodeHandledBy {override val handler:OpCodeHandler=Arithmetic16Bit}
trait HandleRotateShift extends OpCodeHandledBy {override val handler:OpCodeHandler=RotateShift}
trait HandleRotateDigit extends OpCodeHandledBy {override val handler:OpCodeHandler=RotateDigit}
trait HandleJump extends OpCodeHandledBy {override val handler:OpCodeHandler=JumpCallReturn}
trait HandleInOut extends OpCodeHandledBy {override val handler:OpCodeHandler=InputOutput}


//Building blocks for OpCode definition
trait OpCodeSourceLocation {
  val source:Location
}
trait SourceA extends OpCodeSourceLocation {override val source:Location=Location.register("A")}
trait SourceB extends OpCodeSourceLocation {override val source:Location=Location.register("B")}
trait SourceC extends OpCodeSourceLocation {override val source:Location=Location.register("C")}
trait SourceD extends OpCodeSourceLocation {override val source:Location=Location.register("D")}
trait SourceE extends OpCodeSourceLocation {override val source:Location=Location.register("E")}
trait SourceH extends OpCodeSourceLocation {override val source:Location=Location.register("H")}
trait SourceL extends OpCodeSourceLocation {override val source:Location=Location.register("L")}
trait SourceHLra extends OpCodeSourceLocation {override val source:Location=Location.registerAddr("HL")}
trait SourceBC extends OpCodeSourceLocation {override val source:Location=Location.register("BC")}
trait SourceDE extends OpCodeSourceLocation {override val source:Location=Location.register("DE")}
trait SourceHL extends OpCodeSourceLocation {override val source:Location=Location.register("HL")}
trait SourceSP extends OpCodeSourceLocation {override val source:Location=Location.register("SP")}
trait SourceIX extends OpCodeSourceLocation {override val source:Location=Location.register("IX")}
trait SourceIY extends OpCodeSourceLocation {override val source:Location=Location.register("IY")}
trait SourceIXd extends OpCodeSourceLocation {override val source:Location=Location.registerAddrIndirOffset("IX", 2)}
trait SourceIYd extends OpCodeSourceLocation {override val source:Location=Location.registerAddrIndirOffset("IY", 2)}
trait SourceN extends OpCodeSourceLocation {override val source:Location=Location.registerAddrDirOffset("PC", 1)}
trait SourceNw extends OpCodeSourceLocation {override val source:Location=Location.registerAddrDirOffset("PC", 1, isWord = true)}
trait SourceStack extends OpCodeSourceLocation {override val source:Location=Location.registerAddr("SP",isWord = true)}
trait Source00h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0000)}
trait Source08h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0008)}
trait Source10h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0010)}
trait Source18h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0018)}
trait Source20h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0020)}
trait Source28h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0028)}
trait Source30h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0030)}
trait Source38h extends OpCodeSourceLocation {override val source:Location=Location.immediate(0x0038)}

trait OpCodeDestLocation {
  val destination:Location
}
trait DestinationA extends OpCodeDestLocation {override val destination:Location=Location.register("A")}
trait DestinationB extends OpCodeDestLocation {override val destination:Location=Location.register("B")}
trait DestinationC extends OpCodeDestLocation {override val destination:Location=Location.register("C")}
trait DestinationD extends OpCodeDestLocation {override val destination:Location=Location.register("D")}
trait DestinationE extends OpCodeDestLocation {override val destination:Location=Location.register("E")}
trait DestinationH extends OpCodeDestLocation {override val destination:Location=Location.register("H")}
trait DestinationL extends OpCodeDestLocation {override val destination:Location=Location.register("L")}
trait DestinationHLra extends OpCodeDestLocation {override val destination:Location=Location.registerAddr("HL")}
trait DestinationBC extends OpCodeDestLocation {override val destination:Location=Location.register("BC")}
trait DestinationDE extends OpCodeDestLocation {override val destination:Location=Location.register("DE")}
trait DestinationHL extends OpCodeDestLocation {override val destination:Location=Location.register("HL")}
trait DestinationSP extends OpCodeDestLocation {override val destination:Location=Location.register("SP")}
trait DestinationIX extends OpCodeDestLocation {override val destination:Location=Location.register("IX")}
trait DestinationIY extends OpCodeDestLocation {override val destination:Location=Location.register("IY")}
trait DestinationIXd extends OpCodeDestLocation {override val destination:Location=Location.registerAddrIndirOffset("IX", 2)}
trait DestinationIYd extends OpCodeDestLocation {override val destination:Location=Location.registerAddrIndirOffset("IY", 2)}
trait DestinationN extends OpCodeDestLocation {override val destination:Location=Location.registerAddrDirOffset("PC", 1)}
trait DestinationEmpty extends OpCodeDestLocation {override val destination:Location=Location.empty}

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

// copies source and destination from operand - useful for INC and DEC
trait SourceDestFromOperand extends OpCodeSourceLocation with OpCodeDestLocation with OpCodeOperandLocation {
  override val source:Location=operand
  override val destination:Location=operand
}

trait OpCodeOperandLocation {
  val operand:Location
}

trait OperandA extends OpCodeOperandLocation {override val operand:Location=Location.register("A")}
trait OperandB extends OpCodeOperandLocation {override val operand:Location=Location.register("B")}
trait OperandC extends OpCodeOperandLocation {override val operand:Location=Location.register("C")}
trait OperandD extends OpCodeOperandLocation {override val operand:Location=Location.register("D")}
trait OperandE extends OpCodeOperandLocation {override val operand:Location=Location.register("E")}
trait OperandH extends OpCodeOperandLocation {override val operand:Location=Location.register("H")}
trait OperandL extends OpCodeOperandLocation {override val operand:Location=Location.register("L")}
trait OperandHL extends OpCodeOperandLocation {override val operand:Location=Location.registerAddr("HL")}
trait OperandIXd extends OpCodeOperandLocation {override val operand:Location=Location.registerAddrIndirOffset("IX", 2)}
trait OperandIYd extends OpCodeOperandLocation {override val operand:Location=Location.registerAddrIndirOffset("IY", 2)}
trait OperandN extends OpCodeOperandLocation {override val operand:Location=Location.registerAddrDirOffset("PC", 1)}

trait OpCodeArithmetic8b {
  val operation:ArithmeticOperation
}
trait Arith8bAdd extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Add8b}
trait Arith8bAddC extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=AddC8b}
trait Arith8bSub extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Sub8b}
trait Arith8bSubC extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=SubC8b}
trait Arith8bAnd extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=And8b}
trait Arith8bXor extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Xor8b}
trait Arith8bOr extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Or8b}
trait Arith8bCp extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=Comp8b}
trait Arith8bInc extends OpCodeArithmetic8b {override val operation:ArithmeticOperation=Inc8b}
trait Arith8bDec extends OpCodeArithmetic8b {override val operation:ArithmeticOperation=Dec8b}
trait Arith8bCpl extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Cpl8b}
trait Arith8bNeg extends OpCodeArithmetic8b with SourceDestA {override val operation:ArithmeticOperation=Neg8b}
trait Arith8bCcf extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=Ccf8b}
trait Arith8bScf extends OpCodeArithmetic8b with SourceA {override val operation:ArithmeticOperation=Scf8b}

trait OpCodeRotateDigit extends SourceA with OperandHL {
  val operation:ArithmeticOperation
}
trait RotateDigitLeft extends OpCodeRotateDigit {override val operation:ArithmeticOperation=RotateDL}
trait RotateDigitRight extends OpCodeRotateDigit {override val operation:ArithmeticOperation=RotateDR}

trait OpCodeSize {
  val size:Int
}

trait OpCodeArithmetic16b {
  val operation:ArithmeticOperation
}
trait Arith16bAdd extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=Add16b}
trait Arith16bAddC extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=AddC16b}
trait Arith16bSubC extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=SubC16b}
trait Arith16bInc extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=Inc16b}
trait Arith16bDec extends OpCodeArithmetic16b {override val operation:ArithmeticOperation=Dec16b}

trait Size1 extends OpCodeSize {override val size:Int=1}
trait Size2 extends OpCodeSize {override val size:Int=2}
trait Size3 extends OpCodeSize {override val size:Int=3}
trait Size4 extends OpCodeSize {override val size:Int=4}

trait OpCodeExchangeLocation {
  val exchange:List[ExchangeLocationBase]
}
trait ExchangeDEHL extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocation("DE","HL"))}
trait ExchangeAF1 extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocation("AF","AF1"))}
trait ExchangeAll1 extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=
  List(new ExchangeLocation("BC","BC1"),new ExchangeLocation("DE","DE1"),new ExchangeLocation("HL","HL1"))}
trait ExchangeSPHL extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocationIndirect("SP","HL"))}
trait ExchangeSPIX extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocationIndirect("SP","IX"))}
trait ExchangeSPIY extends OpCodeExchangeLocation {override val exchange:List[ExchangeLocationBase]=List(new ExchangeLocationIndirect("SP","IY"))}

trait OpCodeBitManipulation {
  val operation:BitOperation
}
trait BitTest extends OpCodeBitManipulation {override val operation:BitOperation=BitOpType.Test}
trait BitReset extends OpCodeBitManipulation {override val operation:BitOperation=BitOpType.Reset}
trait BitSet extends OpCodeBitManipulation {override val operation:BitOperation=BitOpType.Set}

trait OpCodeLoad8Bit extends OpCodeSourceLocation with OpCodeDestLocation {
  val operation:Load8BitOpType
}
trait Load8BitOp extends OpCodeLoad8Bit {override val operation:Load8BitOpType=Load8BitOpType.Load}

trait OpStackChange  {
  val stackChange:Int
}
trait PushStack extends OpStackChange {override val stackChange:Int= -2}
trait PopStack extends OpStackChange {override val stackChange:Int= 2}

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
  val condition:JumpCondition
}
trait JumpUnconditional extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.empty}
trait JumpC extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.flag(Flag.C,value=true)}
trait JumpNC extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.flag(Flag.C,value=false)}
trait JumpZ extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.flag(Flag.Z,value=true)}
trait JumpNZ extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.flag(Flag.Z,value=false)}
trait JumpB0 extends OpCodeJumpCondition {override val condition:JumpCondition=JumpCondition.register("B",0)}

trait OpCodeInOut {
  val operation:InOutOperation
}
trait InOper extends OpCodeInOut {override val operation:InOutOperation=InOutOpType.In}
trait OutOper extends OpCodeInOut {override val operation:InOutOperation=InOutOpType.Out}


