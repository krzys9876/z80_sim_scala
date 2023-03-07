package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{ArithmeticCalculatorByte, ArithmeticOpInput, ArithmeticOpResult, ArithmeticOperation, EmptyLocation, FlagCBorrow, FlagCCarry, FlagCInvert, FlagCReset, FlagCSet, FlagHBorrow, FlagHCarryByte, FlagHCopyC, FlagHReset, FlagHSet, FlagNReset, FlagNSet, FlagPOverflowByte, FlagPParity, FlagSSignByte, FlagZZero, Location, OpCode, OpCodeArithmetic8b, OpCodeDestLocation, OpCodeSize, OpCodeSourceLocation, OpCodeTCycles}
import org.kr.scala.z80.system.{Debugger, DummyChange, Flag, RegisterChange, Regs, SystemChange, Z80System}
import org.kr.scala.z80.utils.{IntValue, OptionInt, Z80Utils}

object Arithmetic8Bit extends OpCodeHandler {
  // Z80 manual page 50 (NOTE: ADD A,(HL) is 0x86, not 0x88!
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChange], Int, Int) = {
    val actualCode=castType[OpCode with OpCodeArithmetic8b with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize with OpCodeTCycles](code)

    val oper = actualCode.operation
    val destLoc = actualCode.destination
    val dest = system.getOptionalValueFromLocation(destLoc)
    val prevFlags = system.getFlags
    val sourceLocation = actualCode.source
    val prevValue = system.getValueFromLocation(sourceLocation)
    val destLocation = oper.getDestination(sourceLocation)

    val (result, flags) = oper.calcAll(ArithmeticOpInput(prevValue, dest, prevFlags))
    val chgList = destLocation match {
      case EmptyLocation => List(new DummyChange())
      case _ => List(system.putValueToLocation(destLocation, result.valueOut))
    }
    (chgList ++ List(new RegisterChange(Regs.F, flags())), actualCode.size, actualCode.t)
  }
}

object Add8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHCarryByte with FlagPOverflowByte with FlagNReset with FlagCCarry {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value + input.operand())
  override def calcSigned(input: ArithmeticOpInput): OptionInt =
    IntValue(Z80Utils.rawByteTo2Compl(input.value) + Z80Utils.rawByteTo2Compl(input.operand()))
  override def calcAux(input: ArithmeticOpInput): OptionInt = IntValue((input.value & 0x0F)+(input.operand() & 0x0F))
}

object AddC8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHCarryByte with FlagPOverflowByte with FlagNReset with FlagCCarry {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value + input.operand() + input.flags.flagValue(Flag.C))
  override def calcSigned(input: ArithmeticOpInput): OptionInt =
    IntValue(Z80Utils.rawByteTo2Compl(input.value) + Z80Utils.rawByteTo2Compl(input.operand()) + input.flags.flagValue(Flag.C))
  override def calcAux(input: ArithmeticOpInput): OptionInt =
    IntValue((input.value & 0x0F)+(input.operand() & 0x0F)+input.flags.flagValue(Flag.C))
}

object Sub8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value - input.operand())
  override def calcSigned(input: ArithmeticOpInput): OptionInt =
    IntValue(Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand()))
  override def calcAux(input: ArithmeticOpInput): OptionInt =
    IntValue((input.value & 0x0F)-(input.operand() & 0x0F))
}

object SubC8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value - input.operand() - input.flags.flagValue(Flag.C))
  override def calcSigned(input: ArithmeticOpInput): OptionInt =
    IntValue(Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand()) - input.flags.flagValue(Flag.C))
  override def calcAux(input: ArithmeticOpInput): OptionInt =
    IntValue((input.value & 0x0F) - (input.operand() & 0x0F) - input.flags.flagValue(Flag.C))
}

object And8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHSet with FlagPParity with FlagNReset with FlagCReset {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value & input.operand())
}

object Xor8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCReset {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value ^ input.operand())
}

object Or8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCReset {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value | input.operand())
}

object Comp8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value - input.operand())
  override def calcSigned(input: ArithmeticOpInput): OptionInt =
    IntValue(Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand()))
  override def calcAux(input: ArithmeticOpInput): OptionInt =
    IntValue((input.value & 0x0F)-(input.operand() & 0x0F))

  override def getDestination(source:Location):Location=EmptyLocation
}

object Dec8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value - 1)
  override def calcSigned(input: ArithmeticOpInput): OptionInt =
    IntValue(Z80Utils.rawByteTo2Compl(input.value) - 1)
  override def calcAux(input: ArithmeticOpInput): OptionInt = IntValue((input.value & 0x0F)-1)
}

object Inc8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHCarryByte with FlagPOverflowByte with FlagNReset {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value + 1)
  override def calcSigned(input: ArithmeticOpInput): OptionInt =
    IntValue(Z80Utils.rawByteTo2Compl(input.value) + 1)
  override def calcAux(input: ArithmeticOpInput): OptionInt = IntValue((input.value & 0x0F)+1)
}

object Cpl8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagHSet with FlagNSet {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(~input.value)
}

object Neg8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(0 - input.value)
  override def calcSigned(input: ArithmeticOpInput): OptionInt = IntValue(0 - Z80Utils.rawByteTo2Compl(input.value))
  override def calcAux(input: ArithmeticOpInput): OptionInt = IntValue(0 - (input.value & 0x0F))
}

object Ccf8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagHCopyC with FlagNReset with FlagCInvert {

  override def getDestination(source:Location):Location=EmptyLocation
}

object Scf8b extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagHReset with FlagNReset with FlagCSet {

  override def getDestination(source:Location):Location=EmptyLocation
}

object Daa extends ArithmeticOperation with ArithmeticCalculatorByte with FlagSSignByte {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = {
    val (ah,al,c,h)=((input.value & 0xF0) >> 4,input.value & 0x0F,input.flags.flagValue(Flag.C),input.flags.flagValue(Flag.H))
    // http://www.z80.info/zip/z80-documented.pdf page 18 table 1
    val correction = (c,ah,h,al) match {
      case(0,ah1,1,al1) if ah1<=0x9 && al1<=0x9 => 0x06
      case(0,ah1,_,al1) if ah1<=0x8 && al1>=0xA => 0x06
      case(0,ah1,0,al1) if ah1>=0xA && al1<=0x9 => 0x60
      case(1, _ ,0,al1) if             al1<=0x9 => 0x60
      case(1, _ ,1,al1) if             al1<=0x9 => 0x66
      case(1, _ ,_,al1) if             al1>=0xA => 0x66
      case(0,ah1,_,al1) if ah1>=0x9 && al1>=0xA => 0x66
      case(0,ah1,1,al1) if ah1>=0xA && al1<=0x9 => 0x66
      case _ => 0x00
    }
    val outValue=input.value + (if(input.flags(Flag.N)) -correction else correction)
    IntValue(outValue)
  }
  override def calcC(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean= {
    // http://www.z80.info/zip/z80-documented.pdf h table 2
    val (ah,al,c)=((input.value & 0xF0) >> 4,input.value & 0x0F,input.flags.flagValue(Flag.C))
    (c,ah,al) match {
      case (0,ah1,al1) if ah1>=0x09 && al1>=0xA => true
      case (0,ah1,al1) if ah1>=0x0A && al1<=0x9 => true
      case (1, _ , _ )                          => true
      case _ => false
    }
  }
  override def calcH(res: ArithmeticOpResult, input: ArithmeticOpInput): Boolean = {
    // http://www.z80.info/zip/z80-documented.pdf h table 3
    val (al, n, h) = (input.value & 0x0F, input.flags.flagValue(Flag.N),input.flags.flagValue(Flag.H))
    (n, h, al) match {
      case (0,_,al1) if al1>=0xA => true
      case (1,1,al1) if al1<=0x5 => true
      case _ => false
    }
  }
}
