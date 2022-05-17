package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.Flag
import org.kr.scala.z80.utils.{AnyInt, OptionInt, Z80Utils}

abstract class ArithmeticOperation extends ArithmeticCalculatorBase with FlagCalculatorBase {
  def calcAll(input:ArithmeticOpInput):(ArithmeticOpResult,Flag)={
    val calcResult=calc(input)
    val calcFlags=flags(calcResult,input)
    (calcResult,calcFlags)
  }
}

object None8b extends ArithmeticOperation with ArithmeticCalculatorByte
object None16b extends ArithmeticOperation with ArithmeticCalculatorWord

case class ArithmeticOpInput(value:Int, operand:OptionInt, flags:Flag)

class ArithmeticOpResult(val valueUnsigned:OptionInt, val valueSigned:OptionInt, val valueAux:OptionInt, val isWord:Boolean=false) {
  lazy val valueOut:Int=valueUnsigned() & (if(isWord) 0xFFFF else 0xFF)
}

class ArithmeticOpResultByte(override val valueUnsigned:OptionInt, override val valueSigned:OptionInt, override val valueAux:OptionInt)
  extends ArithmeticOpResult(valueUnsigned,valueSigned,valueAux,false)

class ArithmeticOpResultWord(override val valueUnsigned:OptionInt, override val valueSigned:OptionInt, override val valueAux:OptionInt)
  extends ArithmeticOpResult(valueUnsigned,valueSigned,valueAux,true)

trait ArithmeticCalculatorBase {
  def calcUnsigned(input:ArithmeticOpInput):OptionInt=AnyInt
  def calcSigned(input:ArithmeticOpInput):OptionInt=calcUnsigned(input)
  def calcAux(input:ArithmeticOpInput):OptionInt=AnyInt

  def calc(input:ArithmeticOpInput):ArithmeticOpResult

  def getDestination(source:Location):Location=source
}

trait ArithmeticCalculatorByte extends ArithmeticCalculatorBase {
  override def calc(input:ArithmeticOpInput):ArithmeticOpResult=
    new ArithmeticOpResultByte(calcUnsigned(input),calcSigned(input),calcAux(input))
}

trait ArithmeticCalculatorWord extends ArithmeticCalculatorBase {
  override def calc(input:ArithmeticOpInput):ArithmeticOpResult=
    new ArithmeticOpResultWord(calcUnsigned(input),calcSigned(input),calcAux(input))
}

//Building blocks for flag calculation
//http://www.z80.info/z80sflag.htm
trait FlagCalculatorBase {
  def calcS(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=input.flags(Flag.S)
  def calcZ(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=input.flags(Flag.Z)
  def calcH(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=input.flags(Flag.H)
  def calcP(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=input.flags(Flag.P)
  def calcN(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=input.flags(Flag.N)
  def calcC(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=input.flags(Flag.C)

  def flags(res:ArithmeticOpResult,input:ArithmeticOpInput):Flag={
    Flag.of(
      calcS(res,input),
      calcZ(res,input),
      calcH(res,input),
      calcP(res,input),
      calcN(res,input),
      calcC(res,input))
  }
}

trait FlagSSignByte extends FlagCalculatorBase {
  override def calcS(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=Z80Utils.isNegativeByte(res.valueUnsigned())
}
trait FlagSSignWord extends FlagCalculatorBase {
  override def calcS(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=Z80Utils.isNegativeWord(res.valueUnsigned())
}
trait FlagZZero extends FlagCalculatorBase {
  override def calcZ(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=res.valueOut==0
}
trait FlagHCarryByte extends FlagCalculatorBase {
  override def calcH(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=res.valueAux() > 0x0F
}
trait FlagHCarryWord extends FlagCalculatorBase {
  override def calcH(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=res.valueAux() > 0x0FFF
}
trait FlagHBorrow extends FlagCalculatorBase {
  override def calcH(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=res.valueAux() < 0
}
trait FlagHSet extends FlagCalculatorBase {
  override def calcH(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=true
}
trait FlagHReset extends FlagCalculatorBase {
  override def calcH(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=false
}
trait FlagHCopyC extends FlagCalculatorBase {
  override def calcH(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=input.flags(Flag.C)
}
trait FlagPOverflowByte extends FlagCalculatorBase {
  override def calcP(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=Z80Utils.isOutOfRangeByte(res.valueSigned())
}
trait FlagPOverflowWord extends FlagCalculatorBase {
  override def calcP(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=Z80Utils.isOutOfRangeWord(res.valueSigned())
}
trait FlagPParity extends FlagCalculatorBase {
  override def calcP(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=Z80Utils.isEvenBits(res.valueUnsigned())
}
trait FlagNSet extends FlagCalculatorBase {
  override def calcN(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=true
}
trait FlagNReset extends FlagCalculatorBase {
  override def calcN(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=false
}
trait FlagCCarry extends FlagCalculatorBase {
  override def calcC(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=res.valueUnsigned() > res.valueOut
}
trait FlagCBorrow extends FlagCalculatorBase {
  override def calcC(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=res.valueUnsigned() < res.valueOut
}
trait FlagCReset extends FlagCalculatorBase {
  override def calcC(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=false
}
trait FlagCSet extends FlagCalculatorBase {
  override def calcC(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean=true
}
trait FlagCInvert extends FlagCalculatorBase {
  override def calcC(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean= !input.flags(Flag.C)
}
trait FlagCBit7 extends FlagCalculatorBase {
  override def calcC(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean= Z80Utils.getBit(input.value,7)
}
trait FlagCBit0 extends FlagCalculatorBase {
  override def calcC(res:ArithmeticOpResult,input:ArithmeticOpInput):Boolean= Z80Utils.getBit(input.value,0)
}

