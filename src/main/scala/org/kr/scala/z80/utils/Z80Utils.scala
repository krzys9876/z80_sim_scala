package org.kr.scala.z80.utils

object Z80Utils {
  def add8bit(val1: Int, val2: Int): Int = byteEnsureRange((val1 + val2) % 0x100)
  def add16bit(val1: Int, val2: Int): Int = wordEnsureRange((val1 + val2) % 0x10000)

  def getH(word: Int): Int = (word >> 8) & 0xFF

  def getL(word: Int): Int = word & 0xFF

  def makeWord(valH:Int,valL:Int):Int=valH*0x100+valL

  private def wordEnsureRange(raw:Int):Int=if(raw<0) raw & 0xFFFF else raw
  private def byteEnsureRange(raw:Int):Int=if(raw<0) raw & 0xFF else raw

  def rawByteTo2Compl(raw:Int):Int= (raw & 0x7F)-((raw>>7)&1)*0x80
  def rawWordTo2Compl(raw:Int):Int= (raw & 0x7FFF)-((raw>>15)&1)*0x8000
  def word2ComplToRaw(compl:Int):Int= wordEnsureRange(compl)

  def getBitFromString(stringAsBoolean:String, bit:Int):Boolean = stringAsBoolean.substring(7-bit,7-bit+1)=="1"
  def getBit(value:Int,bit:Int):Boolean=(value & (1 << bit))>0
  def getBitValue(value:Int,bit:Int):Int=if(getBit(value,bit)) 1 else 0
  def setBit(value:Int,bit:Int):Int=value | (1 << bit)
  def resetBit(value:Int,bit:Int):Int=value & (~(1 << bit))
  def setOrResetBit(value:Int,bit:Int,bitVal:Boolean):Int=if(bitVal) setBit(value,bit) else resetBit(value,bit)

  def countBits(byte:Int,bitNum:Int=8):Int = List.range(0,bitNum).foldLeft(0)((bits,bit)=>bits + (if((byte & (1 << bit))>0) 1 else 0))
  def isEven(value:Int):Boolean=(value & 1)==0
  def isEvenBits(byte:Int):Boolean=isEven(countBits(byte))

  def isNegative(raw:Int):Boolean=Z80Utils.rawByteTo2Compl(raw)<0
}
