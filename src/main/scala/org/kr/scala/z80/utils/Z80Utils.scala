package org.kr.scala.z80.utils

object Z80Utils {
  def add8bit(val1: Int, val2: Int): Int = (val1 + val2) % 0x100

  def add16bit(val1: Int, val2: Int): Int = (val1 + val2) % 0x10000

  def flattenMapOfLists[A, B](mapOfLists: Map[List[A], B]): Map[A, B] =
    mapOfLists.map(entry => entry._1.flatMap(opcode => Map(opcode -> entry._2))).flatten.toMap

  def getH(word: Int): Int = (word >> 8) & 0xFF

  def getL(word: Int): Int = word & 0xFF

  def makeWord(valH:Int,valL:Int):Int=valH*0x100+valL

  def rawByteTo2Compl(raw:Int):Int= (raw & 0x7F)-((raw>>7)&1)*0x80

  def rawWordTo2Compl(raw:Int):Int= (raw & 0x7FFF)-((raw>>15)&1)*0x8000

  def getBitFromString(stringAsBoolean:String, bit:Int):Boolean = stringAsBoolean.substring(7-bit,7-bit+1)=="1"
  def getBit(value:Int,bit:Int):Boolean=(value & (1 << bit))>0

  def countBits(byte:Int,bitNum:Int=8):Int = List.range(0,bitNum).foldLeft(0)((bits,bit)=>bits + (if((byte & (1 << bit))>0) 1 else 0))
  def isEven(value:Int):Boolean=(value & 1)==0
  def isEvenBits(byte:Int):Boolean=isEven(countBits(byte))
}
