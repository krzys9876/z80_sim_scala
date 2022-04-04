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

  def getBitFromString(stringAsBoolean:String, bit:Int):Boolean = stringAsBoolean.substring(7-bit,7-bit+1)=="1"
}
