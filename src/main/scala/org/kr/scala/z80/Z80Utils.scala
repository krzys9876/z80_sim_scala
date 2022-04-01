package org.kr.scala.z80

object Z80Utils {
  def add8bit(val1:Int,val2:Int):Int= (val1+val2) % 0x100
  def add16bit(val1:Int,val2:Int):Int= (val1+val2) % 0x10000
  def flattenMapOfLists[A,B](mapOfLists:Map[List[A],B]):Map[A,B]=
    mapOfLists.map(entry=>entry._1.flatMap(opcode=>Map(opcode->entry._2))).flatten.toMap
  def getH(word:Int):Int=(word >> 8) & 0xFF
  def getL(word:Int):Int=word & 0xFF
}
