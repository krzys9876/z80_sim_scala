package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{RegSymbol, Regs}
import org.kr.scala.z80.utils.{AnyInt, IntValue, OptionInt}

case class Location(reg:RegSymbol, immediate:OptionInt, offsetPC:OptionInt, addressReg:RegSymbol,
                    directOffset:OptionInt, indirectOffset2Compl:OptionInt, isWord:Boolean=false) {
  override lazy val toString: String =
    this match {
      case Location(r,_,_,_,_,_,_) if r!=Regs.NONE => f"$r$isWordString"
      case Location(_,i,_,_,_,_,_) if i!=AnyInt => f"$i$isWordString"
      case Location(_,_,pco,_,_,_,_) if pco!=AnyInt => f"(PC+0x${pco()}%02X)$isWordString"
      case Location(_,_,_,r,dirO,indirO,_) if r!=Regs.NONE =>
        (dirO, indirO) match {
          case (AnyInt,AnyInt) => f"($r)$isWordString"
          case (o,AnyInt) => f"($r+0x${o()}%02X)$isWordString"
          case (AnyInt,o) => f"($r+(PC+0x${o()}%02X))$isWordString"
        }
      case _ => "empty"
    }

  lazy val label: String =
    this match {
      case Location(r,_,_,_,_,_,_) if r!=Regs.NONE => f"$r"
      case Location(_,i,_,_,_,_,_) if i!=AnyInt => f"$i$isWordString"
      case Location(_,_,pco,_,_,_,_) if pco!=AnyInt => f"(PC+0x${pco()}%02X)$isWordString"
      case Location(_,_,_,r,dirO,indirO,_) if r!=Regs.NONE =>
        (dirO, indirO) match {
          case (AnyInt,AnyInt) => f"($r)"
          case (o,AnyInt) => f"($r+0x${o()}%02X)$isWordString"
          case (AnyInt,_) => f"($r+d)"
        }
      case _ => "empty"
    }

  private val isWordString=if(isWord) "w" else "b"
}

object Location {
  // register
  def register(r:RegSymbol,isWord:Boolean=false):Location=Location(r,AnyInt,AnyInt,Regs.NONE,AnyInt,AnyInt,isWord)
  // immediate value - fixed for the opcode (e.g. RST)
  def immediate(i:Int,isWord:Boolean=false):Location=Location(Regs.NONE,IntValue(i),AnyInt,Regs.NONE,AnyInt,AnyInt,isWord)
  // a value in a memory address located after OpCode,
  // eg. LD (nn),HL - nn=address of a memory location where contents of HL is loaded
  def indirAddress(a:Int,isWord:Boolean=false):Location=Location(Regs.NONE,AnyInt,IntValue(a),Regs.NONE,AnyInt,AnyInt,isWord)
  // a value in a memory address located in 16-bit register or a register pair
  def registerAddr(r:RegSymbol,isWord:Boolean=false):Location=Location(Regs.NONE,AnyInt,AnyInt,r,AnyInt,AnyInt,isWord)
  // same as registerAddr but with directly specified address offset, e.g. PC+1
  def registerAddrDirOffset(r:RegSymbol,o:Int,isWord:Boolean=false):Location=Location(Regs.NONE,AnyInt,AnyInt,r,IntValue(o),AnyInt,isWord)
  // same as registerAddr but with address offset located in memory address PC+x,
  // e.g. (IX+d), where d is a value from memory address after OpCode (PC+x)
  def registerAddrIndirOffset(r:RegSymbol,o:Int,isWord:Boolean=false):Location=Location(Regs.NONE,AnyInt,AnyInt,r,AnyInt,IntValue(o),isWord)

  def empty:Location=Location(Regs.NONE,AnyInt,AnyInt,Regs.NONE,AnyInt,AnyInt)
}

class IncorrectLocation(message : String) extends Exception(message) {}
