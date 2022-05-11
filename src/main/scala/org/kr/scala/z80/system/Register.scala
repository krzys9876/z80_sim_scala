package org.kr.scala.z80.system

import org.kr.scala.z80.opcode.OpCode
import org.kr.scala.z80.utils.Z80Utils

class Register(val a:Int,val f:Int,val b:Int,val c:Int,val d:Int,val e:Int,val h:Int,val l:Int,
               val pc:Int,val sp:Int,val r:Int,val i:Int,val ix:Int,val iy:Int,
               val af1:Int,val bc1:Int,val de1:Int,val hl1:Int) {
  def apply(regSymbolStr:String):Int= {
    val regSymbolObj = Regs.fromString(regSymbolStr)
    regSymbolObj match {
      case Regs.A => a
      case Regs.F => f
      case Regs.AF => Z80Utils.makeWord(a,f)
      case Regs.B => b
      case Regs.C => c
      case Regs.BC => Z80Utils.makeWord(b,c)
      case Regs.D => d
      case Regs.E => e
      case Regs.DE => Z80Utils.makeWord(d,e)
      case Regs.H => h
      case Regs.L => l
      case Regs.HL => Z80Utils.makeWord(h,l)
      case Regs.PC => pc
      case Regs.SP => sp
      case Regs.R => r
      case Regs.I => i
      case Regs.IX => ix
      case Regs.IY => iy
      case Regs.AF1 => af1
      case Regs.BC1 => bc1
      case Regs.DE1 => de1
      case Regs.HL1 => hl1
    }
  }

  def apply(flag:FlagSymbol):Boolean=flag.extract(f)

  def set(regSymbolStr:String,value:Int): Register= {
    val regSymbolObj=Regs.fromString(regSymbolStr)
    regSymbolObj match {
      case Regs.A => new Register(value,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.F => new Register(a,value,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.AF => new Register(Z80Utils.getH(value),Z80Utils.getL(value),b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.B => new Register(a,f,value,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.C => new Register(a,f,b,value,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.BC => new Register(a,f,Z80Utils.getH(value),Z80Utils.getL(value),d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.D => new Register(a,f,b,c,value,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.E => new Register(a,f,b,c,d,value,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.DE => new Register(a,f,b,c,Z80Utils.getH(value),Z80Utils.getL(value),h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.H => new Register(a,f,b,c,d,e,value,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.L => new Register(a,f,b,c,d,e,h,value,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.HL => new Register(a,f,b,c,d,e,Z80Utils.getH(value),Z80Utils.getL(value),pc,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.PC => new Register(a,f,b,c,d,e,h,l,value,sp,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.SP => new Register(a,f,b,c,d,e,h,l,pc,value,r,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.R => new Register(a,f,b,c,d,e,h,l,pc,sp,value,i,ix,iy,af1,bc1,de1,hl1)
      case Regs.I => new Register(a,f,b,c,d,e,h,l,pc,sp,r,value,ix,iy,af1,bc1,de1,hl1)
      case Regs.IX => new Register(a,f,b,c,d,e,h,l,pc,sp,r,i,value,iy,af1,bc1,de1,hl1)
      case Regs.IY => new Register(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,value,af1,bc1,de1,hl1)
      case Regs.AF1 => new Register(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,value,bc1,de1,hl1)
      case Regs.BC1 => new Register(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,value,de1,hl1)
      case Regs.DE1 => new Register(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,value,hl1)
      case Regs.HL1 => new Register(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,value)
    }
  }

  def setRelative(regSymbol:String,relativeValue:Int): Register=
    set(regSymbol,Z80Utils.add16bit(apply(regSymbol),relativeValue))
  def movePC(forward:Int): Register=
    setRelative("PC",forward)

  override def toString:String=
    f"A:$a%02X|F:$f%02X|B:$b%02X|C:$c%02X|D:$d%02X|E:$e%02X|H:$h%02X|L:$l%02X|"+
      f"PC:$pc%04X|SP:$sp%02X|R:$r%02X|I:$i%02X|IX:$ix%02X|IY:$iy%02X|"+
        f"AF1:$af1%04X|BC1:$bc1%04X|DE1:$de1%04X|HL1:$hl1%04X"
}

object Register {
  def blank:Register=new Register(0,0xFF,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  def apply(register: Register):Register=new Register(register.a,register.f,register.b,register.c,
    register.d,register.e,register.h,register.l,register.pc,register.sp,register.r,register.i,register.ix,register.iy,
    register.af1,register.bc1,register.de1,register.hl1)
}

sealed abstract class RegSymbol(val symbol:String) {
  override val toString:String=f"$symbol"
}

object Regs {
  case object A extends RegSymbol("A")
  case object B extends RegSymbol("B")
  case object C extends RegSymbol("C")
  case object D extends RegSymbol("D")
  case object E extends RegSymbol("E")
  case object H extends RegSymbol("H")
  case object L extends RegSymbol("L")
  case object F extends RegSymbol("F")
  case object AF extends RegSymbol("AF")
  case object BC extends RegSymbol("BC")
  case object DE extends RegSymbol("DE")
  case object HL extends RegSymbol("HL")
  case object PC extends RegSymbol("PC")
  case object SP extends RegSymbol("SP")
  case object R extends RegSymbol("R")
  case object I extends RegSymbol("I")
  case object IX extends RegSymbol("IX")
  case object IY extends RegSymbol("IY")
  case object AF1 extends RegSymbol("AF1")
  case object BC1 extends RegSymbol("BC1")
  case object DE1 extends RegSymbol("DE1")
  case object HL1 extends RegSymbol("HL1")

  val fromString:String=>RegSymbol={
    case "A" => A
    case "B" => B
    case "C" => C
    case "D" => D
    case "E" => E
    case "H" => H
    case "L" => L
    case "F" => F
    case "AF" => AF
    case "BC" => BC
    case "DE" => DE
    case "HL" => HL
    case "PC" => PC
    case "SP" => SP
    case "R" => R
    case "I" => I
    case "IX" => IX
    case "IY" => IY
    case "AF1" => AF1
    case "BC1" => BC1
    case "DE1" => DE1
    case "HL1" => HL1
  }
}

sealed abstract class FlagSymbol(val symbol:String, val bit:Int) {
  val extract:Int=>Boolean= flagValue=>((flagValue >> bit) & 1)==1

  override val toString:String=if(symbol.nonEmpty) symbol else "-"
}

class Flag(val value:Int) {
  def apply(newValue:Int):Flag=new Flag(newValue)
  def apply():Int=value
  def apply(symbol:FlagSymbol):Boolean=Z80Utils.getBit(value,symbol.bit)
  def flagValue(symbol:FlagSymbol):Int=if(Z80Utils.getBit(value,symbol.bit)) 1 else 0
  def set(flagSymbol:FlagSymbol,flag:Boolean):Flag=new Flag(Flag.setFlag(value,flagSymbol,flag))
  def set(flagSymbol:FlagSymbol):Flag=new Flag(Flag.setFlag(value,flagSymbol,flag=true))
  def reset(flagSymbol:FlagSymbol):Flag=new Flag(Flag.setFlag(value,flagSymbol,flag=false))
}

object Flag {
  case object S extends FlagSymbol("S",7)
  case object Z extends FlagSymbol("Z",6)
  case object H extends FlagSymbol("H",4)
  case object P extends FlagSymbol("P",2)
  case object N extends FlagSymbol("N",1)
  case object C extends FlagSymbol("C",0)
  case object None extends FlagSymbol("",OpCode.ANY)

  def set(s:Boolean,z:Boolean,h:Boolean,p:Boolean,n:Boolean,c:Boolean):Int={
    (if(s) 1 << 7 else 0) |
    (if(z) 1 << 6 else 0) |
    (if(h) 1 << 4 else 0) |
    (if(p) 1 << 2 else 0) |
    (if(n) 1 << 1 else 0) |
    (if(c) 1 << 0 else 0)
  }

  def setFlag(prevValue:Int,flagSymbol:FlagSymbol,flag:Boolean):Int={
    Z80Utils.setOrResetBit(prevValue,flagSymbol.bit,flag)
  }
}
