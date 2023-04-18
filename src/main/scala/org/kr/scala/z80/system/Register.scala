package org.kr.scala.z80.system

import org.kr.scala.z80.utils.Z80Utils

trait RegisterBase {
  def apply(regSymbolObj:RegSymbol):Int
  def apply(flag:FlagSymbol):Boolean
  def set(regSymbolObj:RegSymbol,value:Int): RegisterBase
  def setRelative(regSymbol:RegSymbol,relativeValue:Int): RegisterBase
}


class ImmutableRegister(val a:Int, val f:Int, val b:Int, val c:Int, val d:Int, val e:Int, val h:Int, val l:Int,
                        val pc:Int, val sp:Int, val r:Int, val i:Int, val ix:Int, val iy:Int,
                        val af1:Int, val bc1:Int, val de1:Int, val hl1:Int,
                        val iff:Int, val im:Int) extends RegisterBase {
  def apply(regSymbolObj:RegSymbol):Int= {
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
      case Regs.IFF => iff
      case Regs.IM => im
      case Regs.NONE => throw new UnknownRegisterException("NONE register cannot be set")
    }
  }

  def apply(flag:FlagSymbol):Boolean=flag.extract(f)

  def set(regSymbolObj:RegSymbol,value:Int): ImmutableRegister= {
    regSymbolObj match {
      case Regs.A => new ImmutableRegister(value,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.F => new ImmutableRegister(a,value,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.AF => new ImmutableRegister(Z80Utils.getH(value),Z80Utils.getL(value),b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.B => new ImmutableRegister(a,f,value,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.C => new ImmutableRegister(a,f,b,value,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.BC => new ImmutableRegister(a,f,Z80Utils.getH(value),Z80Utils.getL(value),d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.D => new ImmutableRegister(a,f,b,c,value,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.E => new ImmutableRegister(a,f,b,c,d,value,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.DE => new ImmutableRegister(a,f,b,c,Z80Utils.getH(value),Z80Utils.getL(value),h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.H => new ImmutableRegister(a,f,b,c,d,e,value,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.L => new ImmutableRegister(a,f,b,c,d,e,h,value,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.HL => new ImmutableRegister(a,f,b,c,d,e,Z80Utils.getH(value),Z80Utils.getL(value),pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.PC => new ImmutableRegister(a,f,b,c,d,e,h,l,value,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.SP => new ImmutableRegister(a,f,b,c,d,e,h,l,pc,value,r,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.R => new ImmutableRegister(a,f,b,c,d,e,h,l,pc,sp,value,i,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.I => new ImmutableRegister(a,f,b,c,d,e,h,l,pc,sp,r,value,ix,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.IX => new ImmutableRegister(a,f,b,c,d,e,h,l,pc,sp,r,i,value,iy,af1,bc1,de1,hl1,iff,im)
      case Regs.IY => new ImmutableRegister(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,value,af1,bc1,de1,hl1,iff,im)
      case Regs.AF1 => new ImmutableRegister(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,value,bc1,de1,hl1,iff,im)
      case Regs.BC1 => new ImmutableRegister(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,value,de1,hl1,iff,im)
      case Regs.DE1 => new ImmutableRegister(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,value,hl1,iff,im)
      case Regs.HL1 => new ImmutableRegister(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,value,iff,im)
      case Regs.IFF => new ImmutableRegister(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,value,im)
      case Regs.IM => new ImmutableRegister(a,f,b,c,d,e,h,l,pc,sp,r,i,ix,iy,af1,bc1,de1,hl1,iff,value)
      case Regs.NONE => this
    }
  }

  def setRelative(regSymbol:RegSymbol,relativeValue:Int): ImmutableRegister=
    set(regSymbol,Z80Utils.add16bit(apply(regSymbol),relativeValue))

  override def toString:String=
    f"A:$a%02X|F:$f%02X|B:$b%02X|C:$c%02X|D:$d%02X|E:$e%02X|H:$h%02X|L:$l%02X|"+
      f"PC:$pc%04X|SP:$sp%02X|R:$r%02X|I:$i%02X|IX:$ix%02X|IY:$iy%02X|"+
        f"AF1:$af1%04X|BC1:$bc1%04X|DE1:$de1%04X|HL1:$hl1%04X||IFF:$iff|IM:$im"
}


trait RegisterHandler {
  def blank:RegisterBase

  // functions changing state (Register=>Register)
  def set: (RegSymbol, Int) => RegisterBase => RegisterBase = (regSymbol, value) => register => register.set(regSymbol, value)
  def relative: (RegSymbol, Int) => RegisterBase => RegisterBase = (regSymbol, value) => register => register.setRelative(regSymbol, value)
}

class ImmutableRegisterHandler extends RegisterHandler {
  override def blank:ImmutableRegister=new ImmutableRegister(0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0,0xFFFF,0,0,0xFFFF,0xFFFF,
    0xFF,0xFF,0xFF,0xFF,0,0)
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
  case object IFF extends RegSymbol("IFF")
  case object IM extends RegSymbol("IM")
  case object NONE extends RegSymbol("NONE")
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
  def set(flagSymbol:FlagSymbol,flag:Boolean):Flag=setFlag(flagSymbol,flag)
  def set(flagSymbol:FlagSymbol):Flag=setFlag(flagSymbol,flag=true)
  def reset(flagSymbol:FlagSymbol):Flag=setFlag(flagSymbol,flag=false)

  private def setFlag(flagSymbol:FlagSymbol,flag:Boolean):Flag=
    new Flag(Z80Utils.setOrResetBit(value,flagSymbol.bit,flag))
}

object Flag {
  case object S extends FlagSymbol("S",7)
  case object Z extends FlagSymbol("Z",6)
  case object H extends FlagSymbol("H",4)
  case object P extends FlagSymbol("P",2)
  case object N extends FlagSymbol("N",1)
  case object C extends FlagSymbol("C",0)
  case object None extends FlagSymbol("",0)

  def of(s:Boolean, z:Boolean, h:Boolean, p:Boolean, n:Boolean, c:Boolean):Flag=
    new Flag(
      (if(s) 1 << 7 else 0) |
      (if(z) 1 << 6 else 0) |
      (if(h) 1 << 4 else 0) |
      (if(p) 1 << 2 else 0) |
      (if(n) 1 << 1 else 0) |
      (if(c) 1 << 0 else 0)
    )
}

class UnknownRegisterException(message : String) extends Exception(message)