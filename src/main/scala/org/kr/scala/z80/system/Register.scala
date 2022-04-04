package org.kr.scala.z80.system

import org.kr.scala.z80.utils.Z80Utils

class Register(val reg:Map[String,Int]) {
  def apply(regSymbol:String):Int=
    regSymbol match {
      case "AF" | "BC" | "DE" | "HL" =>
        Z80Utils.makeWord(getRaw(regSymbol.substring(0, 1)), getRaw(regSymbol.substring(1, 2)))
      case _ => getRaw(regSymbol)
    }

  def apply(flag:FlagSymbol):Boolean=flag.extract(getRaw("F"))

  private def getRaw(regSymbol:String):Int=reg.getOrElse(regSymbol, 0)

  def set(regSymbol:String,value:Int): Register= {
    val newRegMap=regSymbol match {
      case "AF" | "BC" | "DE" | "HL" =>
        Map(regSymbol.substring(0,1)->Z80Utils.getH(value),
          regSymbol.substring(1,2)->Z80Utils.getL(value))
      case _ => Map(regSymbol->value)
    }
    new Register(reg++newRegMap)
  }

  def setRelative(regSymbol:String,relativeValue:Int): Register=
    set(regSymbol,Z80Utils.add16bit(apply(regSymbol),relativeValue))
  def movePC(forward:Int): Register=
    setRelative("PC",forward)
}

object Register {
  def blank:Register=new Register(Map())
  def apply(register: Register):Register=new Register(register.reg)
}

sealed abstract class FlagSymbol(val symbol:String, val bit:Int) {
  val extract:Int=>Boolean= flagValue=>((flagValue >> bit) & 1)==1
}

object Flag {
  case object S extends FlagSymbol("S",7)
  case object Z extends FlagSymbol("Z",6)
  case object H extends FlagSymbol("H",4)
  case object P extends FlagSymbol("P",2)
  case object N extends FlagSymbol("N",1)
  case object C extends FlagSymbol("C",0)

  def set(s:Boolean,z:Boolean,h:Boolean,p:Boolean,n:Boolean,c:Boolean):Int={
    (if(s) 1 << 7 else 0) |
    (if(z) 1 << 6 else 0) |
    (if(h) 1 << 4 else 0) |
    (if(p) 1 << 2 else 0) |
    (if(n) 1 << 1 else 0) |
    (if(c) 1 << 0 else 0)
  }
}