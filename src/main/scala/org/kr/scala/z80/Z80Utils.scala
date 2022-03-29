package org.kr.scala.z80

object Z80Utils {
  def add8bit(val1:Int,val2:Int):Int=
    (val1+val2) % 0x100
  def add16bit(val1:Int,val2:Int):Int=
    (val1+val2) % 0x10000
}
