package org.kr.scala.z80

import scala.annotation.tailrec

class Memory(val mem: Vector[Int], val size:Int) {
  def apply(address:Int):Int=mem(address)
  def apply(address:Int,offset:Int):Int=mem(Z80Utils.add16bit(address,offset))
  def replaceAt(address:Int,value:Int):Memory=
    new Memory((mem.slice(0,address) :+ value) ++ mem.slice(address+1,size),size)
  def replaceAt(address:Int,values:Vector[Int]):Memory=
    new Memory(mem.slice(0,address) ++ values ++ mem.slice(address+values.size,size),size)
}

object Memory {
  def blank(size:Int):Memory=new Memory(fillZero(Vector[Int](),size),size)
  def preloaded(initial:Vector[Int],size:Int):Memory= {
    new Memory(initial++fillZero(Vector[Int](),size-initial.size),size)
  }

  @tailrec
  def fillZero(memory: Vector[Int], noLeft: Int): Vector[Int] =
    noLeft match {
      case 0 => memory
      case _ => fillZero(memory :+ 0.toShort,noLeft-1)
    }

  def apply(memory: Memory):Memory=new Memory(memory.mem,memory.size)
}