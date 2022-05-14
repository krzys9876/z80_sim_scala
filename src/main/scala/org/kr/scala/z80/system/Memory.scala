package org.kr.scala.z80.system

import org.kr.scala.z80.utils.Z80Utils

import scala.annotation.tailrec

class Memory(val mem: Vector[Int], val size:Int, val lock:Int=0) {
  def apply(address:Int):Int=mem(address)
  def apply(address:Int,offset:Int):Int=mem(Z80Utils.add16bit(address,offset))
  def poke(address:Int, value:Int):Memory=
    if(address>=lock) replaceSingle(address,value)
    else this
  def pokeMulti(address:Int, values:Vector[Int]):Memory= {
    if(values.isEmpty || address+values.size<lock) this
    else if(address>=lock) replaceMulti(address,values)
    else replaceMulti(lock,values.slice(lock-address,values.size))
  }
  def loadHexLine(loader:HexLineLoader):Memory=pokeMulti(loader.address,loader.values)
  def loadHexLine(line:String):Memory= loadHexLine(new HexLineLoader(line))
  def loadHexLines(lines:List[String]):Memory= lines.foldLeft(this)((mem,line)=>mem.loadHexLine(line))
  def lockTo(upperAddressExcl: Int): Memory = new Memory(mem,size,upperAddressExcl)

  private def replaceSingle(address:Int,value:Int):Memory=
    new Memory((mem.slice(0,address) :+ value) ++ mem.slice(address+1,size),size,lock)
  private def replaceMulti(address:Int,values:Vector[Int]):Memory=
    new Memory(mem.slice(0,address) ++ values ++ mem.slice(address+values.size,size),size,lock)

}

object Memory {
  def blank(size:Int):Memory=new Memory(fillZero(Vector(),size),size)
  def preloaded(initial:Vector[Int],size:Int):Memory= {
    new Memory(initial++fillZero(Vector(),size-initial.size),size)
  }

  @tailrec
  def fillZero(memory: Vector[Int], noLeft: Int): Vector[Int] =
    noLeft match {
      case 0 => memory
      case _ => fillZero(memory :+ 0.toShort,noLeft-1)
    }

  // functors - functions changing state (Memory=>Memory)
  def poke: (Int, Int) => Memory => Memory = (address, value) => memory => memory.poke(address, value)
  def pokeMulti: (Int, Vector[Int]) => Memory => Memory = (address, values) => memory => memory.pokeMulti(address, values)
  def loadHexLine:String => Memory => Memory = line => memory => memory.loadHexLine(line)
  def loadHexLines:List[String] => Memory => Memory = lines => memory => memory.loadHexLines(lines)
  def lockTo: Int => Memory => Memory = upperAddressExcl => memory => memory.lockTo(upperAddressExcl)
}