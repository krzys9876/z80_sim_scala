package org.kr.scala.z80.system

import org.kr.scala.z80.utils.Z80Utils

import scala.annotation.tailrec

class ImmutableMemory(override val copy: Vector[Int], val size:Int, val lock:Int=0) extends MemoryContents {
  override def apply(address:Int):Int=copy(address)
  override def apply(address:Int,offset:Int):Int=copy(Z80Utils.add16bit(address,offset))

  override def poke(address:Int, value:Int):ImmutableMemory= if(address>=lock) replaceSingle(address,value) else this
  override def pokeW(address:Int, value:Int):ImmutableMemory= pokeMulti(address,Vector(Z80Utils.getL(value),Z80Utils.getH(value)))
  override def pokeMulti(address:Int, values:Vector[Int]):ImmutableMemory= {
    if(values.isEmpty || address+values.size<lock) this
    else {
      val startAddress = unlockedAddress(address)
      val (endAddressExcl, overflow:Boolean) = addressInRangeOrOverflow(address+values.size)
      val startElement = startAddress - address
      val endElementExcl = startElement + endAddressExcl - startAddress
      val valuesToReplace = sliceInRange(values,startElement,endElementExcl)

      val newMem=replaceMulti(startAddress, valuesToReplace)
      if(overflow) newMem.pokeMulti(0,values.slice(endElementExcl,values.size))
      else newMem
    }
  }
  private def unlockedAddress(address:Int):Int=if (address < lock) lock else address
  private def addressInRangeOrOverflow(address:Int):(Int,Boolean)=
    if (address > size) (size, true) else (address, false)
  private def sliceInRange(values:Vector[Int], start:Int, endExcl:Int):Vector[Int]=
    if(start==0 && endExcl==values.size) values
    else values.slice(start, endExcl)

  override def loadHexLines(lines:List[String]):ImmutableMemory= lines.foldLeft(this)((mem, line)=>mem.loadHexLine(line))
  private def loadHexLine(line:String):ImmutableMemory= loadHexLine(new HexLineLoader(line))
  private def loadHexLine(loader:HexLineLoader):ImmutableMemory=pokeMulti(loader.address,loader.values)
  override def lockTo(upperAddressExcl: Int): ImmutableMemory = new ImmutableMemory(copy,size,upperAddressExcl)

  private def replaceSingle(address:Int,value:Int):ImmutableMemory=
    new ImmutableMemory((copy.slice(0,address) :+ value) ++ copy.slice(address+1,size),size,lock)
  private def replaceMulti(address:Int,values:Vector[Int]):ImmutableMemory=
    new ImmutableMemory(copy.slice(0,address) ++ values ++ copy.slice(address+values.size,size),size,lock)
}

object ImmutableMemory extends MemoryHandler {
  override def blank(size:Int):MemoryContents=new ImmutableMemory(Vector.fill(size)(0),size)
  def preloaded(initial:Vector[Int],size:Int):MemoryContents= new ImmutableMemory(initial++Vector.fill(size-initial.size)(0),size)
  // functions changing state (Memory=>Memory)
  override def poke: (Int, Int) => MemoryContents => MemoryContents = (address, value) => memory => memory.poke(address, value)
  override def pokeW: (Int, Int) => MemoryContents => MemoryContents = (address, value) => memory => memory.pokeW(address, value)
  override def pokeMulti: (Int, Vector[Int]) => MemoryContents => MemoryContents = (address, values) => memory => memory.pokeMulti(address, values)
  override def loadHexLines: List[String] => MemoryContents => MemoryContents = lines => memory => memory.loadHexLines(lines)
  override def lockTo: Int => MemoryContents => MemoryContents = upperAddressExcl => memory => memory.lockTo(upperAddressExcl)
}

class MutableMemory(val initial: Vector[Int], val size:Int, var lock:Int=0) extends MemoryContents {
  private val data: MemoryArray = new MemoryArray(size,initial.toArray)
  override def apply(address:Int):Int=data.data(address)
  override def apply(address:Int,offset:Int):Int=data.data(Z80Utils.add16bit(address,offset))

  override def poke(address:Int, value:Int):MutableMemory= if(address>=lock) replaceSingle(address,value) else this
  override def pokeW(address:Int, value:Int):MutableMemory= pokeMulti(address,Vector(Z80Utils.getL(value),Z80Utils.getH(value)))
  override def pokeMulti(address:Int, values:Vector[Int]):MutableMemory= {
    if(values.isEmpty || address+values.size<lock) this
    else {
      val startAddress = unlockedAddress(address)
      val (endAddressExcl, overflow:Boolean) = addressInRangeOrOverflow(address+values.size)
      val startElement = startAddress - address
      val endElementExcl = startElement + endAddressExcl - startAddress
      val valuesToReplace = sliceInRange(values,startElement,endElementExcl)

      val newMem=replaceMulti(startAddress, valuesToReplace)
      if(overflow) newMem.pokeMulti(0,values.slice(endElementExcl,values.size))
      else newMem
    }
  }
  private def unlockedAddress(address:Int):Int=if (address < lock) lock else address
  private def addressInRangeOrOverflow(address:Int):(Int,Boolean)=
    if (address > size) (size, true) else (address, false)
  private def sliceInRange(values:Vector[Int], start:Int, endExcl:Int):Vector[Int]=
    if(start==0 && endExcl==values.size) values
    else values.slice(start, endExcl)

  override def loadHexLines(lines: List[String]): MutableMemory = lines.foldLeft(this)((mem, line) => mem.loadHexLine(line))
  private def loadHexLine(line:String):MutableMemory= loadHexLine(new HexLineLoader(line))
  private def loadHexLine(loader:HexLineLoader):MutableMemory=pokeMulti(loader.address,loader.values)
  override def lockTo(upperAddressExcl: Int): MutableMemory = {
    lock=upperAddressExcl
    this
  }

  private def replaceSingle(address:Int,value:Int):MutableMemory={
    data.data(address)=value
    this
  }
  private def replaceMulti(address:Int,values:Vector[Int]):MutableMemory= {
    for(addr<-address until address+values.size) data.data(addr)=values(addr-address)
    this
  }

  override def copy: Vector[Int] = data.data.toVector
}

object MutableMemory extends MemoryHandler {
  override def blank(size:Int):MemoryContents=new MutableMemory(Vector.fill(size)(0),size)
  def preloaded(initial:Vector[Int],size:Int):MemoryContents= new MutableMemory(initial++Vector.fill(size-initial.size)(0),size)
  // functions changing state (Memory=>Memory)
  override def poke: (Int, Int) => MemoryContents => MemoryContents = (address, value) => memory => memory.poke(address, value)
  override def pokeW: (Int, Int) => MemoryContents => MemoryContents = (address, value) => memory => memory.pokeW(address, value)
  override def pokeMulti: (Int, Vector[Int]) => MemoryContents => MemoryContents = (address, values) => memory => memory.pokeMulti(address, values)
  override def loadHexLines: List[String] => MemoryContents => MemoryContents = lines => memory => memory.loadHexLines(lines)
  override def lockTo: Int => MemoryContents => MemoryContents = upperAddressExcl => memory => memory.lockTo(upperAddressExcl)
}

trait MemoryHandler {
  def blank(size: Int): MemoryContents
  def preloaded(initial: Vector[Int], size: Int): MemoryContents
  // functions changing state (Memory=>Memory)
  def poke: (Int, Int) => MemoryContents => MemoryContents
  def pokeW: (Int, Int) => MemoryContents => MemoryContents
  def pokeMulti: (Int, Vector[Int]) => MemoryContents => MemoryContents
  def loadHexLines: List[String] => MemoryContents => MemoryContents
  def lockTo: Int => MemoryContents => MemoryContents
}

trait MemoryContents {
  def apply(address: Int): Int
  def apply(address: Int, offset: Int): Int
  def poke(address:Int, value:Int):MemoryContents
  def pokeW(address:Int, value:Int):MemoryContents
  def pokeMulti(address:Int, values:Vector[Int]):MemoryContents
  def loadHexLines(lines:List[String]):MemoryContents
  def lockTo(upperAddressExcl: Int): MemoryContents
  def copy:Vector[Int]
}