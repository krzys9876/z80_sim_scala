package org.kr.scala.z80.system

import org.kr.scala.z80.utils.Z80Utils

import scala.annotation.tailrec

class ImmutableMemory(override val copy: Vector[Int], val size:Int, override val lock: AddressRange=AddressRange.empty) extends MemoryContents {
  override def apply(address:Int):Int=copy(address)
  override def apply(address:Int,offset:Int):Int=copy(Z80Utils.add16bit(address,offset))
  override def lock(range: AddressRange): ImmutableMemory = new ImmutableMemory(copy,size,range)
  override def replaceSingle(address:Int,value:Int):ImmutableMemory=
    new ImmutableMemory((copy.slice(0,address) :+ value) ++ copy.slice(address+1,size),size,lock)
  override def replaceMulti(address:Int,values:Vector[Int]):ImmutableMemory=
    new ImmutableMemory(copy.slice(0,address) ++ values ++ copy.slice(address+values.size,size),size,lock)
}

class MutableMemory(val initial: Vector[Int], val size:Int, var lockRange: AddressRange=AddressRange.empty) extends MemoryContents {
  override def lock:AddressRange=lockRange
  private val data: MemoryArray = new MemoryArray(size,initial.toArray)
  override def apply(address:Int):Int=data.data(address)
  override def apply(address:Int,offset:Int):Int=data.data(Z80Utils.add16bit(address,offset))
  override def copy: Vector[Int] = data.data.toVector
  override def lock(newLockRange:AddressRange): MutableMemory = {
    lockRange=newLockRange
    this
  }
  override def replaceSingle(address:Int,value:Int):MutableMemory={
    data.data(address)=value
    this
  }
  override def replaceMulti(address:Int,values:Vector[Int]):MutableMemory= {
    for(addr<-address until address+values.size) data.data(addr)=values(addr-address)
    this
  }
}

class ImmutableMemoryHandler extends MemoryHandler {
  override def blank(size:Int):ImmutableMemory=new ImmutableMemory(Vector.fill(size)(0),size)
  override def preloaded(initial:Vector[Int],size:Int):ImmutableMemory= new ImmutableMemory(initial++Vector.fill(size-initial.size)(0),size)
}

class MutableMemoryHandler extends MemoryHandler {
  override def blank(size:Int):MutableMemory=new MutableMemory(Vector.fill(size)(0),size)
  override def preloaded(initial:Vector[Int],size:Int):MutableMemory= new MutableMemory(initial++Vector.fill(size-initial.size)(0),size)
}


trait MemoryHandler {
  def blank(size: Int): MemoryContents
  def preloaded(initial: Vector[Int], size: Int): MemoryContents
  // functions changing state (Memory=>Memory)
  def poke: (Int, Int) => MemoryContents => MemoryContents = (address, value) => memory => memory.poke(address, value)
  def pokeW: (Int, Int) => MemoryContents => MemoryContents = (address, value) => memory => memory.pokeW(address, value)
  def pokeMulti: (Int, Vector[Int]) => MemoryContents => MemoryContents = (address, values) => memory => memory.pokeMulti(address, values)
  def loadHexLines: List[String] => MemoryContents => MemoryContents = lines => memory => memory.loadHexLines(lines)
  def lockTo: Int => MemoryContents => MemoryContents = upperAddressExcl => memory => memory.lock(AddressRange(0, upperAddressExcl))
  def lock: AddressRange => MemoryContents => MemoryContents = range => memory => memory.lock(range)
}

trait MemoryContents {
  def lock:AddressRange
  val size:Int
  def apply(address: Int): Int
  def apply(address: Int, offset: Int): Int
  def word(address: Int): Int = word(address,0)
  def word(address: Int, offset: Int): Int = Z80Utils.makeWord(apply(address,offset+1),apply(address, offset))
  def poke(address:Int, value:Int):MemoryContents= if(!lock.overlaps(address)) replaceSingle(address,value) else this
  def pokeMulti(address: Int, values: Vector[Int]): MemoryContents = {
    address + values.size match {
      case overflow if overflow > size =>
        val sizeInRange = values.size - (overflow - size)
        doPokeMulti(address, values.slice(0, sizeInRange))
          .doPokeMulti(0, values.slice(sizeInRange, sizeInRange + overflow - size))
      case _ => doPokeMulti(address, values)
    }
  }
  private def doPokeMulti(address: Int, values: Vector[Int]): MemoryContents = {
    val range = AddressRange(address, address + values.size)
    val locked = lock.overlapping(range)
    (range, locked) match {
      case (empty, _) if empty.isEmpty => this //no change
      case (_, empty) if empty.isEmpty => replaceMulti(address, values) // range is fully unlocked
      case (r, l) if r == l => this // range is fully locked
      case (r, l) => // range is partially locked
        val (firstRange, secondRange) = r.noOverlappingRanges(l)
        replaceMulti(firstRange.from, values.slice(0, firstRange.size))
          .replaceMulti(secondRange.from, values.slice(secondRange.from - address, values.size))
    }
  }
  def pokeW(address: Int, value: Int): MemoryContents = pokeMulti(address, Vector(Z80Utils.getL(value), Z80Utils.getH(value)))
  def lock(range:AddressRange): MemoryContents
  def copy:Vector[Int]
  def loadHexLines(lines: List[String]): MemoryContents = lines.foldLeft(this)((mem, line) => mem.loadHexLine(line))
  def loadHexLine(line: String): MemoryContents = loadHexLine(new HexLineLoader(line))
  def loadHexLine(loader: HexLineLoader): MemoryContents = pokeMulti(loader.address, loader.values)

  def replaceSingle(address: Int, value: Int): MemoryContents
  def replaceMulti(address: Int, values: Vector[Int]): MemoryContents

}

case class AddressRange(from:Int, toExcl:Int) {
  def contains(address:Int):Boolean = address>=from && address<toExcl
  def isEmpty:Boolean = from>=toExcl
  def overlaps(address: Int): Boolean = address>=from && address<toExcl
  def overlaps(other: AddressRange): Boolean = other.from < toExcl && other.toExcl >= from
  def overlapping(other:AddressRange):AddressRange =
    if(overlaps(other)) AddressRange(Math.max(from,other.from),Math.min(toExcl,other.toExcl))
    else AddressRange.emptyFrom(other.from)
  def noOverlappingRanges(other: AddressRange): (AddressRange,AddressRange) = {
    val overlappingRange=overlapping(other)
    val firstRange= overlappingRange match {
      case o if o.from<from => AddressRange(o.from,from)
      case _ => AddressRange.emptyFrom(from)
    }
    val secondRange = overlappingRange match {
      case o if toExcl > o.toExcl => AddressRange(o.toExcl, toExcl)
      case _ => AddressRange.emptyFrom(other.toExcl)
    }
    (firstRange,secondRange)
  }
  def size:Int = Math.max(toExcl-from,0)
}

object AddressRange {
  def emptyFrom(from:Int):AddressRange = new AddressRange(from,from)
  def empty:AddressRange = emptyFrom(0)
}