package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{DummyChange, OutputChange, RegisterChange, SystemChangeBase, Z80System}

object InputOutput extends OperationSpec with OpCodeHandler {

  val portLocationListMap: Map[List[OpCode],Location] = Map(
    List(OpCode(0xDB),OpCode(0xD3))->Location.registerAddrDirOffset("PC", 1),
      OpCode.generateListByReg(OpCode(0xED,0x41),2,3) ->Location.register("C")
  )
  val portLocation: OpCodeMap[Location] = new OpCodeMap(portLocationListMap, Location.empty)

  val valueLocationListMap: Map[List[OpCode],Location] = Map(
    List(OpCode(0xD3),OpCode(0xDB))->Location.register("A")
  ) ++
  //registers
    OpCode.generateMapByReg(OpCode(0xED,0x41),2,3)

  val valueLocation: OpCodeMap[Location] = new OpCodeMap(valueLocationListMap, Location.empty)

  val operationListMap: Map[List[OpCode],InOutOperation] = Map(
    List(OpCode(0xD3))->InOutOpType.Out,
    OpCode.generateListByReg(OpCode(0xED,0x41),2,3)->InOutOpType.Out,
    List(OpCode(0xDB))->InOutOpType.In,
  )

  val operation: OpCodeMap[InOutOperation] = new OpCodeMap(operationListMap, InOutOpType.None)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0xD3),OpCode(0xDB))->2,
    OpCode.generateListByReg(OpCode(0xED,0x41),2,3)->2
  )

  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    val port=system.getValueFromLocation(portLocation.find(code))
    val chgList=operation.find(code).handle(system,port,valueLocation.find(code))
    (List(chgList),instSize.find(code))
  }
}

sealed abstract class InOutOperation(val name:String) {
  def handle(system:Z80System,port:Int,location:Location):SystemChangeBase
}

object InOutOpType {
  case object In extends InOutOperation("IN") {
    override def handle(system:Z80System,port:Int,location:Location):SystemChangeBase=
      system.putValueToLocation(location,system.readPort(port))
  }
  case object Out extends InOutOperation("OUT") {
    override def handle(system:Z80System,port:Int,location:Location):SystemChangeBase=
      new OutputChange(port,system.getValueFromLocation(location))
  }
  case object None extends InOutOperation("NONE") {
    override def handle(system:Z80System,port:Int,location:Location):SystemChangeBase=
      new DummyChange()
  }
}
