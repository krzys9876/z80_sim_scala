package org.kr.scala.z80

import org.kr.scala.z80.system.{CharFormatter, Debugger, DummyDebugger, InputController, InputFile, InputPortMultiple, MemoryController, OutputController, OutputFormatter, Outputter, PrintOutputter, RegisterController, Z80System, Z80SystemController}

import scala.jdk.CollectionConverters.ListHasAsScala
import java.nio.file.{Files, Path}

object Main extends App {

  val CONTROL_PORT=0xB1
  val DATA_PORT=0xB0
  val CR=0x0D
  val hexFile=Path.of("C:\\data\\data","basicall_KR_simpleIO_01.hex")
  val hexLines=Files.readAllLines(hexFile).asScala.toList
  // memory
  val memory=MemoryController.blank(0x10000) >>= MemoryController.loadHexLines(hexLines) >>= MemoryController.lockTo(0x4000)
  val keys="""
    |65536
    |10 FOR I=1 TO 10
    |20 PRINT I
    |30 NEXT I
    |LIST
    |RUN
    """.stripMargin
  val keysList:List[Int]=InputFile.lines2IntsCR(keys,CR)
  val inputPortKeys=new InputPortMultiple(keysList)
  val inputPortControl=new InputPortMultiple(List.fill(keysList.length)(1))
  val input=InputController.blank >>=
    InputController.attachPort(CONTROL_PORT,inputPortControl) >>=
    InputController.attachPort(DATA_PORT,inputPortKeys)
  val initSystem=Z80SystemController(new Z80System(memory,RegisterController.blank,OutputController.blank,input))

  //implicit val debugger:Debugger=ConsoleDetailedDebugger
  implicit val debugger:Debugger=DummyDebugger
  val after=initSystem >>= Z80SystemController.run(debugger)(100000)

  implicit val outputFormatter:OutputFormatter=CharFormatter
  implicit val outputter:Outputter=PrintOutputter
  after.get.outputController.get.show(DATA_PORT)
}