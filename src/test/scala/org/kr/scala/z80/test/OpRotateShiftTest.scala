package org.kr.scala.z80.test

import org.scalatest.funsuite.AnyFunSuite

class OpRotateShiftTest extends AnyFunSuite {

  test("run RLCA") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0x07)), "A", 0x0000, 0x02, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x01)), List((0x0000, 0x07)), "A", 0x0000, 0x02, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x80)), List((0x0000, 0x07)), "A", 0x0000, 0x01, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x80)), List((0x0000, 0x07)), "A", 0x0000, 0x01, "11_0_101")
  }

  test("run RLC r") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0xCB), (0x0001, 0x07)), "A", 0x0000, 0x02, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0x55)), List((0x0000, 0xCB), (0x0001, 0x00)), "B", 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("C", 0x55)), List((0x0000, 0xCB), (0x0001, 0x01)), "C", 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("D", 0xA0)), List((0x0000, 0xCB), (0x0001, 0x02)), "D", 0x0000, 0x41, "00_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("E", 0x00)), List((0x0000, 0xCB), (0x0001, 0x03)), "E", 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("H", 0x55)), List((0x0000, 0xCB), (0x0001, 0x04)), "H", 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("L", 0x55)), List((0x0000, 0xCB), (0x0001, 0x05)), "L", 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("HL", 0x0101)), List((0x0000, 0xCB), (0x0001, 0x06), (0x0101, 0x55)),
      "", 0x0101, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IX", 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x06), (0x0202, 0x55)),
      "", 0x0202, 0xAA, "10_0_100", 4)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IY", 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x06), (0x0200, 0x55)),
      "", 0x0200, 0xAA, "10_0_100", 4)
  }

  test("run RRCA") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x80)), List((0x0000, 0x0F)), "A", 0x0000, 0x40, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x80)), List((0x0000, 0x0F)), "A", 0x0000, 0x40, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0x0F)), "A", 0x0000, 0x80, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x01)), List((0x0000, 0x0F)), "A", 0x0000, 0x80, "11_0_101")
  }

  test("run RRC r") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x80)), List((0x0000, 0xCB), (0x0001, 0x0F)), "A", 0x0000, 0x40, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0xAA)), List((0x0000, 0xCB), (0x0001, 0x08)), "B", 0x0000, 0x55, "00_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("C", 0xAA)), List((0x0000, 0xCB), (0x0001, 0x09)), "C", 0x0000, 0x55, "00_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("D", 0x03)), List((0x0000, 0xCB), (0x0001, 0x0A)), "D", 0x0000, 0x81, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("E", 0x03)), List((0x0000, 0xCB), (0x0001, 0x0B)), "E", 0x0000, 0x81, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("H", 0x00)), List((0x0000, 0xCB), (0x0001, 0x0C)), "H", 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("L", 0x03)), List((0x0000, 0xCB), (0x0001, 0x0D)), "L", 0x0000, 0x81, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("HL", 0x0101)), List((0x0000, 0xCB), (0x0001, 0x0E), (0x0101, 0x03)),
      "", 0x0101, 0x81, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IX", 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x0E), (0x0202, 0x03)),
      "", 0x0202, 0x81, "10_0_101", 4)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IY", 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x0E), (0x0200, 0x03)),
      "", 0x0200, 0x81, "10_0_101", 4)
  }

  test("run RLA") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0x17)), "A", 0x0000, 0x02, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x01)), List((0x0000, 0x17)), "A", 0x0000, 0x03, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x80)), List((0x0000, 0x17)), "A", 0x0000, 0x00, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x80)), List((0x0000, 0x17)), "A", 0x0000, 0x01, "11_0_101")
  }

  test("run RL r") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0xCB), (0x0001, 0x17)), "A", 0x0000, 0x02, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0x55)), List((0x0000, 0xCB), (0x0001, 0x10)), "B", 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("C", 0x55)), List((0x0000, 0xCB), (0x0001, 0x11)), "C", 0x0000, 0xAB, "10_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("D", 0xA0)), List((0x0000, 0xCB), (0x0001, 0x12)), "D", 0x0000, 0x40, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("E", 0x00)), List((0x0000, 0xCB), (0x0001, 0x13)), "E", 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("H", 0x55)), List((0x0000, 0xCB), (0x0001, 0x14)), "H", 0x0000, 0xAB, "10_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("L", 0x85)), List((0x0000, 0xCB), (0x0001, 0x15)), "L", 0x0000, 0x0A, "00_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("HL", 0x0101)), List((0x0000, 0xCB), (0x0001, 0x16), (0x0101, 0x85)),
      "", 0x0101, 0x0A, "00_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IX", 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x16), (0x0202, 0x85)),
      "", 0x0202, 0x0A, "00_0_101", 4)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IY", 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x16), (0x0200, 0x85)),
      "", 0x0200, 0x0A, "00_0_101", 4)
  }

  test("run RRA") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x80)), List((0x0000, 0x1F)), "A", 0x0000, 0x40, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x80)), List((0x0000, 0x1F)), "A", 0x0000, 0xC0, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0x1F)), "A", 0x0000, 0x00, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x01)), List((0x0000, 0x1F)), "A", 0x0000, 0x80, "11_0_101")
  }

  test("run RR r") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x80)), List((0x0000, 0xCB), (0x0001, 0x1F)), "A", 0x0000, 0xC0, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0x01)), List((0x0000, 0xCB), (0x0001, 0x18)), "B", 0x0000, 0x00, "01_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("C", 0xAA)), List((0x0000, 0xCB), (0x0001, 0x19)), "C", 0x0000, 0x55, "00_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("D", 0xAA)), List((0x0000, 0xCB), (0x0001, 0x1A)), "D", 0x0000, 0xD5, "10_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("E", 0x03)), List((0x0000, 0xCB), (0x0001, 0x1B)), "E", 0x0000, 0x01, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("H", 0x03)), List((0x0000, 0xCB), (0x0001, 0x1C)), "H", 0x0000, 0x81, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("L", 0x02)), List((0x0000, 0xCB), (0x0001, 0x1D)), "L", 0x0000, 0x81, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("HL", 0x0101)), List((0x0000, 0xCB), (0x0001, 0x1E), (0x0101, 0x03)),
      "", 0x0101, 0x01, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IX", 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x1E), (0x0202, 0x03)),
      "", 0x0202, 0x01, "00_0_001", 4)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("IY", 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x1E), (0x0200, 0x03)),
      "", 0x0200, 0x81, "10_0_101", 4)
  }

  test("run SLA r") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0xCB), (0x0001, 0x27)), "A", 0x0000, 0x02, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("B", 0x01)), List((0x0000, 0xCB), (0x0001, 0x20)), "B", 0x0000, 0x02, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("C", 0x55)), List((0x0000, 0xCB), (0x0001, 0x21)), "C", 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("D", 0xAA)), List((0x0000, 0xCB), (0x0001, 0x22)), "D", 0x0000, 0x54, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("E", 0x00)), List((0x0000, 0xCB), (0x0001, 0x23)), "E", 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("H", 0x80)), List((0x0000, 0xCB), (0x0001, 0x24)), "H", 0x0000, 0x00, "01_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("L", 0x80)), List((0x0000, 0xCB), (0x0001, 0x25)), "L", 0x0000, 0x00, "01_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("HL", 0x0101)), List((0x0000, 0xCB), (0x0001, 0x26), (0x0101, 0x80)),
      "", 0x0101, 0x00, "01_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IX", 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x26), (0x0202, 0x80)),
      "", 0x0202, 0x00, "01_0_101", 4)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IY", 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x26), (0x0200, 0x80)),
      "", 0x0200, 0x00, "01_0_101", 4)
  }

  test("run SRA r") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x02)), List((0x0000, 0xCB), (0x0001, 0x2F)), "A", 0x0000, 0x01, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0x03)), List((0x0000, 0xCB), (0x0001, 0x28)), "B", 0x0000, 0x01, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("C", 0x01)), List((0x0000, 0xCB), (0x0001, 0x29)), "C", 0x0000, 0x00, "01_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("D", 0x80)), List((0x0000, 0xCB), (0x0001, 0x2A)), "D", 0x0000, 0xC0, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("E", 0x80)), List((0x0000, 0xCB), (0x0001, 0x2B)), "E", 0x0000, 0xC0, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("H", 0x81)), List((0x0000, 0xCB), (0x0001, 0x2C)), "H", 0x0000, 0xC0, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("L", 0x81)), List((0x0000, 0xCB), (0x0001, 0x2D)), "L", 0x0000, 0xC0, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("HL", 0x0101)), List((0x0000, 0xCB), (0x0001, 0x2E), (0x0101, 0x81)),
      "", 0x0101, 0xC0, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IX", 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x2E), (0x0202, 0x81)),
      "", 0x0202, 0xC0, "10_0_101", 4)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IY", 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x2E), (0x0200, 0x81)),
      "", 0x0200, 0xC0, "10_0_101", 4)
  }

  test("run RLD") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x12),("HL",0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x34)),
      "A", 0x0000, 0x13, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x12),("HL",0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x34)),
      "", 0x1212, 0x42, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("A", 0x87),("HL",0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x65)),
      "A", 0x0000, 0x86, "10_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("A", 0x87),("HL",0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x65)),
      "", 0x1212, 0x57, "10_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01),("HL",0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x02)),
      "A", 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01),("HL",0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x02)),
      "", 0x1212, 0x21, "01_0_100", 2)
  }

  test("run RRD") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x12), ("HL", 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0x34)),
      "A", 0x0000, 0x14, "00_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x12), ("HL", 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0x34)),
      "", 0x1313, 0x23, "00_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("A", 0xAB), ("HL", 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0xCD)),
      "A", 0x0000, 0xAD, "10_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("A", 0xAB), ("HL", 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0xCD)),
      "", 0x1313, 0xBC, "10_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x02), ("HL", 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0x30)),
      "A", 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x02), ("HL", 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0x30)),
      "", 0x1313, 0x23, "01_0_100", 2)
  }
}