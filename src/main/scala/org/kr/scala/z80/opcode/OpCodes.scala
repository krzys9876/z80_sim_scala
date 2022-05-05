package org.kr.scala.z80.opcode

import org.kr.scala.z80.opcode.handler.{BitOperation, ExchangeLocationBase, InOutOperation, JumpCondition, JumpOperation, Load16BitOpType, Load8BitOpType, OpCodeHandler}

//TODO: all pointer to opcode handler in definition of every opcode - this will also simplify Z80System.handle method
object OpCodes {
  val list:List[OpCode]=
    ADD_A_reg.codes ++ ADC_A_reg.codes ++ SUB_reg.codes ++ SBC_A_reg.codes ++
      AND_reg.codes ++ XOR_reg.codes ++ OR_reg.codes ++ CP_reg.codes ++
      INC_reg.codes ++ DEC_reg.codes ++
      BIT_b_reg.codes ++ RES_b_reg.codes ++ SET_b_reg.codes ++
      LD_reg_all.codes ++ LD_HL_reg.codes ++ LD_IXd_reg.codes ++ LD_IYd_reg.codes ++ LD_all_n.codes ++
      RLC_all.codes ++ RRC_all.codes ++ RL_all.codes ++ RR_all.codes ++ SLA_all.codes ++ SRA_all.codes ++ SRL_all.codes ++
      JP_cond.codes ++ CALL_cond.codes ++ RET_cond.codes ++ RST_all.codes ++
      List(
        //Arithmetic8b
        ADD_A_n,ADC_A_n,SUB_n,SBC_A_n,AND_n,XOR_n,OR_n,CP_n,CPL,SCF,CCF,NEG,
        //Rotate digit
        RLD,RRD,
        //Rotate shift
        RLCA,RRCA,RLA,RRA,
        //Arithmetic16b
        ADD_HL_BC,ADD_HL_DE,ADD_HL_HL,ADD_HL_SP,ADD_IX_BC,ADD_IX_DE,ADD_IX_IX,ADD_IX_SP,
        ADD_IY_BC,ADD_IY_DE,ADD_IY_IY,ADD_IY_SP,ADC_HL_BC,ADC_HL_DE,ADC_HL_HL,ADC_HL_SP,
        SBC_HL_BC,SBC_HL_DE,SBC_HL_HL,SBC_HL_SP,INC_BC,INC_DE,INC_HL_16,INC_SP,INC_IX,INC_IY,
        DEC_BC,DEC_DE,DEC_HL_16,DEC_SP,DEC_IX,DEC_IY,
        //Exchange
        EX_DE_HL, EX_AF_AF1, EXX, EX_SP_HL, EX_SP_IX, EX_SP_IY,
        //Load 8 bit
        LD_A_I,LD_A_R,LD_I_A,LD_R_A,LD_A_BC,LD_A_DE,LD_BC_A,LD_DE_A,LD_A_nn,LD_nn_A,
        //Load 16 bit
        PUSH_AF,PUSH_BC,PUSH_DE,PUSH_HL,PUSH_IX,PUSH_IY,POP_AF,POP_BC,POP_DE,POP_HL,POP_IX,POP_IY,
        LD_SP_HL,LD_SP_IX,LD_SP_IY,LD_nn_BC,LD_nn_DE,LD_nn_HL,LD_nn_SP,LD_nn_IX,LD_nn_IY,
        LD_BC_nn,LD_DE_nn,LD_HL_nn,LD_SP_nn,LD_IX_nn,LD_IY_nn,LD_BC_i,LD_DE_i,LD_HL_i,LD_SP_i,LD_IX_i,LD_IY_i,
        //Jump
        JP_nn,JP_HL,JP_IX,JP_IY,JR_n,JR_NZ_n,JR_Z_n,JR_NC_n,JR_C_n,CALL_nn,DJNZ,RET,RETI,
        //IO
        IN_A_n,OUT_n_A,OUT_C_A,OUT_C_B,OUT_C_D,OUT_C_E,OUT_C_H,OUT_C_L,
        //NOP
        NOP
      )

  //TODO: flatten list - refactor OpCodeMap
  //NOTE: cannot use generics in vals (only defs) - these maps are used in vals in other classes
  val operandMap:Map[List[OpCode],Location]= //opCodeListToMap[OpCodeOperandLocation,LoadLocation](op=>op.operand)
  //filterTo(op=>op.isInstanceOf[OpCodeOperandLocation]).map(op=> List(op)->op.asInstanceOf[OpCodeOperandLocation].operand).toMap
  list
    .filter(_.isInstanceOf[OpCodeOperandLocation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeOperandLocation].operand).toMap
  val sourceMap:Map[List[OpCode],Location]= list
    .filter(_.isInstanceOf[OpCodeSourceLocation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeSourceLocation].source).toMap
  val destinationMap:Map[List[OpCode],Location]= list
    .filter(_.isInstanceOf[OpCodeDestLocation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeDestLocation].destination).toMap
  val operation8bMap:Map[List[OpCode],ArithmeticOperation]= list
    .filter(_.isInstanceOf[OpCodeArithmetic8b])
    .map(op=> List(op)->op.asInstanceOf[OpCodeArithmetic8b].operation).toMap
  val rotateShiftMap:Map[List[OpCode],ArithmeticOperation]= list
    .filter(_.isInstanceOf[OpCodeRotateShift])
    .map(op=> List(op)->op.asInstanceOf[OpCodeRotateShift].operation).toMap
  val rotateDigitMap:Map[List[OpCode],ArithmeticOperation]= list
    .filter(_.isInstanceOf[OpCodeRotateDigit])
    .map(op=> List(op)->op.asInstanceOf[OpCodeRotateDigit].operation).toMap
  val operation16bMap:Map[List[OpCode],ArithmeticOperation]= list
    .filter(_.isInstanceOf[OpCodeArithmetic16b])
    .map(op=> List(op)->op.asInstanceOf[OpCodeArithmetic16b].operation).toMap
  val exchangeMap:Map[List[OpCode],List[ExchangeLocationBase]]= list
    .filter(_.isInstanceOf[OpCodeExchangeLocation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeExchangeLocation].exchange).toMap
  val bitManipulationMap:Map[List[OpCode],BitOperation]= list
    .filter(_.isInstanceOf[OpCodeBitManipulation])
    .map(op=> List(op)->op.asInstanceOf[OpCodeBitManipulation].operation).toMap
  val bitNumMap:Map[List[OpCode],Int]= list
    .filter(_.isInstanceOf[BitManipulationDef])
    .map(op=> List(op)->op.asInstanceOf[BitManipulationDef].bit).toMap
  val load8bMap:Map[List[OpCode],Load8BitOpType]= list
    .filter(_.isInstanceOf[Load8BitOp])
    .map(op=> List(op)->op.asInstanceOf[Load8BitOp].operation).toMap
  val load16bMap:Map[List[OpCode],Load16BitOpType]= list
    .filter(_.isInstanceOf[Load16BitOp])
    .map(op=> List(op)->op.asInstanceOf[Load16BitOp].operation).toMap
  val stackChangeMap:Map[List[OpCode],Int]= list
    .filter(_.isInstanceOf[OpStackChange])
    .map(op=> List(op)->op.asInstanceOf[OpStackChange].stackChange).toMap
  val jumpConditionMap:Map[List[OpCode],JumpCondition]= list
    .filter(_.isInstanceOf[OpCodeJumpCondition])
    .map(op=> List(op)->op.asInstanceOf[OpCodeJumpCondition].condition).toMap
  val jumpOperationMap:Map[List[OpCode],JumpOperation]= list
    .filter(_.isInstanceOf[OpCodeJump])
    .map(op=> List(op)->op.asInstanceOf[OpCodeJump].operation).toMap
  val inOutOperationMap:Map[List[OpCode],InOutOperation]= list
    .filter(_.isInstanceOf[OpCodeInOut])
    .map(op=> List(op)->op.asInstanceOf[OpCodeInOut].operation).toMap
  val nopMap:Map[List[OpCode],_]= list
    .filter(_.equals(NOP))
    .map(op=> List(op)->Nil).toMap

  val sizeMap:Map[List[OpCode],Int]= list
    .filter(_.isInstanceOf[OpCodeSize])
    .map(op=> List(op)->op.asInstanceOf[OpCodeSize].size).toMap
}
