package org.kr.scala.z80.opcode

object OpCodes {
  // All defined opcodes
  val list:List[OpCode with OpCodeHandledBy]=
    ADD_A_reg.codes ++ ADC_A_reg.codes ++ SUB_reg.codes ++ SBC_A_reg.codes ++
      AND_reg.codes ++ XOR_reg.codes ++ OR_reg.codes ++ CP_reg.codes ++
      INC_reg.codes ++ DEC_reg.codes ++
      BIT_b_reg.codes ++ RES_b_reg.codes ++ SET_b_reg.codes ++
      LD_reg_all.codes ++ LD_HL_reg.codes ++ LD_IXd_reg.codes ++ LD_IYd_reg.codes ++ LD_all_n.codes ++
      RLC_all.codes ++ RRC_all.codes ++ RL_all.codes ++ RR_all.codes ++ SLA_all.codes ++ SRA_all.codes ++ SRL_all.codes ++
      JP_cond.codes ++ CALL_cond.codes ++ RET_cond.codes ++ RST_all.codes ++
      List(
        //Arithmetic8b
        ADD_A_n,ADC_A_n,SUB_n,SBC_A_n,AND_n,XOR_n,OR_n,CP_n,CPL,SCF,CCF,NEG,DAA,
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
        //Block transfer
        LDI,LDIR,LDD,LDDR,
        //Search
        CPI,CPIR,CPD,CPDR,
        //Load 8 bit
        LD_A_I,LD_A_R,LD_I_A,LD_R_A,LD_A_BC,LD_A_DE,LD_BC_A,LD_DE_A,LD_A_nn,LD_nn_A,
        //Load 16 bit
        PUSH_AF,PUSH_BC,PUSH_DE,PUSH_HL,PUSH_IX,PUSH_IY,POP_AF,POP_BC,POP_DE,POP_HL,POP_IX,POP_IY,
        LD_SP_HL,LD_SP_IX,LD_SP_IY,LD_nn_BC,LD_nn_DE,LD_nn_HL,LD_nn_SP,LD_nn_IX,LD_nn_IY,
        LD_BC_nn,LD_DE_nn,LD_HL_nn,LD_SP_nn,LD_IX_nn,LD_IY_nn,LD_BC_i,LD_DE_i,LD_HL_i,LD_SP_i,LD_IX_i,LD_IY_i,
        //Jump
        JP_nn,JP_HL,JP_IX,JP_IY,JR_n,JR_NZ_n,JR_Z_n,JR_NC_n,JR_C_n,CALL_nn,DJNZ,RET,RETI,
        //IO
        IN_A_n,IN_A_C,IN_B_C,IN_C_C,IN_D_C,IN_E_C,IN_H_C,IN_L_C,
        OUT_n_A,OUT_C_A,OUT_C_B,OUT_C_C,OUT_C_D,OUT_C_E,OUT_C_H,OUT_C_L,
        //Control / INT
        EI, DI, IM0, IM1, IM2,
        //NOP, HALT
        NOP, HALT
      )

  //Separate maps to find opcodes 1-, 2- and 3-bytes long
  //(separation due to performance considerations after profiling)
  // 1-byte opcodes
  lazy val mapMainOnly:Map[OpCode,OpCode with OpCodeHandledBy]=
    list
      .filter(op=>op.numberOfCodes==1)
      .foldLeft(Map[OpCode,OpCode with OpCodeHandledBy]())((m,op)=>
        m ++ Map(OpCode(op.main)->op)
      )

  // 2-byte opcodes
  lazy val mapMainSupp:Map[OpCode,OpCode with OpCodeHandledBy]=
    list
      .filter(op=>op.numberOfCodes==2)
      .foldLeft(Map[OpCode,OpCode with OpCodeHandledBy]())((m,op)=>
        m ++ Map(OpCode(op.main,op.supp)->op)
      )

  // 3-byte opcodes
  lazy val mapMainSupp2:Map[OpCode,OpCode with OpCodeHandledBy]=
    list
      .filter(op=>op.numberOfCodes==3)
      .foldLeft(Map[OpCode,OpCode with OpCodeHandledBy]())((m,op)=>
        m ++ Map(OpCode(op.main,op.supp,op.supp2)->op)
      )

  def getOpCodeObject(code:OpCode):OpCode with OpCodeHandledBy= {
    // search in 3 smaller maps instead of one larger list - ~20% faster (many most often used opcodes are 1-byte)
    mapMainOnly.getOrElse(code.mainOnly,
      mapMainSupp.getOrElse(code.mainSupp,
        mapMainSupp2.getOrElse(code,
          new UNKNOWN(code))))
  }
}
