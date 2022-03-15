package five_stage
import chisel3._
import chisel3.util._

class Top extends Module{
    val io =  IO(new Bundle{
        val Instruction = Output(UInt(32.W))
        val pc = Output(UInt(32.W))
        val writeBack = Output(SInt(32.W))
        val regDataA = Output(SInt(32.W))
        val regDataB = Output(SInt(32.W))
        val AluResult = Output(SInt(32.W))
        val rs1 = Output(UInt(32.W))
        val rs2 = Output(UInt(32.W))
        val CoreAluA = Output(SInt(32.W))
        val CoreAluB = Output(SInt(32.W))
    })

    val ALU = Module(new ALU(32))
    val AluControl = Module(new AluControl())
    val ControlUnit = Module(new ControlUnit())
    val DataMemory = Module(new DataMemory())
    val ImmediateGeneration = Module(new ImmediateGeneration())
    val InstructionMemory = Module(new InstructionMemory())
    val PC = Module(new PC())
    val Regfile = Module(new Regfile())
    

    // val loadPC = RegNext(0.U(32.W))
    // when(loadPC === 0.U){
    //     PC.io.input := 0.U
    //     loadPC := 1.U
    // }.otherwise{
    PC.io.input := PC.io.pc4

    
    
    
    io.pc := PC.io.pc
        // instruction memory and control unit

    
    InstructionMemory.io.instAddr := PC.io.pc(11,2)
    ControlUnit.io.Opcode := InstructionMemory.io.instOut(6,0)

    // immediate generator
    ImmediateGeneration.io.inst := InstructionMemory.io.instOut
    ImmediateGeneration.io.PC := PC.io.pc

    // ALU CONtrol
    AluControl.io.func3 := InstructionMemory.io.instOut(14,12)
    AluControl.io.func7 := InstructionMemory.io.instOut(30)
    AluControl.io.branch :=  ControlUnit.io.Branch


    // reg file
    Regfile.io.rs1 := InstructionMemory.io.instOut(19,15)
    Regfile.io.rs2 := InstructionMemory.io.instOut(24,20)
    Regfile.io.writeEnable := ControlUnit.io.RegWrite
    io.rs1 := Regfile.io.rs1 // for expecting outputs
    io.rs2 := Regfile.io.rs2 // for expecting outputs
    // reg writeback logic
    
    
    when(ControlUnit.io.jal === 1.U){
        Regfile.io.writeBack := (PC.io.pc4).asSInt
    }
    .elsewhen(ControlUnit.io.UType === 1.U)
    {
        Regfile.io.writeBack := ImmediateGeneration.io.U_Imm

    }.elsewhen(ControlUnit.io.Load === 1.U){
        Regfile.io.writeBack := (DataMemory.io.DataOut).asSInt
        
    }.otherwise{
        Regfile.io.writeBack := ALU.io.alu_out

    }


    // regfile rd logic
    when(ControlUnit.io.jal === 1.U){
    Regfile.io.rd := "h01".U
    }.otherwise{
        Regfile.io.rd := InstructionMemory.io.instOut(11,7)
    }

    // ALU

    // bus a logic
    when(ControlUnit.io.Auipc === 1.U){
        ALU.io.alu_a := (PC.io.pc4).asSInt
        
    }.otherwise{
        ALU.io.alu_a := Regfile.io.Aout

    }

    // handling addi for negative immediates
    ALU.io.i_type := ControlUnit.io.Immediate

    // bus b logic
    val TwoBit = Cat(ControlUnit.io.Auipc,ControlUnit.io.Immediate)
    when(TwoBit === "b00".U){
        ALU.io.alu_b := Regfile.io.Bout

    }.elsewhen(TwoBit === "b01".U){
        when(ControlUnit.io.Store === 1.U  ){
            ALU.io.alu_b := ImmediateGeneration.io.S_Imm

        }.otherwise{
            ALU.io.alu_b := ImmediateGeneration.io.I_Imm

        }
    }.otherwise{
        ALU.io.alu_b := ImmediateGeneration.io.U_Imm

    }


    // ALUOP logic
    when(ControlUnit.io.Load === 1.U || ControlUnit.io.Store === 1.U){
        ALU.io.alu_oper := "h00".U
    }.otherwise{
        ALU.io.alu_oper := AluControl.io.AluControlOut

    }

    io.CoreAluA :=  ALU.io.alu_a
    io.CoreAluB :=  ALU.io.alu_b


    // data memory
    when(ControlUnit.io.Jalr === 1.U){
            PC.io.input := (ALU.io.alu_out).asUInt
        }.elsewhen(ControlUnit.io.jal === 1.U){
            PC.io.input := (ImmediateGeneration.io.UJ_Imm).asUInt
        }.elsewhen(ALU.io.alu_branch === 1.U && ControlUnit.io.Branch === 1.U){
            PC.io.input := (ImmediateGeneration.io.SB_Imm).asUInt
        }.otherwise{
            PC.io.input := PC.io.pc4
        }


    DataMemory.io.DataAddr := (ALU.io.alu_out).asUInt
    DataMemory.io.DataIn := Regfile.io.Bout
    DataMemory.io.store := ControlUnit.io.Store
    DataMemory.io.load := ControlUnit.io.Load



    io.Instruction := InstructionMemory.io.instOut
    io.writeBack := Regfile.io.writeBack
    io.regDataA := Regfile.io.Aout
    io.regDataB := Regfile.io.Bout
    io.AluResult := ALU.io.alu_out



    // INSTRUCTION FETCH STAGE
    imem.io.wrAddr := PC.io.out(11,2).asUInt
    IF_ID.io.pc_in := PC.io.out
    IF_ID.io.pc4_in := PC.io.pc4
    IF_ID.io.inst_in := imem.io.readData
    PC.io.input := PC.io.pc4


    // INSTRUCTION DECODE STAGE
    control.io.in_opcode := IF_ID.io.inst_out(6, 0)

    reg_file.io.rs1_sel := IF_ID.io.inst_out(19, 15)
    reg_file.io.rs2_sel := IF_ID.io.inst_out(24, 20)
    reg_file.io.regWrite := MEM_WB.io.mem_wb_regWr_output
    reg_file.io.rd_sel := MEM_WB.io.mem_wb_rdSel_output

    imm_generation.io.instruction := IF_ID.io.inst_out
    imm_generation.io.pc := IF_ID.io.pc_out

    ID_EX.io.IF_ID_pc := IF_ID.io.pc_out
    ID_EX.io.IF_ID_pc4 := IF_ID.io.pc4_out
    ID_EX.io.func3_in := IF_ID.io.inst_out(14,12)
    ID_EX.io.func7_in := IF_ID.io.inst_out(30)
    ID_EX.io.rd_sel_in := IF_ID.io.inst_out(11,7)
    ID_EX.io.rs1_sel_in := IF_ID.io.inst_out(19, 15)
    ID_EX.io.rs2_sel_in := IF_ID.io.inst_out(24, 20)
    ID_EX.io.rs1_in := reg_file.io.rs1
    ID_EX.io.rs2_in := reg_file.io.rs2

    ID_EX.io.ctrl_MemWr_in := control.io.out_memWrite
    ID_EX.io.ctrl_MemRd_in := control.io.out_memRead
    ID_EX.io.ctrl_Branch_in := control.io.out_branch
    ID_EX.io.ctrl_RegWr_in := control.io.out_regWrite
    ID_EX.io.ctrl_MemToReg_in := control.io.out_memToReg
    ID_EX.io.ctrl_AluOp_in := control.io.out_aluOp
    ID_EX.io.ctrl_OpA_sel_in := control.io.out_operand_a_sel
    ID_EX.io.ctrl_OpB_sel_in := control.io.out_operand_b_sel
    ID_EX.io.ctrl_nextPc_sel_in := control.io.out_next_pc_sel

    when(control.io.out_extend_sel === "b00".U) {
    // I-Type instruction
    ID_EX.io.imm := imm_generation.io.i_imm
} .elsewhen(control.io.out_extend_sel === "b01".U) {
    // S-Type instruction
    ID_EX.io.imm := imm_generation.io.s_imm
} .elsewhen(control.io.out_extend_sel === "b10".U) {
    // U-Type instruction
    ID_EX.io.imm := imm_generation.io.u_imm
} .otherwise {
    ID_EX.io.imm := 0.S(32.W)
}

    // EXECUTION STAGE
    alu.io.oper_a := ID_EX.io.rs1_out

    when(ID_EX.io.ctrl_OpB_sel_out === "b1".U) {
        alu.io.oper_b := ID_EX.io.imm_out
    } .otherwise {
    alu.io.oper_b := ID_EX.io.rs2_out
    }

    alu.io.aluCtrl := alu_control.io.output

    alu_control.io.aluOp := ID_EX.io.ctrl_AluOp_out
    alu_control.io.func7 := ID_EX.io.func7_out
    alu_control.io.func3 := ID_EX.io.func3_out

    EX_MEM.io.alu_output := alu.io.output
    EX_MEM.io.ID_EX_RDSEL := ID_EX.io.rd_sel_out
    EX_MEM.io.ID_EX_RS2SEL := ID_EX.io.rs2_sel_out
    EX_MEM.io.ID_EX_MEMWR := ID_EX.io.ctrl_MemWr_out
    EX_MEM.io.ID_EX_MEMRD := ID_EX.io.ctrl_MemRd_out
    EX_MEM.io.ID_EX_REGWR := ID_EX.io.ctrl_RegWr_out
    EX_MEM.io.ID_EX_MEMTOREG := ID_EX.io.ctrl_MemToReg_out

    EX_MEM.io.ID_EX_RS2 := ID_EX.io.rs2_out


    // MEMORY STAGE
    MEM_WB.io.in_alu_output := EX_MEM.io.ex_mem_alu_output

    MEM_WB.io.in_dataMem_data := dmem.io.memOut

    MEM_WB.io.in_dataMem_data := dmem.io.memOut

    dmem.io.memAddress := EX_MEM.io.ex_mem_alu_output(11, 2).asUInt
    dmem.io.memWrite := EX_MEM.io.ex_mem_memWr_out
    dmem.io.memRead := EX_MEM.io.ex_mem_memRd_out
    dmem.io.memData := EX_MEM.io.ex_mem_rs2_output


    // WRITE BACK STAGE
    when(MEM_WB.io.mem_wb_memToReg_output === "b1".U) {
    reg_file.io.writeData := MEM_WB.io.mem_wb_dataMem_data
    } .otherwise {
    reg_file.io.writeData := MEM_WB.io.mem_wb_alu_output
    }

    







}