package Core
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



    

    







}