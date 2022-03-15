package five_stage
import chisel3._
import chisel3.util._


class ImmediateGeneration extends Module{
    val io = IO(new Bundle{
		val inst = Input(UInt(32.W))
		val PC = Input(UInt(32.W))
		val S_Imm = Output(SInt(32.W))
		val SB_Imm = Output(SInt(32.W))
		val U_Imm = Output(SInt(32.W))
		val UJ_Imm = Output(SInt(32.W))
		val I_Imm = Output(SInt(32.W))
	})
    val imm7 = io.inst(7)
    val imm11 = io.inst(11,8)
    val imm19 = io.inst(19,12)
    val imm20 = io.inst(20)
    val imm24 = io.inst(24,21)
    val imm30 = io.inst(30,25)
    val imm31 = io.inst(31)
    
    io.I_Imm := Cat(Fill(20,imm31),imm31,imm30,imm24,imm20).asSInt
    io.S_Imm := Cat(Fill(20,imm31),imm31,imm30,imm11,imm7).asSInt
    io.SB_Imm := Cat(Fill(19,imm31),imm31,imm7,imm30,imm11,0.U).asSInt + io.PC.asSInt
    io.U_Imm := (Cat(Fill(12,imm31),imm31,imm30,imm24,imm20,imm19)).asSInt
    io.UJ_Imm := Cat(Fill(11,imm31),imm31,imm19,imm20,imm30,imm24,0.U).asSInt + io.PC.asSInt
    


}