package five_stage
import chisel3._
import chisel3.util._

class ControlUnit extends Module{
    val io = IO(new Bundle{
        val Opcode = Input(UInt(7.W))
        val Branch = Output(UInt(1.W))
        val RegWrite = Output(UInt(1.W))
        val Immediate = Output(UInt(1.W))
        val Load = Output(UInt(1.W))
        val Store = Output(UInt(1.W))
        val UType = Output(UInt(1.W))
        val Auipc = Output(UInt(1.W))
        val Jalr = Output(UInt(1.W))
        val jal = Output(UInt(1.W))
    })
    val TypeDecoded = Module(new TypeDecode())
    val ControlDecoded = Module(new ControlDecode())

    TypeDecoded.io.opcode := io.Opcode
    ControlDecoded.io.R_type := TypeDecoded.io.R_type
    ControlDecoded.io.I_type := TypeDecoded.io.I_type
    ControlDecoded.io.S_type := TypeDecoded.io.S_type
    ControlDecoded.io.SB_type := TypeDecoded.io.SB_type
    ControlDecoded.io.L_type := TypeDecoded.io.L_type
    ControlDecoded.io.U_type := TypeDecoded.io.U_type
    ControlDecoded.io.UJ_type := TypeDecoded.io.UJ_type
    ControlDecoded.io.auipc_type := TypeDecoded.io.auipc_type
    ControlDecoded.io.jalr_type := TypeDecoded.io.jalr_type


    io.Branch := ControlDecoded.io.BranchOut
    io.RegWrite := ControlDecoded.io.RegWriteOut
    io.Immediate := ControlDecoded.io.ImmediateSelOut
    io.Load := ControlDecoded.io.LoadOut
    io.Store := ControlDecoded.io.StoreOut
    io.UType := ControlDecoded.io.UtypeOut
    io.Auipc := ControlDecoded.io.AuipcOut
    io.Jalr := ControlDecoded.io.JalrOut
    io.jal := ControlDecoded.io.JalOut



}