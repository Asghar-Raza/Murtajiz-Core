package Core
import chisel3._
import chisel3.util._

class ControlDecode extends Module{
    val io = IO(new Bundle{
        val R_type = Input(UInt(1.W))
        val I_type = Input(UInt(1.W))
        val S_type = Input(UInt(1.W))
        val SB_type = Input(UInt(1.W))
        val L_type = Input(UInt(1.W))
        val U_type = Input(UInt(1.W))
        val UJ_type = Input(UInt(1.W))
        val auipc_type = Input(UInt(1.W))
        val jalr_type = Input(UInt(1.W))
        val BranchOut = Output(UInt(1.W))
        val RegWriteOut = Output(UInt(1.W))
        val ImmediateSelOut = Output(UInt(1.W))
        val StoreOut = Output(UInt(1.W))
        val LoadOut = Output(UInt(1.W))
        val UtypeOut = Output(UInt(1.W))
        val AuipcOut = Output(UInt(1.W))
        val JalrOut = Output(UInt(1.W))
        val JalOut = Output(UInt(1.W))
    })
        io.BranchOut := 0.U
        io.RegWriteOut := 0.U
        io.ImmediateSelOut := 0.U
        io.StoreOut := 0.U
        io.LoadOut := 0.U
        io.UtypeOut := 0.U
        io.AuipcOut := 0.U
        io.JalrOut := 0.U
        io.JalOut := 0.U

        when(io.SB_type === 1.U){
            io.BranchOut := 1.U
        }
        when(io.UJ_type === 1.U || io.U_type === 1.U || io.R_type === 1.U || io.I_type === 1.U || io.L_type === 1.U || io.auipc_type === 1.U){
            io.RegWriteOut := 1.U

        }
        when(io.I_type === 1.U || io.S_type === 1.U || io.L_type === 1.U ){
            io.ImmediateSelOut := 1.U
        }
        when(io.S_type === 1.U){
            io.StoreOut := 1.U
        }
        when(io.L_type === 1.U){
            io.LoadOut := 1.U
        }
        when(io.U_type === 1.U){
            io.UtypeOut := 1.U
        }
        when(io.auipc_type === 1.U){
            io.AuipcOut := 1.U
        }
        when(io.jalr_type === 1.U){
            io.JalrOut := 1.U
        }
        when(io.UJ_type === 1.U){
            io.JalOut := 1.U
        }
}