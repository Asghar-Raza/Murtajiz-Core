package five_stage
import chisel3._
import chisel3.util._

class TypeDecode extends Module{
    val io = IO(new Bundle{
        val opcode = Input(UInt(7.W))
        val R_type = Output(UInt(1.W))
        val I_type = Output(UInt(1.W))
        val S_type = Output(UInt(1.W))
        val SB_type = Output(UInt(1.W))
        val L_type = Output(UInt(1.W))
        val U_type = Output(UInt(1.W))
        val UJ_type = Output(UInt(1.W))
        val auipc_type = Output(UInt(1.W))
        val jalr_type = Output(UInt(1.W))
    })
    
    io.R_type := 0.U
    io.I_type := 0.U
    io.S_type := 0.U
    io.SB_type := 0.U
    io.L_type := 0.U
    io.U_type := 0.U
    io.UJ_type := 0.U
    io.auipc_type := 0.U
    io.jalr_type := 0.U

    when(io.opcode === "h33".U){
        io.R_type := 1.U
    }
    .elsewhen(io.opcode === "h13".U){
        io.I_type := 1.U
    }
    .elsewhen(io.opcode === "h63".U){
        io.SB_type := 1.U
    }
    .elsewhen(io.opcode === "h03".U){
        io.L_type := 1.U
    }
    .elsewhen(io.opcode === "h23".U){
        io.S_type := 1.U
    }
    .elsewhen(io.opcode === "h37".U){
        io.U_type := 1.U
    }
    .elsewhen(io.opcode === "h6f".U){
        io.UJ_type := 1.U
    }
    .elsewhen(io.opcode === "h17".U){
        io.auipc_type := 1.U
    }
    .elsewhen(io.opcode === "h67".U){
        io.jalr_type := 1.U
    }
}