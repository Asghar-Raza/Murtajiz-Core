package Core

import chisel3._
import chisel3.util._

class IO_Interface1(width:Int) extends Bundle{
    val index = log2Ceil(width)
    val i_type = Input(UInt(1.W))
    val alu_oper = Input(UInt(index.W))

    val alu_a = Input(SInt(width.W))
    val alu_b = Input(SInt(width.W))
    val alu_out = Output(SInt(width.W))
    val alu_branch = Output(UInt(1.W))
}

class ALU(width : Int) extends Module{
    val io = IO(new IO_Interface1(width))
    io.alu_out := 0.S

    // and

    when(io.alu_oper === "b00000".U){  // add 00000
        io.alu_out := io.alu_a + io.alu_b
    }
    .elsewhen(io.alu_oper === "b01000".U){  // sub 01000
        when(io.i_type === 1.U){
        io.alu_out := io.alu_a + io.alu_b

        }.otherwise{

            io.alu_out := io.alu_a - io.alu_b
        }
    }
    .elsewhen(io.alu_oper === "b00001".U){ // sll 00001
        io.alu_out := (io.alu_a << io.alu_b(4,0)).asSInt
    }
    .elsewhen(io.alu_oper === "b00010".U){ // slt 00010
        when(io.alu_a.asSInt < io.alu_b.asSInt){
            io.alu_out := 1.S
        }.otherwise{
            io.alu_out := 0.S
            
        }
    }
    .elsewhen(io.alu_oper === "b00011".U){ // sltu 00011
        val a = io.alu_a.asUInt
        val b = io.alu_b.asUInt
        when(io.alu_a < io.alu_b){
            io.alu_out := 1.S
        }.otherwise{
            io.alu_out := 0.S
            
        }
    }
    .elsewhen(io.alu_oper === "b00100".U){ // sltu 00100
        io.alu_out := io.alu_a ^ io.alu_b
    }
    .elsewhen((io.alu_oper === "b00101".U)|(io.alu_oper === "b01101".U)){ // srl 00101 sra 01101
        io.alu_out := io.alu_a >> io.alu_b(4,0)
    }
    .elsewhen(io.alu_oper === "b00110".U){ // or 00110
        io.alu_out := io.alu_a | io.alu_b
    }
    
    .elsewhen(io.alu_oper === "b00111".U){ // and 00111
        io.alu_out := io.alu_a & io.alu_b
    }
    .elsewhen(io.alu_oper === "b10000".U){ // beq 10000
        when(io.alu_a === io.alu_b){
            io.alu_out := 1.S
           
        }.otherwise{
            io.alu_out := 0.S
        }
    }
    .elsewhen(io.alu_oper === "b10001".U){ // bne 10001
        when(io.alu_a === io.alu_b){
            io.alu_out := 0.S
           
        }.otherwise{
            io.alu_out := 1.S
        }
    }
    .elsewhen(io.alu_oper === "b10100".U){ // blt 10100
        when(io.alu_a < io.alu_b){
            io.alu_out := 1.S
           
        }.otherwise{
            io.alu_out := 0.S
        }
    }
    .elsewhen(io.alu_oper === "b10101".U){ // bge 10101
        when((io.alu_a === io.alu_b)|(io.alu_a > io.alu_b)){
            io.alu_out := 1.S
           
        }.otherwise{
            io.alu_out := 0.S
        }
    }
    .elsewhen(io.alu_oper === "b10110".U){ // bltu 10110
        val a = io.alu_a.asUInt
        val b = io.alu_b.asUInt
        when(a > b){
            io.alu_out := 1.S
           
        }.otherwise{
            io.alu_out := 0.S
        }
    }
    .elsewhen(io.alu_oper === "b10111".U){ // bgeu 10111
        val a = io.alu_a.asUInt
        val b = io.alu_b.asUInt
        when((a===b)|(a>b)){
            io.alu_out := 1.S
           
        }.otherwise{
            io.alu_out := 0.S
        }
    }.elsewhen(io.alu_oper === "b11111".U){ //jal jalr 11111
        io.alu_out := io.alu_a
    }

    // branching
    when((io.alu_oper(4,3)==="b10".U) && (io.alu_out === 1.S)){
        io.alu_branch := 1.U
    }.otherwise{
        io.alu_branch := 0.U
    }
    
}