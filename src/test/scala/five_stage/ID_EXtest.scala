package five_stage
import chisel3._
import chisel3.util
import org.scalatest._
import chiseltest._

class TopTest extends FreeSpec with ChiselScalatestTester{
    "ID_EX test" in{
        test(new ID_EX()){ c=>
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)
        step(1)




        }
    }
}