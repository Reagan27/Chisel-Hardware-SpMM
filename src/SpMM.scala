import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SpMM extends Module {
  val io = IO(new Bundle {
    // Inputs for matrix A (sparse)
    val inARowIdx = Input(Vec(16, UInt(4.W))) // RowIdx for matrix A
    val inACol = Input(Vec(16, UInt(4.W)))     // Col for matrix A
    val inAData = Input(Vec(16, UInt(4.W)))    // Data for matrix A

    // Inputs for matrix B (dense, transposed)
    val inBRhsReset = Input(Bool())            // Indicates the start of a new input process for matrix B
    val inBRhsData = Input(Vec(16, UInt(4.W))) // Data for matrix B (a row of the transpose of B)

    // Output for matrix C (dense, transposed)
    val outCValid = Output(Bool())             // Output valid signal
    val outCData = Output(Vec(16, UInt(4.W)))  // Data for matrix C 
  })

  // State machine definition
  val idle :: processing :: Nil = Enum(2)
  val state = RegInit(idle)

  // Buffer for storing matrix B (transposed)
  val bBuffer = Reg(Vec(16, UInt(4.W)))

  // Counter for tracking the progress of input B
  val bCounter = RegInit(0.U(4.W))

  // Logic for input B processing
  when(io.inBRhsReset) {
    state := processing
    bCounter := 0.U
    bBuffer := io.inBRhsData
  }.elsewhen(state === processing) {
    when(bCounter === 15.U) {
      state := idle
    }
    bCounter := bCounter + 1.U
  }

  // Buffer for storing intermediate results
  val intermediateResult = RegInit(0.U(4.W))

  // Logic for performing sparse matrix multiplication
  when(state === processing) {
    intermediateResult := 0.U
    for (i <- 0 until 16) {
      when(io.inACol(i) === bCounter) {
        intermediateResult := intermediateResult + (io.inAData(i) * bBuffer(i))
      }
    }
  }

  // Output C data
  io.outCData := intermediateResult

  // Output valid signal
  io.outCValid := state === processing && bCounter === 15.U
}

class SpMMTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "perform sparse matrix multiplication correctly" in {
    test(new SpMM) { dut =>
      // Test inputs
      val lhsRowIdx = Seq(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7).map(_.U)
      val lhsCol = Seq(0, 1, 0, 1, 2, 3, 2, 3, 4, 5, 4, 5, 6, 7, 6, 7).map(_.U)
      val lhsData = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16).map(_.U)
      val rhsReset = true.B
      val rhsData = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16).map(_.U)

      // Set inputs
      dut.io.inARowIdx.zip(lhsRowIdx).foreach { case (a, b) => a.poke(b) }
      dut.io.inACol.zip(lhsCol).foreach { case (a, b) => a.poke(b) }
      dut.io.inAData.zip(lhsData).foreach { case (a, b) => a.poke(b) }
      dut.io.inBRhsReset.poke(rhsReset)
      dut.io.inBRhsData.zip(rhsData).foreach { case (a, b) => a.poke(b) }

      // Wait for output valid
      while (!dut.io.outCValid.peek().litToBoolean) {
        dut.clock.step()
      }

      // Check output
      dut.io.outCData.expect(Seq(30, 70, 110, 150, 190, 230, 270, 310, 350, 390, 430, 470, 510, 550, 590, 630).map(_.U))
    }
  }
}
