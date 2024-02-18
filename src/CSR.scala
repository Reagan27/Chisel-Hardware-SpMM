import chisel3._

class CSR(rows: Int, cols: Int) extends Module {
  val io = IO(new Bundle {
    // Input: CSR data (rowPtr, colInd, data)
    val rowPtr = Input(Vec(rows + 1, UInt(log2Ceil(cols).W)))
    val colInd = Input(Vec(cols, UInt(log2Ceil(rows).W)))
    val data = Input(Vec(cols, UInt(8.W)))

    // Input: matrix dimensions
    val numRows = Input(UInt(4.W))
    val numCols = Input(UInt(4.W))

    // Output: retrieved data
    val out = Output(Vec(cols, UInt(8.W)))
  })

  // CSR storage
  val storage = Mem(rows * cols, UInt(8.W))

  // Write data to CSR storage
  for (i <- 0 until rows) {
    for (j <- io.rowPtr(i) until io.rowPtr(i + 1)) {
      storage(j * cols + io.colInd(j)) := io.data(j)
    }
  }

  // Read data from CSR storage
  for (i <- 0 until cols) {
    io.out(i) := storage(i)
  }
}
