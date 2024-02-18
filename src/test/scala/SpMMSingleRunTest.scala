package spmm

import chisel3._
import chiseltest._
import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import spmm.SpMMTest.addScore

import scala.collection.mutable
import scala.util.Random

case class LhsInput(lhsRowIdx: Seq[Int], lhsCol: Seq[Int], lhsData: Seq[Int]) {
  def * (rhs: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    for(k <- 0 until 16) yield {
      for(i <- 0 until 16) yield {
        val start = if(i > 0) lhsRowIdx(i - 1) + 1 else 0
        val it = for(jp <- start to lhsRowIdx(i))
          yield ((lhsData(jp) * rhs(k)(lhsCol(jp))) >> 4)
        val sum = it.sum & 0xff
        if (sum >= 128) sum - 256 else sum
      }
    }
  }
}
case class SpMMOutput(out: Seq[Seq[Seq[Int]]])
case class SpMMInput(lhsInput: Seq[LhsInput], rhs: Seq[Seq[Int]])

object DataGen {
  def eyeLhs = LhsInput(
    lhsRowIdx = Seq.tabulate(16)(i => i),
    lhsCol = Seq.tabulate(16)(i => i),
    lhsData = Seq.fill(16)(16)
  )
  def emptyLhs = LhsInput(
    lhsRowIdx = Seq.fill(16)(0),
    lhsCol = Seq(0),
    lhsData = Seq(16)
  )
  def fullLhs(seed: Int) = {
    val rnd = new Random(seed)
    LhsInput(
      lhsRowIdx = Seq.tabulate(16)(i => i * 16 + 15),
      lhsCol = Seq.tabulate(256)(i => i % 16),
      lhsData = Seq.fill(256)(rnd.nextInt(256) - 128)
    )
  }
  def randPerm(rnd: Random, max: Int): Seq[Int] = {
    val ch = mutable.ArrayBuffer.tabulate(max)(i => i)
    for(i <- 0 until max - 1) {
      val j = i + rnd.nextInt(max - i)
      val (di, dj) = (ch(i), ch(j))
      ch(i) = dj
      ch(j) = di
    }
    ch.toSeq
  }
  def simpleLhs(seed: Int) = {
    val rnd = new Random(seed)
    val rowIdx = mutable.ArrayBuffer[Int]()
    val col = mutable.ArrayBuffer[Int]()
    val data  = mutable.ArrayBuffer[Int]()
    var rowIdxAcc = -1
    for(i <- 0 until 16) {
      val rowCount = if(i == 0) rnd.nextInt(15) + 1 else rnd.nextInt(16)
      rowIdxAcc += rowCount
      rowIdx += rowIdxAcc
      col  ++= randPerm(rnd, 16).take(rowCount).sorted // Seq.fill(rowCount)(rnd.nextInt(16)).sorted
      data ++= Seq.fill(rowCount)(16)
    }
    LhsInput(lhsRowIdx = rowIdx.toSeq, lhsCol=col.toSeq, lhsData=data.toSeq)
  }
  def rndLhs(seed: Int, rowMin: Int = 0, rowMax: Int = 16) = {
    val rnd = new Random(seed)
    val rowIdx = mutable.ArrayBuffer[Int]()
    val col = mutable.ArrayBuffer[Int]()
    val data  = mutable.ArrayBuffer[Int]()
    var rowIdxAcc = -1
    for(i <- 0 until 16) {
      val mn = if (i == 0 && rowMin == 0) 1 else rowMin
      val rowCount = rnd.nextInt(rowMax - mn) + mn
      rowIdxAcc += rowCount
      rowIdx += rowIdxAcc
      col  ++= randPerm(rnd, 16).take(rowCount).sorted // Seq.fill(rowCount)(rnd.nextInt(16)).sorted
      data ++= Seq.fill(rowCount)(rnd.nextInt(256) - 128)
    }
    LhsInput(lhsRowIdx = rowIdx.toSeq, lhsCol=col.toSeq, lhsData=data.toSeq)
  }
  def rndRhs(seed: Int) = {
    val rnd = new Random(seed)
    Seq.fill(16)(Seq.fill(16)(rnd.nextInt(256) - 128))
  }
  def simpleRhs = Seq.tabulate(16)(i=>Seq.fill(16)(i))
}

object SpMMTest {
  private var score = 0.0
  def addScore(n: Double): Double = synchronized {
    score += n
    score
  }
}

trait SpMMTest extends AnyFlatSpec
  with ChiselScalatestTester
//  with ParallelTestExecution // 并行测试加速
  {
  def testDriver(dut: SpMM, rnd: Random, inputs: Seq[SpMMInput]): Int = {
    var clock = 0
    fork {
      for(input <- inputs) {
        for((lhs, idx) <- input.lhsInput.zipWithIndex) {
          while(!dut.io.inputReady.peek().litToBoolean) dut.clock.step()
          dut.clock.step()
          dut.io.lhsRowIdx zip lhs.lhsRowIdx foreach { case (l, r) => l.poke(r.U) }
          dut.io.start.poke(true)
          dut.io.rhsReset.poke(idx == 0)
          def inputData(): Unit = {
            for(i <- 0 until 16) {
              for(j <- 0 until 16) {
                if(i * 16 + j < lhs.lhsCol.length) {
                  dut.io.lhsCol(j).poke(lhs.lhsCol(i * 16 + j))
                  dut.io.lhsData(j).data.poke(lhs.lhsData(i * 16 + j))
                } else {
                  dut.io.lhsCol(j).poke(rnd.nextInt(256))
                  dut.io.lhsData(j).data.poke(rnd.nextInt(256) - 128)
                }
                if(idx == 0) {
                  dut.io.rhsData(j).data.poke(input.rhs(i)(j))
                } else {
                  dut.io.rhsData(j).data.poke(rnd.nextInt(256) - 128)
                }
              }
              if(dut.io.inputReady.peek().litToBoolean) return
              dut.clock.step()
              dut.io.start.poke(false)
            }
          }
          inputData()
        }
      }
    } .fork {
      for(input <- inputs) {
        for(lhs <- input.lhsInput) {
          val output = lhs * input.rhs
          for(i <- 0 until 16) {
            while(!dut.io.outValid.peek().litToBoolean) {
              clock += 1
              dut.clock.step()
            }
            dut.io.outData zip output(i) foreach {
              case(l, r) => l.data.expect(r.S)
            }
            clock += 1
            dut.clock.step()
          }
        }
      }
    } .join()
    clock
  }
  def testItOn(tag: String, scoreFn: (Int, Int, Int) => Double, inputs: Seq[SpMMInput]): Unit = {
    val counter = F44.counter.get()
    counter.clear()
    test(new SpMM)
      .withAnnotations(Seq(
        WriteVcdAnnotation,         // 输出的波形在 test_run_dir 里
        VerilatorBackendAnnotation  // 使用 Verilator 会评测地更快
      )) { dut =>
        val add = counter("add")
        val mul = counter("mul")
        val clock = testDriver(dut, new Random(0), inputs)
        val score = scoreFn(add, mul, clock)
        val totalScore = addScore(score)
        println(f"test=$tag add=$add mul=$mul clock=$clock%5d score=$score total_score=$totalScore%.2f")
      }
  }
  def testItOn(tag: String, score: Double, inputs: Seq[SpMMInput]): Unit = {
    testItOn(tag, (a, m, c) => score, inputs)
  }
  def resourceScoreWeight(add: Int, mul: Int): Double = {
    if (mul > 16) {
      0.40
    } else {
      add match {
        case a if a <= 20 => 1.00
        case a if a <= 35 => 0.90
        case a if a <= 68 => 0.80
        case _ => 0.70
      }
    }
  }
  def testItOn(tag: String, score: Double, inputs: Seq[SpMMInput], scoreList: Seq[(Int, Double)]): Unit = {
    val scoreFn: (Int, Int, Int) => Double = (add, mul, clock) => {
      var ratio = scoreList.collectFirst { case (c, r) if clock <= c => r }.getOrElse(0.0)
      ratio = Math.min(ratio * resourceScoreWeight(add, mul), 1.0)
      ratio * score
    }
    testItOn(tag, scoreFn, inputs)
  }
}

class SpMMSingleRunTest extends SpMMTest {
  behavior of "SpMM on single lhs and single rhs"
  import DataGen._
  it should "work on eye * simple" in
    testItOn("t1.1", 2, Seq(SpMMInput(lhsInput=Seq(eyeLhs), rhs=simpleRhs)))
  it should "work on eye * random" in
    testItOn("t1.2", 3, Seq(SpMMInput(lhsInput=Seq(eyeLhs), rhs=rndRhs(0))))
  it should "work on empty * simple" in
    testItOn("t1.3", 3, Seq(SpMMInput(lhsInput=Seq(emptyLhs), rhs=simpleRhs)))
  it should "work on empty * random" in
    testItOn("t1.4", 3, Seq(SpMMInput(lhsInput=Seq(emptyLhs), rhs=rndRhs(0))))
  it should "work on simple * simple" in
    testItOn("t1.5", 3, Seq(SpMMInput(lhsInput=Seq(simpleLhs(0)), rhs=simpleRhs)))
  it should "work on full * simple" in
    testItOn("t1.6", 3, Seq(SpMMInput(lhsInput=Seq(fullLhs(0)), rhs=simpleRhs)))
  it should "work on random * random" in
    testItOn("t1.7", 3, Seq(SpMMInput(lhsInput=Seq(rndLhs(0)), rhs=rndRhs(0))))
}

class SpMMStreamingSingleRunTest extends SpMMTest {
  behavior of "SpMM on streaming single lhs and single rhs"
  import DataGen._
  it should "work on eye * simple" in
    testItOn("t2.1", 2, Seq.fill(4)(SpMMInput(lhsInput=Seq(eyeLhs), rhs=simpleRhs)))
  it should "work on eye * random" in
    testItOn("t2.2", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq(eyeLhs), rhs=rndRhs(i))))
  it should "work on empty * simple" in
    testItOn("t2.3", 2, Seq.fill(4)(SpMMInput(lhsInput=Seq(emptyLhs), rhs=simpleRhs)))
  it should "work on empty * random" in
    testItOn("t2.4", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq(emptyLhs), rhs=rndRhs(i))))
  it should "work on simple * simple" in
    testItOn("t2.5", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq(simpleLhs(i)), rhs=simpleRhs)))
  it should "work on full * simple" in
    testItOn("t2.6", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq(fullLhs(i)), rhs=simpleRhs)))
  it should "work on random * random" in
    testItOn("t2.7", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq(rndLhs(i)), rhs=rndRhs(i))))
}

class SpMMStreamingLhsTest extends SpMMTest {
  behavior of "SpMM on streaming lhs and single rhs"
  import DataGen._
  it should "work on streaming lhs eye" in
    testItOn("t3.1", 2, Seq(SpMMInput(lhsInput=Seq(eyeLhs, eyeLhs), rhs=simpleRhs)))
  it should "work on streaming lhs eye and empty" in
    testItOn("t3.2", 2, Seq(SpMMInput(lhsInput=Seq(eyeLhs, emptyLhs), rhs=simpleRhs)))
  it should "work on streaming lhs empty" in
    testItOn("t3.3", 2, Seq(SpMMInput(lhsInput=Seq(emptyLhs, emptyLhs), rhs=simpleRhs)))
  it should "work on streaming lhs simple" in
    testItOn("t3.4", 2, Seq(SpMMInput(lhsInput=Seq(simpleLhs(0), simpleLhs(1)), rhs=simpleRhs)))
  it should "work on streaming lhs multiple simple" in
    testItOn("t3.5", 2, Seq(SpMMInput(lhsInput=Seq.tabulate(4)(i => simpleLhs(i)), rhs=simpleRhs)))
  it should "work on streaming lhs random" in
    testItOn("t3.6", 2, Seq(SpMMInput(lhsInput=Seq.tabulate(4)(i => rndLhs(i)), rhs=rndRhs(0))))
}

class SpMMStreamingTest extends SpMMTest {
  behavior of "SpMM on streaming single lhs and single rhs"
  import DataGen._
  it should "work on streaming eye" in
    testItOn("t4.1", 2, Seq.fill(4)(SpMMInput(lhsInput=Seq(eyeLhs, eyeLhs), rhs=simpleRhs)))
  it should "work on streaming eye and empty" in
    testItOn("t4.2", 2, Seq.fill(4)(SpMMInput(lhsInput=Seq(eyeLhs, emptyLhs), rhs=simpleRhs)))
  it should "work on streaming empty" in
    testItOn("t4.3", 2, Seq.fill(4)(SpMMInput(lhsInput=Seq(emptyLhs, emptyLhs), rhs=simpleRhs)))
  it should "work on streaming simple" in
    testItOn("t4.4", 2, Seq.fill(4)(SpMMInput(lhsInput=Seq(simpleLhs(0), simpleLhs(1)), rhs=simpleRhs)))
  it should "work on streaming multiple simple" in
    testItOn("t4.5", 2, Seq.fill(4)(SpMMInput(lhsInput=Seq.tabulate(4)(i => simpleLhs(i)), rhs=simpleRhs)))
  it should "work on streaming random" in
    testItOn("t4.6", 2, Seq.tabulate(4)(t=>SpMMInput(lhsInput=Seq.tabulate(4)(i => rndLhs(i)), rhs=rndRhs(t))))
  it should "work on streaming full" in
    testItOn("t4.7", 2, Seq.tabulate(4)(t=>SpMMInput(lhsInput=Seq.tabulate(4)(i => fullLhs(i)), rhs=rndRhs(t))))
}

class SpMMPerformanceTest extends SpMMTest {
  behavior of "SpMM performance"
  import DataGen._
  it should "have good performance on 0-4" in
    testItOn("p1.1", 2, Seq(SpMMInput(lhsInput=Seq(rndLhs(0, rowMin=0, rowMax=4)), rhs=rndRhs(0))),
      Seq.tabulate(10)(i => (50 + i * 40, 1 - i * 0.1)))
  it should "have good performance on 4-8" in
    testItOn("p1.2", 2, Seq(SpMMInput(lhsInput=Seq(rndLhs(0, rowMin=4, rowMax=8)), rhs=rndRhs(0))),
      Seq.tabulate(10)(i => (120 + i * 60, 1 - i * 0.1)))
  it should "have good performance on 9" in
    testItOn("p1.3", 2, Seq(SpMMInput(lhsInput=Seq(rndLhs(0, rowMin=9, rowMax=10)), rhs=rndRhs(0))),
      Seq.tabulate(10)(i => (160 + i * 80, 1 - i * 0.1)))
  it should "have good performance on 9-12" in
    testItOn("p1.4", 2, Seq(SpMMInput(lhsInput=Seq(rndLhs(0, rowMin=9, rowMax=12)), rhs=rndRhs(0))),
      Seq.tabulate(10)(i => (190 + i * 80, 1 - i * 0.1)))
  it should "have good performance on 12-16" in
    testItOn("p1.5", 2, Seq(SpMMInput(lhsInput=Seq(rndLhs(0, rowMin=12, rowMax=16)), rhs=rndRhs(0))),
      Seq.tabulate(10)(i => (250 + i * 80, 1 - i * 0.1)))

  it should "have good performance on multiple input 0-4" in
    testItOn("p2.1", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq(rndLhs(i, rowMin=0, rowMax=4)), rhs=rndRhs(i))),
      Seq.tabulate(10)(i => (160 + i * 80, 1 - i * 0.1)))
  it should "have good performance on multiple input 4-8" in
    testItOn("p2.2", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq(rndLhs(i, rowMin=4, rowMax=8)), rhs=rndRhs(i))),
      Seq.tabulate(10)(i => (420 + i * 120, 1 - i * 0.1)))
  it should "have good performance on multiple input 9" in
    testItOn("p2.3", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq(rndLhs(i, rowMin=9, rowMax=10)), rhs=rndRhs(i))),
      Seq.tabulate(10)(i => (600 + i * 200, 1 - i * 0.1)))
  it should "have good performance on multiple input 9-12" in
    testItOn("p2.4", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq(rndLhs(i, rowMin=9, rowMax=12)), rhs=rndRhs(i))),
      Seq.tabulate(10)(i => (720 + i * 200, 1 - i * 0.1)))
  it should "have good performance on multiple input 12-16" in
    testItOn("p2.5", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq(rndLhs(i, rowMin=12, rowMax=16)), rhs=rndRhs(i))),
      Seq.tabulate(10)(i => (930 + i * 200, 1 - i * 0.1)))

  it should "have good performance on streaming lhs 0-4" in
    testItOn("p3.1", 2, Seq(SpMMInput(lhsInput=Seq.tabulate(4)(i=>rndLhs(i, rowMin=0, rowMax=4)), rhs=rndRhs(0))),
      Seq.tabulate(10)(i => (160 + i * 80, 1 - i * 0.1)))
  it should "have good performance on streaming lhs 4-8" in
    testItOn("p3.2", 2, Seq(SpMMInput(lhsInput=Seq.tabulate(4)(i=>rndLhs(i, rowMin=4, rowMax=8)), rhs=rndRhs(0))),
      Seq.tabulate(10)(i => (420 + i * 120, 1 - i * 0.1)))
  it should "have good performance on streaming lhs 9" in
    testItOn("p3.3", 2, Seq(SpMMInput(lhsInput=Seq.tabulate(4)(i=>rndLhs(i, rowMin=9, rowMax=10)), rhs=rndRhs(0))),
      Seq.tabulate(10)(i => (600 + i * 200, 1 - i * 0.1)))
  it should "have good performance on streaming lhs 9-12" in
    testItOn("p3.4", 2, Seq(SpMMInput(lhsInput=Seq.tabulate(4)(i=>rndLhs(i, rowMin=9, rowMax=12)), rhs=rndRhs(0))),
      Seq.tabulate(10)(i => (720 + i * 200, 1 - i * 0.1)))
  it should "have good performance on streaming lhs 12-16" in
    testItOn("p3.5", 2, Seq(SpMMInput(lhsInput=Seq.tabulate(4)(i=>rndLhs(i, rowMin=12, rowMax=16)), rhs=rndRhs(0))),
      Seq.tabulate(10)(i => (930 + i * 200, 1 - i * 0.1)))

  it should "have good performance on general input 0-4" in
    testItOn("p4.1", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq.tabulate(4)(i=>rndLhs(i, rowMin=0, rowMax=4)), rhs=rndRhs(i))),
      Seq.tabulate(10)(i => (550 + i * 200, 1 - i * 0.1)))
  it should "have good performance on general input 4-8" in
    testItOn("p4.2", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq.tabulate(4)(i=>rndLhs(i, rowMin=4, rowMax=8)), rhs=rndRhs(i))),
      Seq.tabulate(10)(i => (1580 + i * 700, 1 - i * 0.1)))
  it should "have good performance on general input 9" in
    testItOn("p4.3", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq.tabulate(4)(i=>rndLhs(i, rowMin=9, rowMax=10)), rhs=rndRhs(i))),
      Seq.tabulate(10)(i => (2350 + i * 1200, 1 - i * 0.1)))
  it should "have good performance on general input 9-12" in
    testItOn("p4.4", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq.tabulate(4)(i=>rndLhs(i, rowMin=9, rowMax=12)), rhs=rndRhs(i))),
      Seq.tabulate(10)(i => (2800 + i * 1200, 1 - i * 0.1)))
  it should "have good performance on general input 12-16" in
    testItOn("p4.5", 2, Seq.tabulate(4)(i=>SpMMInput(lhsInput=Seq.tabulate(4)(i=>rndLhs(i, rowMin=12, rowMax=16)), rhs=rndRhs(i))),
      Seq.tabulate(10)(i => (3650 + i * 1600, 1 - i * 0.1)))
}
