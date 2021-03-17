package system

import scala.collection.mutable.ArrayBuffer

abstract class StateSpace

case class ContinuousState(time: Double, x: Double, dx: Double, ddx: Double) extends StateSpace
case class DiscreteState(time: Double, a: Int) extends StateSpace

abstract class HybridAutomaton {
  private val mClocks: ArrayBuffer[ContinuousState] = ArrayBuffer.empty
  private val mVariables: ArrayBuffer[DiscreteState] = ArrayBuffer.empty
  private var mTimeStep: Double = 1e-3
  private var mDebug: Boolean = true

  def initialization(clocks: ContinuousState, variables: DiscreteState): HybridAutomaton = {
    clocks match {
      case ContinuousState(time, _, _, _) => if (time != 0) throw new Exception("TODO: Test")
    }

    variables match {
      case DiscreteState(time, _) => if (time != 0) throw new Exception("TODO: Test")
    }

    mClocks.clear
    mVariables.clear
    
    mClocks.append(clocks)
    mVariables.append(variables)

    this
  }

  def setTimeStep(timeStep: Double): HybridAutomaton = {
    mTimeStep = timeStep
    this
  }

  def setDebugFlag(flag: Boolean): HybridAutomaton = {
    mDebug = flag
    this
  }

  def evolution(timeDuration: Double): HybridAutomaton = {

    val tCount:Int = (timeDuration / mTimeStep).toInt
    for (elem <- 1 to tCount) {
      doContinuousStep()
    }
    this
  }

  def setContinuousVariables(clock: ContinuousState): Unit = {
    mClocks.append(clock)
  }
  
  def setDiscreteVariables(variable: DiscreteState): Unit = {
    mVariables.append(variable)
  }

  def getContinuousVariables: ContinuousState = mClocks.last

  def getDiscreteVariables: DiscreteState = mVariables.last

  private def doContinuousStep(): Unit= {
    val last = getContinuousVariables
    val disState = getDiscreteVariables

    def compute(x: Double, dx: Double): Double = x + dx * mTimeStep

    val current = ContinuousState(
      compute(last.time, 1),
      compute(last.x, last.dx),
      compute(last.dx, last.ddx),
      compute(last.ddx, disState.a.toDouble)
    )

    setContinuousVariables(current)
    printContinuousUpdate()

    if (stateEvents) {
      discreteStateUpdate()
      printDiscreteUpdate()
    }
  }

  def printContinuousUpdate(): Unit = {
    val cs = getContinuousVariables
    if (mDebug) println(s"Time ${cs.time}s: x = ${cs.x}; dx = ${cs.dx}; ddx = ${cs.ddx};")
  }

  def printDiscreteUpdate(): Unit = {
    val ds = getDiscreteVariables
    if (mDebug) println(s"Time ${ds.time}s: a = ${ds.a};")
  }

  def stateEvents: Boolean

  def discreteStateUpdate(): Unit
}

class DemoAutomaton extends HybridAutomaton {
  override def stateEvents: Boolean = {
    getContinuousVariables match {
      case ContinuousState(time, x, dx, dxx) => if (x > 10 || x < -20) true else false
    }
  }

  override def discreteStateUpdate(): Unit = {
    if (getContinuousVariables.x > 10)
      setDiscreteVariables(DiscreteState(getContinuousVariables.time, -20))
    else
      setDiscreteVariables(DiscreteState(getContinuousVariables.time, 20))
  }
}