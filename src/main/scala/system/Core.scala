package system

object Core {
  // Cellular Automaton Demo
  def cellularAutomatonTest(): Unit = {
    val ca = new Demo(iterNum = 100)
    ca.iteration("Rule30")
    println("Self-organized system !")
  }

  def hybridAutomatonTest(): Unit = {
    val ha = new DemoAutomaton
    ha.initialization(ContinuousState(0.0, 0.0, 0.0, 3.0), DiscreteState(0.0, 0))
      .setTimeStep(1e-2)
      .setDebugFlag(true)
      .evolution(5)
  }

  def main(args: Array[String]): Unit = {
    // cellularAutomatonTest()
    hybridAutomatonTest()
  }

}
