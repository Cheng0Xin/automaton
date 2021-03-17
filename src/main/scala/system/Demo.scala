package system

import scala.collection.immutable.HashMap


case class Element(id: Boolean)
case class Preliminary(t0: Element, t1: Element, t2: Element)

class Demo(iterNum: Int) {

  val mlInitialState: List[Element] = {
    val aTState = Array.fill(2 * iterNum + 1){Element(false)}
    aTState(iterNum) = Element(true)
    aTState.toList
  }

  private val mmRuleList: HashMap[String, (Preliminary => Element)] = HashMap(
    "Rule30" -> rule30,
    "Rule250" -> rule250,
    "Rule254" -> rule254
  )

  def iteration(funcName: String) {
    // Initial state of CA
    showLine(mlInitialState)

    // Iteration
    var lCurrentState = mlInitialState
    for (i <- 0 until iterNum) {
      val lPreState = stateToPreliminary(lCurrentState)
      lCurrentState = lPreState.map(mmRuleList(funcName))
      showLine(lCurrentState)
    }
  }

  def stateToPreliminary(state: List[Element]): List[Preliminary] = {
    val extendedState = List.concat(List[Element](Element(false)), state, List[Element](Element(false)))
    val result = {
      for (i <- 0 until (extendedState.length - 2)) 
      yield Preliminary(extendedState(i), extendedState(i + 1), extendedState(i + 2))
    }
    result.toList
  }

  private def showLine(state: List[Element]) {
    state.foreach({ case Element(x) =>
      if(x) print("#") else print("~")
    })
    print("\n")
  }

  def rule254(pre: Preliminary): Element = pre match {
    case Preliminary(Element(true), Element(true), Element(true)) => Element(true)
    case Preliminary(Element(true), Element(true), Element(false)) => Element(true)
    case Preliminary(Element(true), Element(false), Element(true)) => Element(true)
    case Preliminary(Element(true), Element(false), Element(false)) => Element(true)
    case Preliminary(Element(false), Element(true), Element(true)) => Element(true)
    case Preliminary(Element(false), Element(true), Element(false)) => Element(true)
    case Preliminary(Element(false), Element(false), Element(true)) => Element(true)
    case Preliminary(Element(false), Element(false), Element(false)) => Element(false)
  }

  def rule250(pre: Preliminary): Element = pre match {
    case Preliminary(Element(true), Element(true), Element(true)) => Element(true)
    case Preliminary(Element(true), Element(true), Element(false)) => Element(true)
    case Preliminary(Element(true), Element(false), Element(true)) => Element(true)
    case Preliminary(Element(true), Element(false), Element(false)) => Element(true)
    case Preliminary(Element(false), Element(true), Element(true)) => Element(true)
    case Preliminary(Element(false), Element(true), Element(false)) => Element(false)
    case Preliminary(Element(false), Element(false), Element(true)) => Element(true)
    case Preliminary(Element(false), Element(false), Element(false)) => Element(false)
  }

  def rule30(pre: Preliminary): Element = pre match {
    case Preliminary(Element(true), Element(true), Element(true)) => Element(false)
    case Preliminary(Element(true), Element(true), Element(false)) => Element(false)
    case Preliminary(Element(true), Element(false), Element(true)) => Element(false)
    case Preliminary(Element(true), Element(false), Element(false)) => Element(true)
    case Preliminary(Element(false), Element(true), Element(true)) => Element(true)
    case Preliminary(Element(false), Element(true), Element(false)) => Element(true)
    case Preliminary(Element(false), Element(false), Element(true)) => Element(true)
    case Preliminary(Element(false), Element(false), Element(false)) => Element(false)
  }
}