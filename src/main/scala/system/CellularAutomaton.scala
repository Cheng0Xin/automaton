package system

import java.util.UUID

trait CellularAutomaton {
  def encode()
  def iteration()
}

case class Cell(uuid: String)
case class Relation(src: Cell, dst: Cell)

object CellBuilder {
  def build() = Cell(UUID.randomUUID.toString)
}



object Rule {

  def apply[A](f: A => A, v: A, n: Int): A = {
    if (n > 1) f(apply(f, v, n - 1)) else f(v)
  }

  val rule: Relation => List[Relation] = e => {
    val mid = CellBuilder.build
    List(Relation(e.dst, mid), Relation(mid, e.src))
  }
}
