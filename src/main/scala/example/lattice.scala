package example

trait lattice[t] {
  val bottom: t
  def <=(a: t, b: t): Boolean
  def lub(a: t, b: t): t
  def glb(a: t, b: t): t
}
