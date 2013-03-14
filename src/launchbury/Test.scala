package launchbury

import lops._

object tester {
  val id = Lam("x", Var("x"))
  val bs = Map("x" -> Var("x"), "y" -> Var("y"))
  val bll = new BaseLetLam[LaunchExp](bs, id)
}