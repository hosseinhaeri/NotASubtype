package lops

//9.6.1.4

trait LazyExp[+This <: LazyExp[This]] extends BaseExp with Serializable with Product {
  this: This =>
  
  type OpSemExp <: LazyExp[OpSemExp] with This
  type Val <: AbsVal[This] with This
  type App <: BaseApp[This] with This
  
  def incarnator: BaseExp => OpSemExp
  def apply(x: Idn): App =
    incarnator(new BaseApp[This](this, x)).asInstanceOf[App]  
  def < (p: (Idn, BaseExp)): LazyExpGreaterThanWaiter[This] =
    LazyExpGreaterThanWaiter[This](this, p._1, p._2)
}

case class LazyExpGreaterThanWaiter[+Exp <: LazyExp[Exp]](e: Exp, y: Idn, be: BaseExp) {
  def > : Exp = e.incarnator(e.subst(y, be))
}

//*************************************************************************
