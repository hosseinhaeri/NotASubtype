//9.3.12.2

package object lops {

  import shapeless._
  import Typeable._
  
  type Idn = String
  implicit def idn2IdnWithSlash(i: Idn): IdnWithSlash = IdnWithSlash(i)

  def fv[Exp <: LazyExp[_]](e: Exp): Set[Idn] = e.fv
  def bv[Exp <: LazyExp[_]](e: Exp): Set[Idn] = e.bv
  def subst[Exp <: LazyExp[_]](e: Exp, arg: Idn, repl: Exp): e.OpSemExp = {
    val temp: BaseExp = e.asInstanceOf[BaseExp].subst(arg, repl.asInstanceOf[BaseExp])
    e.incarnator(temp)
  }
  
  private[lops] def flatten[T](ss: Set[Set[T]]): Set[T] = (Set[T]() /: ss) (_ ++ _)
  
}