package launchbury

import lops._
import shapeless._
import Typeable._

sealed trait LaunchExp extends LazyExp[LaunchExp] with BaseExp {
  type OpSemExp = LaunchExp
  type Val = launchbury.Lam
  type App = launchbury.App
  type Var = launchbury.Var
  type Let = launchbury.Let
    
  override def incarnator: BaseExp => LaunchExp = _ match {
    case v: BaseVar => Var(v.name)
    case l: BaseLam[_] => {
      val mlo = l.cast[BaseLam[LaunchExp]]
      if(mlo.isDefined)
        launchbury.Lam(l.x, mlo.get.e)
      else
        throw new IllegalArgumentException("Casting BaseLam[T] to LaunchExp where T != LaunchExp")
    }
    case a: BaseApp[_] => {
      val mao = a.cast[BaseApp[LaunchExp]]
      if(mao.isDefined)
        App(mao.get.e, a.x)
      else
        throw new IllegalArgumentException("Casting BaseApp[T] to LaunchExp where T != LaunchExp")
    }
    case l: BaseLetLam[_] => {
      val mlo = l.cast[BaseLetLam[LaunchExp]]
      if(mlo.isDefined)
        Let(mlo.get.bs, mlo.get.e)
      else
        throw new IllegalArgumentException("Casting BaseLet[T] to LaunchExp where T != LaunchExp")
    }
    case _ =>
      throw new IllegalArgumentException("Casting Unsupported BaseExp to LaunchExp")
  }
}

final case class Var(override val x: Idn) extends BaseVar(x) with LaunchExp
final case class Lam(override val x: Idn, override val e: LaunchExp)
  extends BaseLam[LaunchExp](x, e) with LaunchExp
final case class App(override val e: LaunchExp, override val x: Idn) 
  extends BaseApp[LaunchExp](e, x) with LaunchExp
final case class Let(override val bs: Map[Idn, LaunchExp], override val e: LaunchExp)
  extends BaseLetLam[LaunchExp](bs, e) with LaunchExp