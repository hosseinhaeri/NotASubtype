package lops

//9.6.37.28

import shapeless._
import Typeable._
import TypeOperators._

trait BaseExp {
  def fv: Set[Idn]
  def bv: Set[Idn]
  
  def subst(arg: Idn, repl: BaseExp): BaseExp
  
  def / (x: Idn): (Idn, BaseExp) = (x, this)
}

//*************************************************************************

class BaseVar(val x: Idn) extends BaseExp {
  override def toString() = x
  val name: Idn = x
  
  override def fv = Set(x)
  override def bv = Set()
  override def subst(arg: Idn, repl: BaseExp): BaseExp =
    if(arg == x) repl else this
}

//*************************************************************************

class BaseApp[+Exp <: LazyExp[Exp]](val e: Exp, val x: Idn) extends BaseExp {
  override def toString(): String = e match {
    case y: BaseVar => y + " " + x
    case _ => "(" + e + ") " + x
  }
  
  override def fv = e.fv + x
  override def bv = e.bv
  override def subst(arg: Idn, repl: BaseExp): BaseExp = repl match {
    case v: BaseVar if x == arg => 
      new BaseApp[Exp](e<(repl / arg)>, v.name)
    case _ if x == arg =>
  	  throw new IllegalArgumentException("Attempting to replace " + x + " in " + this +
        		  						 " with non-variable expression " + repl)
    case _ =>
      new BaseApp[Exp](e<(repl / arg)>, x)
  }
}

//*************************************************************************

trait AbsVal[+Exp <: LazyExp[Exp]] {
  def ^-\ (y: Idn): Exp
}

//*************************************************************************

class BaseLam[+Exp <: LazyExp[Exp]{type Val <: BaseLam[Exp] with Exp}](val x: Idn,
															  		   val e: Exp) extends AbsVal[Exp] with BaseExp {
  require(x != "", {println("unnamed variable bound in '" + this + "'")})
  require(!e.bv.contains(x),
		  {println("rebinding variable " + x + " in " + e)})
  override def toString() = "\\" + x + "." + e
  
  override def fv = e.fv - x
  override def bv = e.bv + x
  override def subst(arg:Idn, repl: BaseExp): BaseExp =
    if(e.bv.contains(arg) || !(e.bv & repl.fv).isEmpty)  this
    else new BaseLam[Exp](x, e<(repl / arg)>)

  private final def incompatible[X <: BaseLam[_] with BaseVal[_]] = {}

  override def ^-\ (y: Idn): Exp = {
    val repl = new BaseVar(y)
    e<(repl / x)>
  }
}

object BaseLam {
  def apply[Exp <: LazyExp[Exp]{type Val <: BaseLam[Exp] with Exp}](x: Idn, e: Exp): BaseLam[Exp] =
    new BaseLam[Exp](x, e)
}

//*************************************************************************

class BaseVal[+Exp <: LazyExp[Exp]{type Val <: BaseVal[Exp] with Exp
								   type Let <: BaseLetVal[Exp] with Exp}](val bs: Map[Idn, Exp],
															  		   	  val x: Idn,
															  		   	  val e: Exp) extends AbsVal[Exp] with BaseExp {
  
  require(x != "", {println("unnamed variable bound in " + this)})
  require(!e.bv.contains(x),
		  {println("rebinding variable " + x + " in " + e)})

  def this(bs: Map[Idn, Exp], x: Idn, e: BaseVal[Exp]) = this(bs ++ e.bs, x, e.e)
  def ++ [
          MoreExp >: Exp <: LazyExp[MoreExp]{type Val <: BaseVal[MoreExp] with MoreExp
          									 type Let <: BaseLetVal[MoreExp] with MoreExp}
         ](ebs: Map[Idn, MoreExp]): BaseVal[MoreExp] = {
	  require((bs.keySet & ebs.keySet).isEmpty,
		      "Augmenting " + this + " with existing bindings in " + ebs)
	  new BaseVal[MoreExp](bs ++ ebs, x, e)
  }
  
  override def fv =
    e.fv ++ flatten((for((_, ei) <- bs) yield ei.fv).toSet) -- bs.keySet - x
  override def bv =
    e.bv + x ++ bs.keySet ++ flatten((for((_, ei) <- bs) yield ei.bv).toSet)
  override def subst(arg: Idn, repl: BaseExp): BaseExp =
    if(e.bv.contains(arg) || !(e.bv & repl.fv).isEmpty) this
    else new BaseVal[Exp](for((xi, ei) <- bs) yield (xi, ei<(repl / arg)>), x, e<(repl / arg)>)

  override def toString(): String = {
	var temp = ""
	if(!bs.isEmpty) {
	  temp += "let {"
      val comma = ", "
      for((xi, ei) <- bs) temp += xi + " = " + ei + comma
      temp = temp.substring(0, temp.length() - comma.length()) + "} in "
    }
    temp + "\\" + x + "." + e
  }

  def canEqual(other: Any): Boolean = other.cast[BaseVal[Exp]].isDefined
  override def equals(other: Any): Boolean = other match {
    case that: BaseVal[_] => (that canEqual this) && (bs == that.bs) && (e == that.e) && (x == that.x)
    case _ => false
  }
  override def hashCode: Int = 41 * (41 * (41 + bs.hashCode) + x.hashCode) + e.hashCode

  private final def incompatible[X <: BaseLam[_] with BaseVal[_]] = {}

  override def ^-\ (y: Idn): Exp = {
    val repl = new BaseVar(y)
    val raw_ret = e match {
      case rv: BaseVal[_] => {
        val v = rv.cast[BaseVal[Exp]].get
        BaseVal(bs ++ v.bs, v.x, v.e).subst(x, repl)
      }
      case _ if bs.isEmpty =>
        e subst(x, repl)
      case _ =>
        BaseLetVal(bs, e).subst(x, repl)
    }
    e.incarnator(raw_ret)
  }
}

object BaseVal {
  def apply[
            Exp <: LazyExp[Exp]{type Val <: BaseVal[Exp] with Exp
								type Let <: BaseLetVal[Exp] with Exp}
           ](x: Idn, e: Exp): BaseVal[Exp] = new BaseVal[Exp](Map[Idn, Exp](), x, e)
  def apply[
            Exp <: LazyExp[Exp]{type Val <: BaseVal[Exp] with Exp
								type Let <: BaseLetVal[Exp] with Exp}
           ](bs: Map[Idn, Exp], x: Idn, e: Exp): BaseVal[Exp] = new BaseVal[Exp](bs, x, e)
}

//*************************************************************************

abstract class AbsLet[+Exp <: LazyExp[Exp]{type Let}](val bs: Map[Idn, Exp], val e: Exp) extends BaseExp {
  override def fv = e.fv ++ flatten((for((_, ei) <- bs) yield ei.fv).toSet) -- bs.keySet
  override def bv = bs.keySet ++ e.bv ++ flatten((for((_, ei) <- bs) yield ei.bv).toSet)

  protected def subst_pair(arg: Idn, repl: BaseExp): (Map[Idn, Exp], Exp) =
    (for((xi, ei) <- bs) yield (xi, ei<(repl / arg)>), e<(repl / arg)>)
  protected def letBit: String
  protected def inBit: String
  override def toString(): String = {
    var temp = letBit
    if(!bs.isEmpty) {
      val comma = ", "
      for((xi, ei) <- bs) {
        temp += xi + " = "
        if(ei.isInstanceOf[AbsLet[_]])
          temp += "(" + ei + ")"
        else
          temp += ei
        temp += comma
      }
      temp = temp.substring(0, temp.length() - comma.length())
    }
    temp + inBit + e
  }
}

//*************************************************************************

class BaseLetLam[
                 +Exp <: LazyExp[Exp]{type Val <: BaseLam[Exp] with Exp
									  type Let <: BaseLetLam[Exp] with Exp}
                ](override val bs: Map[Idn, Exp], override val e: Exp)
                 (implicit ev: Exp#Val <:!< BaseVal[_]) extends AbsLet[Exp](bs, e) {
  require(!bs.isEmpty, {println("Attempting to instantiate BaseLetLam in " + this + "with no bindings")})
  
  override def subst(arg: Idn, repl: BaseExp): BaseExp = {
	val p = subst_pair(arg, repl)
	new BaseLetLam[Exp](p._1, p._2)
  }
  protected override def letBit = "let "
  protected override def inBit = " in "
}

object BaseLetLam {
  def apply[
            Exp <: LazyExp[Exp]{type Val <: BaseLam[Exp] with Exp
              					type Let <: BaseLetLam[Exp] with Exp}
           ](bs: Map[Idn, Exp], e: Exp): BaseLetLam[Exp] = new BaseLetLam[Exp](bs, e)
}

//*************************************************************************

class BaseLetVal[
                 +Exp <: LazyExp[Exp]{type Val <: BaseVal[Exp] with Exp
									  type Let <: BaseLetVal[Exp] with Exp}
                ](override val bs: Map[Idn, Exp], override val e: Exp)
                 (implicit ev: Exp#Val <:!< BaseLam[_]) extends AbsLet[Exp](bs, e) {
    override def subst(arg: Idn, repl: BaseExp): BaseExp = {
      val p = subst_pair(arg, repl)
      new BaseLetVal[Exp](p._1, p._2)
    }
    protected override def letBit = "let {"
    protected override def inBit = "} in "
}

object BaseLetVal {
  def apply[
            Exp <: LazyExp[Exp]{type Val <: BaseVal[Exp] with Exp
              					type Let <: BaseLetVal[Exp] with Exp}
           ](bs: Map[Idn, Exp], e: Exp): BaseLetVal[Exp] = new BaseLetVal[Exp](bs, e)
}
