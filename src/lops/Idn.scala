package lops

//9.0.0.1

case class IdnWithSlash(y: Idn) {
  def / (x: Idn): (Idn, BaseExp) = (new BaseVar(y)) / x
}