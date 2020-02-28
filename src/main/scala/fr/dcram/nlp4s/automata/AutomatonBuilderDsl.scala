package fr.dcram.nlp4s.automata

trait AutomatonBuilderDsl[Tok] {
  this:Transitionable[Tok]=>
    def repeat(m:Int, n:Int):Transitionable[Tok] = AutomatonFactory.quantified(this, Quantifiers.MN(m,n))
    def mn(m:Int, n:Int):Transitionable[Tok] = repeat(m,n)
    def *():Transitionable[Tok] = AutomatonFactory.star(this)
    def n(n:Int):Transitionable[Tok] = repeat(n,n)
    def +(n:Int):Transitionable[Tok] = AutomatonFactory.oneN(this)
    def n_inf(n:Int):Transitionable[Tok] = AutomatonFactory.quantified(this, Quantifiers.NStar(n))
    def zeroN(n:Int):Transitionable[Tok] = AutomatonFactory.quantified(this, Quantifiers.ZeroN(n))
    def ?():Transitionable[Tok] = AutomatonFactory.zeroOne(this)
    def opt():Transitionable[Tok] = AutomatonFactory.zeroOne(this)
}
