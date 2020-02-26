package fr.dcram.nlp4s.automata

object AutomataTests {
  case class E(c:Char)
  def fixSeq(string:String):Seq[E] = string.toCharArray.toSeq.map(E.apply)
  def matchToString(m:RegexMatch[E]):String = m.tokens.map(_.c).mkString

  object Vowel extends UserTokenMatcher[E] {
    override def toString: String = "V"
    private[this] val values = "aeiouy".toCharArray.toSet
    override def matches(tok: E): Boolean =
      values.contains(tok.c.toLower)
  }
  object Consomn extends UserTokenMatcher[E] {
    override def matches(tok: E): Boolean =
      !Vowel.matches(tok)
  }

  case class Letter(letter:Char) extends UserTokenMatcher[E] {
    override def toString: String = letter.toString
    override def matches(tok: E): Boolean =
      tok.c == letter
  }

  private[this] def letterAutomaton(letter:Char) = matcherAutomaton(Letter(letter))
  private[this] def matcherAutomaton(matcher:TokenMatcher[E]) = new AutomatonBuilder[E]()
    .initialState(1).state(2, accepting = true)
    .matcherTransition(1,2,matcher)
    .build

  // Letters
  val AutConsomn:Automaton[E] = matcherAutomaton(Consomn)
  val AutVowel:Automaton[E] = matcherAutomaton(Vowel)
  val AutA:Automaton[E] = letterAutomaton('a')
  val AutB:Automaton[E] = letterAutomaton('b')
  val AutC:Automaton[E] = letterAutomaton('c')
  val AutD:Automaton[E] = letterAutomaton('d')
  val AutE:Automaton[E] = letterAutomaton('e')

  // Vowel Consomn
  val AutAConsA:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3).state(4, accepting = true)
    .matcherTransition(1,2,Letter('a'))
    .matcherTransition(2,3,Consomn)
    .matcherTransition(3,4,Letter('a'))
    .build

  //
  val ZeroOneA:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2, accepting = true)
    .matcherTransition(1,2,Letter('a'))
    .epsilon(1,2)
    .build

  // Vowel Consomn
  val AutVowCons:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3, accepting = true)
    .matcherTransition(1,2,Vowel)
    .matcherTransition(2,3,Consomn)
    .build

  // Vowel Consomn Consomn?
  val AutVowConsCons:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3, accepting = true).state(4, accepting = true)
    .matcherTransition(1,2,Vowel)
    .matcherTransition(2,3,Consomn)
    .matcherTransition(3,4,Consomn)
    .build


  // Vowel Consomn+
  val AutVowConsPlus:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3, accepting = true)
    .matcherTransition(1,2,Vowel)
    .matcherTransition(2,3,Consomn)
    .matcherTransition(3,3,Consomn)
    .build

  // Vowel (toto: Vowel Consomn) Vowel
  val A4:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3).state(4, accepting = true)
    .transition(1,2,Vowel)
    .transition(2,3, AutVowCons)
    .transition(3,4,Vowel)
    .build

  // Vowel (toto: Vowel Consomn)? Vowel
  val A5:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3).state(4, accepting = true)
    .transition(1,2,Vowel)
    .transition(2,3, AutVowCons)
    .epsilon(2,3)
    .transition(3,4,Vowel)
    .build

  // Vowel (toto: Vowel Consomn)? Vowel
  val A6:Automaton[E] = new AutomatonBuilder[E]()
    .initialState(1).state(2).state(3).state(4, accepting = true)
    .transition(1,2,Vowel)
    .namedTransition("toto", 2,3, AutVowCons)
    .epsilon(2,3)
    .transition(3,4,Vowel)
    .build

  // a(?<toto>(?<tata>bV?c)+d)
  import AutomatonFactory._
  val A7:Automaton[E] = sequence(
    Letter('a'),
    named(
      "toto",
      oneN(named("tata", Letter('b'), zeroOne(Vowel), Letter('c'))),
      Letter('d')
    )
  )


}
