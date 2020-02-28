package fr.dcram.nlp4s.ner
import fr.dcram.nlp4s.automata.{AutomatonBuilderDsl, UserTokenMatcher}
import fr.dcram.nlp4s.model.Token

abstract class NerTokenMatcher extends UserTokenMatcher[Token] with AutomatonBuilderDsl[Token]
