signature a2_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val TERM:  'a * 'a -> (svalue,'a) token
val ELSE: (string) *  'a * 'a -> (svalue,'a) token
val THEN: (string) *  'a * 'a -> (svalue,'a) token
val IF: (string) *  'a * 'a -> (svalue,'a) token
val IMPLIES: (string) *  'a * 'a -> (svalue,'a) token
val EQUALS: (string) *  'a * 'a -> (svalue,'a) token
val XOR: (string) *  'a * 'a -> (svalue,'a) token
val OR: (string) *  'a * 'a -> (svalue,'a) token
val AND: (string) *  'a * 'a -> (svalue,'a) token
val NOT: (string) *  'a * 'a -> (svalue,'a) token
val CONST: (string) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
end
signature a2_LRVALS=
sig
structure Tokens : a2_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
