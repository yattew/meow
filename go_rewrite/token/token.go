package token

const (
	ILLEGAL    = "ILLEGAL"
	EOF        = "EOF"
	IDENTIFIER = "IDENTIFIER"
	INTEGER    = "INTEGER"
	COMMA      = "COMMA"
	SEMICOLON  = "SEMICOLON"
	LPAREN     = "LPAREN"
	RPAREN     = "RPAREN"
	LBRACE     = "LBRACE"
	RBRACE     = "RBRACE"
	//keywords
	FUNCTION = "FUNCTION"
	LET      = "LET"
	TRUE     = "TRUE"
	FALSE    = "FALSE"
	IF       = "IF"
	ELSE     = "ELSE"
	RETURN   = "RETURN"
	//operators
	ASSIGN   = "ASSIGN"
	PLUS     = "PLUS"
	MINUS    = "MINUS"
	ASTERICS = "ASTERICS"
	SLASH    = "SLASH"
	LT       = "LT"
	GT       = "GT"
	BANG     = "BANG"
	EQ       = "EQ"
	NOT_EQ   = "NOT_EQ"
)

type Token struct {
	Token   string
	Literal string
}
