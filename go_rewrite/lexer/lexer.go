package lexer

import (
	tok "meow/token"
	"strings"
	"unicode"
)

type Lexer struct {
	Input    string
	Position int
}

func (l *Lexer) copy() *Lexer {
	l1 := Lexer{l.Input, l.Position}
	return &l1
}

func (l *Lexer) readChar() *Lexer {
	if len(l.Input) > int(l.Position) {
		l.Position++
	}
	return l
}

func (l *Lexer) char() byte {
	if l.Position >= len(l.Input) {
		return '\000'
	}
	return l.Input[l.Position]
}

func (l *Lexer) peekChar() byte {
	if l.Position+1 >= len(l.Input) {
		return '\000'
	}
	return l.Input[l.Position+1]
}

func (l *Lexer) skipWhitespace() {
	for l.char() == ' ' || l.char() == '\n' || l.char() == '\r' || l.char() == '\t' {
		l.readChar()
	}
}

func (l *Lexer) readInt() tok.Token {
	var buf strings.Builder
	for l.char() != '\000' && unicode.IsDigit(rune(l.char())) {
		buf.WriteByte(l.char())
		l.readChar()
	}
	return tok.Token{Token: tok.INTEGER, Literal: buf.String()}
}

func isLetter(c byte) bool {
	if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_' {
		return true
	}
	return false
}

func lookupIdent(literal string) tok.Token {
	switch literal {
	case "fn":
		return tok.Token{Token: tok.FUNCTION, Literal: literal}
	case "let":
		return tok.Token{Token: tok.LET, Literal: literal}
	case "true":
		return tok.Token{Token: tok.TRUE, Literal: literal}
	case "false":
		return tok.Token{Token: tok.FALSE, Literal: literal}
	case "if":
		return tok.Token{Token: tok.IF, Literal: literal}
	case "else":
		return tok.Token{Token: tok.ELSE, Literal: literal}
	case "return":
		return tok.Token{Token: tok.RETURN, Literal: literal}
	default:
		return tok.Token{Token: tok.IDENTIFIER, Literal: literal}
	}
}

func (l *Lexer) readSymbol() string {
	var buf strings.Builder
	for l.char() != '\000' && (isLetter(l.char()) || unicode.IsDigit(rune(l.char()))) {
		buf.WriteByte(l.char())
		l.readChar()
	}
	literal := buf.String()
	return literal
}

func (l *Lexer) Next() tok.Token {
	for l.char() != '\000' {
		l.skipWhitespace()
		switch {
		case l.char() == '=':
			l.readChar()
			if l.char() == '=' {
				l.readChar()
				return tok.Token{Token: tok.EQ, Literal: "=="}
			}
			return tok.Token{Token: tok.ASSIGN, Literal: "="}
		case l.char() == '+':
			l.readChar()
			return tok.Token{Token: tok.PLUS, Literal: "+"}
		case l.char() == '-':
			l.readChar()
			return tok.Token{Token: tok.MINUS, Literal: "-"}
		case l.char() == '*':
			l.readChar()
			return tok.Token{Token: tok.ASTERICS, Literal: "*"}
		case l.char() == '/':
			l.readChar()
			return tok.Token{Token: tok.SLASH, Literal: "/"}
		case l.char() == '<':
			l.readChar()
			return tok.Token{Token: tok.LT, Literal: "<"}
		case l.char() == '>':
			l.readChar()
			return tok.Token{Token: tok.GT, Literal: ">"}
		case l.char() == '!':
			l.readChar()
			return tok.Token{Token: tok.BANG, Literal: "!"}
		case l.char() == ';':
			l.readChar()
			return tok.Token{Token: tok.SEMICOLON, Literal: ";"}
		case l.char() == '(':
			l.readChar()
			return tok.Token{Token: tok.LPAREN, Literal: "("}
		case l.char() == ')':
			l.readChar()
			return tok.Token{Token: tok.RPAREN, Literal: ")"}
		case l.char() == ',':
			l.readChar()
			return tok.Token{Token: tok.COMMA, Literal: ","}
		case l.char() == '{':
			l.readChar()
			return tok.Token{Token: tok.LBRACE, Literal: "{"}
		case l.char() == '}':
			l.readChar()
			return tok.Token{Token: tok.RBRACE, Literal: "}"}
		case unicode.IsDigit(rune(l.char())):
			return l.readInt()
		case isLetter(l.char()):
			symbol := l.readSymbol()
			return lookupIdent(symbol)
		default:
			return tok.Token{Token: tok.ILLEGAL, Literal: ""}
		}
	}
	return tok.Token{Token: tok.EOF, Literal: ""}
}
