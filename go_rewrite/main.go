package main

import (
	"fmt"
	"meow/lexer"
	"meow/token"
)

func main() {
	l := lexer.Lexer{Input: "let var10 = 10; if(var10==10){15}else{20}", Position: 0}
	fmt.Println("lexing:", l.Input)
	for {
		tok := l.Next()
		if tok.Token == token.EOF {
			break
		}
		fmt.Println(tok)
	}
}
