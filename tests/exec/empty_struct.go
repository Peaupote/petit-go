package main
import "fmt"

type S struct {}

func f(s S) { fmt.Print(s, "\n") }
func g() S  { var s S; return s }

func main() {
	var s S
	x := s
	fmt.Print(x, "\n")
	f(x)

	y := g()
	f(y)
	
	f(g())
}

