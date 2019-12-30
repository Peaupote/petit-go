package main
import "fmt"

type T struct { x, y int }

func main() {
	var t1, t2 T
	if t1 == t2 { fmt.Print("I'm sorry") }

	t1.y = 42
	if t1 == t2 { fmt.Print("<nop>") }

	t2.y = 42
	if t1 == t2 { fmt.Print(" Dave. ") }

	t2.x = 42
	if t1 != t2 { fmt.Print("I'm afraid I can't do that.\n") }
}

