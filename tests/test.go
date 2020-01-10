package main
import "fmt"

type S struct { x, y int }

// func f () *S {
// 	var s S;
// 	s.x, s.y = 4, 2
// 	return &s
// }

func main() {
	var s S
	s.x, s.y = 4, 2
	fmt.Print(&s, "\n")

	// ss := f()
	// fmt.Print(ss, "\n")
}

