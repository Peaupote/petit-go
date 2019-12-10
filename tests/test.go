package main
import "fmt"

type PGF2 struct { a0, a1 bool }
type gauss struct { r, i int }

func main() {
	var one, i, b gauss
	one.r = 1
	one.i = 0

	i.r = 0
	i.i = 1

	b.r = i.r + one.r
	b.i = i.i + one.i

	if i.r == 0 {
		fmt.Print("i is pure imaginary\n")
	}

	if one.r != 0 {
		fmt.Print("one is not pure imaginary\n")
	}

	fmt.Print("b: (", b.r, ", ", b.i, ")\n")
}
