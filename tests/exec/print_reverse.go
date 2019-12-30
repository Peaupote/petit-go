package main
import "fmt"

type T struct { x, y int }
func n(x int) T { var t T; t.x, t.y = x, x + 1; return t }
func f(x int) (int, T, int) { return x, n(x+1), x+3 }
func h(x int) (int, T, int, int) { return x, n(x+1), x+3, x+4 }
func k(x int) (int, T, int, T) { return x, n(x+1), x+3, n(x+4) }

func main() {
	fmt.Print(f(1))
	fmt.Print("\n")

	fmt.Print(h(1))
	fmt.Print("\n")
	
	fmt.Print(k(1))
	fmt.Print("\n")
}

