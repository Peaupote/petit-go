package main
import "fmt"

type L struct { t1, t2 T }
type T struct { x, y int; z int }

func f() (int, int, int, int, int) { return 1, 2, 3, 4, 5 }
func g(x int) (int, int, int) { return x, x + 1, x + 2 }
func h(t T) T { t.x++; t.y++; t.z++; return t }

func main() {
	fmt.Print(g(1))
	fmt.Print("\n")
	
	fmt.Print(f())
	fmt.Print("\n")

	var t T
	t.x, t.y, t.z = 1, 2, 3
	fmt.Print(t, "\n")
	fmt.Print(t, t, "\n")

	var l L
	l.t1 = t
	l.t2 = h(t)
	fmt.Print(l.t1, l.t2, "\n")
	fmt.Print(l, "\n")
}

