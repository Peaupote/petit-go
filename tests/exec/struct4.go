package main
import "fmt"

type L struct { x, y, z int; t T }
type T struct { x, y int }

func ppL(l L) { fmt.Print(l.x, l.y, l.z, " "); pp(l.t) }
func pp(t T) { fmt.Print(t.x, t.y, "\n") }
func f(t T) T { t.x = t.x + 2; t.y = t.y + 2; return t }

func g(x int) (int, int) { return x, x + 1 }

func main() {
	var t1, t2 T
	t1.x = 1
	t1.y = 2
	t2.x = 3
	t2.y = 4
	
	pp(t1) // 1 2
	pp(t2) // 3 4

	t2 = f(t1)
	pp(t1) // 1 2
	pp(t2) // 3 4

	var l L
	l.x, l.y, l.z, l.t = 1, 2, 3, f(t2)

	pp(t1) // 1 2
	pp(t2) // 3 4
	ppL(l) // 1 2 3 5 6
}

