package main
import "fmt"
import "some_pkg"

type X struct {
	left, right *X
	v int
}

type Y struct {}

func add(x int, y int) int { return x + y }
func swap(x, y int) (int, int) { return y, x }

func main() {
	var a, b = swap(1, 2)
	var l = new(X)
	x := &l.v
}
