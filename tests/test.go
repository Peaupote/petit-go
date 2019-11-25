package main
import "fmt"
import "some_pkg"

type X struct {
	left, right *X
	v bool
}

type Y struct {}

func add(x int, y int) int { return x + y }
func swap(x, y int) (int, int) { return y, x }

func main(x X, y int) {
	x.left
	for x.v {
		y = add(y, y)
	}
}
