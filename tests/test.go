package main
import "fmt"
import "some_pkg"

type X struct {
	left, right *X
	v bool
}

type Y struct {}

func main(x X, y int) {
	x.left
	for x.v {
		y = y + 2
	}
	return x + y
}
