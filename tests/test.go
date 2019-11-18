package main
import "fmt"
import "some_pkg"

type X struct {
	left, right *X
	v Y
}

type Y struct {}

func add (x X, y X) Y {}

func main(x, y int) {
	y = *nil
	y = x + 1
}
