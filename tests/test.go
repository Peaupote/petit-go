package main
import "fmt"

type L struct {
	x1    int
	x2    int
	x3    int
	x4    int
	x5    int
	x6    int
	x7    int
	x8    int
	next P
}

type P struct {}

type List struct { x *List; v int }

func main() {
	fmt.Print(1)
	fmt.Print("\n")
	fmt.Print(1)
	fmt.Print("\n")
}
