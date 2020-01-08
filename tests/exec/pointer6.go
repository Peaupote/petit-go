package main
import "fmt"

func f() *int { x := 42; p := &x; return p }

func main() {
	x := f();
	fmt.Print(*x, "\n")
}
