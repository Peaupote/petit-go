package main
import "fmt"

type S struct { x, y int }

func main() {
	var x int
	var b bool
	var p S
	var pp = new(S)

	// pp.x = 0
	
	fmt.Print(x, "\n")
	fmt.Print(b, "\n")
	fmt.Print(p.x, "\n")
	fmt.Print(p.y, "\n")
	fmt.Print(pp.x, "\n")
	fmt.Print(pp.y, "\n")
}
