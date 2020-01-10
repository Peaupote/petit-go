package main
import "fmt"

func main() {
	x := 1 + 1 + 1 + 1
	x = x + 1

	if true && false {
		fmt.Print(x, "\n")
	}
}
