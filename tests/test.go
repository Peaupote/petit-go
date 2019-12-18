package main
import "fmt"

func step(a, b int) (int, int) {
	return b, a+b
}

func main() {
	x, y := step(step(1,2));
	fmt.Print(x, y, "\n");
}
