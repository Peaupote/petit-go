package main
import "fmt"

type T struct {
	x, y int
}

func main() {
	var s *T
	fmt.Print(s, "\n")
	s = new(T)
	s.x = 1
	s.y = 1
	fmt.Print(s, "\n")
}
