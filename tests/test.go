package main
import "fmt"

func main() {
	var y, z = 2, 3
	x := 1

	fmt.Print("x ", x, "\n")
	fmt.Print("y ", y, "\n")
	fmt.Print("z ", z, "\n")

	cond := !false
	fmt.Print("cond ", cond, "\n")
	if cond {
		fmt.Print("yay\n")
	} else {
		fmt.Print("ohh\n")
	}
}
