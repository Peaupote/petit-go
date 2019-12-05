package main
import "fmt"
import "abr"

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
	var dico *abr.BST = nil
	for i := 1; i < 10; i++ {
		x := (55 * i) % 34
		abr.add(&dico, x)
		abr.print(dico)
		fmt.Print("\n")
	}
	if abr.mem(dico, 8) && !abr.mem(dico, 0) &&
		abr.mem(dico, 32) && !abr.mem(dico, 22) {
	   fmt.Print("ok\n");
	}
	abr.add(&dico, 42);
	abr.add(&dico, -1);
	abr.print(dico); fmt.Print("\n")

	a := new(abr.string)
	a = nil
}
