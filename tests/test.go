package main
import "fmt"
import "abr"

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
}

