package main
import "fmt"

func side_effect() bool { fmt.Print("oh!\n"); return true }

func main() {
	fmt.Print(true || side_effect(), "\n")
}
