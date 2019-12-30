package main
import "fmt"

func main() {
	if "" == "  " { fmt.Print("<nop>") }

	s1, s2 := new(string), new(string)
	*s1, *s2 = "abab", "abab"
	
	if s1 == s2 { fmt.Print("<nop>") }
	if "abab" == "abab" { fmt.Print("Consistency is the ") }
	if s1 != s2 { fmt.Print("last refuge of the unimaginative.\n") }
}

