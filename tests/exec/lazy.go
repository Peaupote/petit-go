package main

func main() {
	if true || 0 == 1/0 {}
	if false && 0 == 1/0 {}
}
