package main

import "fmt"

type L struct {
	x    int
	next *L
}

func test(x *L) *L {
	return x.next.next
}

func main() {
	var x, y *L = nil, new(L)
	var w *L = x.next
}
