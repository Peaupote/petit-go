package main

type L struct {
	x    int
	next *L
}

func test(x *L) (*L, int) {
	return x.next.next
}

func main() {
	return
}
