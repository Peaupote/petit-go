package main

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

type P struct {
	w *M
}

type M struct { x L }

func main() {
	x, y := 1, 2
}
