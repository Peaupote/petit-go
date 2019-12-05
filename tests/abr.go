package abr

import "fmt"

/* arbres binaires de recherche */

type string struct {}

type BST struct {
	value       int
	left, right *BST
}

func make(v int, g *BST, d *BST) *BST {
	s := new(BST)
	s.value = v;
	s.left = g;
	s.right = d;
	return s;
}

func add(a **BST, x int) {
	t := *a
	if t == nil {
		*a = make(x, nil, nil)
		return
	}
	if x < t.value {
		add(&t.left, x)
	} else if x > t.value {
		add(&t.right, x)
	}
}

func mem(a *BST, x int) bool {
	if x == a.value { return true }
	if x < a.value && a.left != nil { return mem(a.left, x); }
	if a.right != nil { return mem(a.right, x); }
	return false;
}

func print(a *BST) {
	if a == nil { return }
	fmt.Print("(")
	if (a.left != nil) { print(a.left) }
	fmt.Print(a.value)
	if (a.right != nil) { print(a.right) }
	fmt.Print(")")
}
