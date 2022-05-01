package main

import "fmt"

func main() {
	mult_as_var := mult
	var div_as_var func(int, int) int
	div_as_var = div
	var a = compute(add, 5, 6)
	var s = compute(sub, -7, 16)
	fmt.Println(a, s, compute(mult_as_var, 7, 3), compute(div_as_var, 32, 1))
}

func add(a int, b int) int {
	return a + b
}

func sub(a int, b int) int {
	return a - b
}

func mult(a int, b int) int {
	return a * b
}

func div(a int, b int) int {
	return a / b
}

func compute(operation func(int, int) int, v1 int, v2 int) int {
	return operation(v1, v2)
}
