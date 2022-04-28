package main

import "fmt"

func main() {
	fmt.Println(factorial(5))
}

func factorial(n int) int {
	var res int
	if n == 0 {
		res = 1
	} else {
		res = n * factorial(n-1)
	}
	return res
}
