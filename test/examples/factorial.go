package main

import "fmt"

func main() {
	var fac = factorial(5)
	fmt.Println(fac)
}

func factorial(n int) int {
	if n == 0 {
		return 1
	} else {
		return n * factorial(n-1)
	}
}
