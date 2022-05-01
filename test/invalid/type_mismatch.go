package main

import "fmt"

func main() {
	var e = "hey"
	fmt.Println(square(e))
}

func square(a int) int {
	return a * a
}
