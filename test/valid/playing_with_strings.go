package main

import "fmt"

func main() {
	var s = "hello, " + "mini-go"
	s = s + "!"
	const i int = 2
	const b = true
	var c = 4 + 6i + 3/2 - 2*1i
	fmt.Println(s+"!!", i, b, c)
	fmt.Println(hello_world())
}

func hello_world() string {
	return "hello" + ", " + "world!"
}
