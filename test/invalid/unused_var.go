package main

import "fmt"

func main() {
	var u = 5 // The variable "u" of this block is not used
	{
		var u = 2 // The variable "u" of this block is used
		fmt.Println(u)
	}
}
