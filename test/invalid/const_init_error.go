package main

func main() {
	const a = real(7 + 2i) // This const initialization is valid because "real" is a builtin function and all arguments are constants
	const b = add(1, 2)    // Invalid const initialization because the call of a user-defined function cannot be used as a const initializer
}

func add(a int, b int) int {
	return a + b
}
