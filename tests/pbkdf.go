// https://datatracker.ietf.org/doc/html/rfc2898
// go build -C tests -o $PWD/pbkdf pbkdf.go
package main

import (
	"crypto/sha1"
	"fmt"
	"os"
	"strconv"

	"golang.org/x/crypto/pbkdf2"
)

func main() {
    if len(os.Args) != 5 {
        fmt.Printf("Usage: %s <password> <salt> <iterations> <length>\n", os.Args[0])
        os.Exit(1)
    }

    password := []byte(os.Args[1])
    salt := []byte(os.Args[2])
    iterations, _ := strconv.Atoi(os.Args[3])
    length, _ := strconv.Atoi(os.Args[4])

    dk := pbkdf2.Key(password, salt, iterations, length, sha1.New)
    fmt.Printf("%+v\n", dk);
}
