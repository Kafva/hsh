// https://datatracker.ietf.org/doc/html/rfc2898
// go build -C rfc -o $PWD/pbkdf pbkdf.go
package main

import (
	"crypto/sha1"
	"fmt"
	"os"

	"golang.org/x/crypto/pbkdf2"
)

func main() {
    if len(os.Args) != 3 {
        fmt.Printf("Usage: %s <password> <salt>\n", os.Args[0])
        os.Exit(1)
    }

    password := []byte(os.Args[1])
    salt := []byte(os.Args[2])

    dk := pbkdf2.Key(password, salt, 4096, 32, sha1.New)
    fmt.Printf("%+v\n", dk);
}
