package main

import (
	"crypto/hmac"
	"crypto/sha1"
	"encoding/hex"
	"fmt"
	"os"
	"path"
	"strconv"

	"golang.org/x/crypto/pbkdf2"
)

func main() {
    switch path.Base(os.Args[0]) {
    case "hmac":
        runHmac()
    case "pbkdf2":
        runPbkdf2()
    default:
        println("Bad program name")
        os.Exit(1)
    }
}

func runHmac() {
    if len(os.Args) != 3 {
        fmt.Printf("Usage: %s <data> <key>\n", path.Base(os.Args[0]))
        os.Exit(1)
    }

    data := []byte(os.Args[1])
    key := []byte(os.Args[2])

    fmt.Fprintf(os.Stderr, "data: %+v\n", data)
    fmt.Fprintf(os.Stderr, "key: %+v\n", key)

    mac := hmac.New(sha1.New, key)
    digest := mac.Sum(data)

    fmt.Fprintf(os.Stderr, "output: %+v\n", digest)
    println(hex.EncodeToString(digest))
}

func runPbkdf2() {
    if len(os.Args) != 5 {
        fmt.Printf("Usage: %s <password> <salt> <iterations> <length>\n", path.Base(os.Args[0]))
        os.Exit(1)
    }

    password := []byte(os.Args[1])
    salt := []byte(os.Args[2])
    iterations, _ := strconv.Atoi(os.Args[3])
    length, _ := strconv.Atoi(os.Args[4])

    dk := pbkdf2.Key(password, salt, iterations, length, sha1.New)
    fmt.Printf("%+v\n", dk);
}
