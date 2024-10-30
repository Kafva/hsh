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

func dumpWordArray(label string, arr []byte) {
    fmt.Fprintf(os.Stderr, "%s: [", label)
    for i := range arr {
        fmt.Fprintf(os.Stderr, "0x%x ", arr[i])
    }
    fmt.Fprintf(os.Stderr, "]\n")
}

func runHmac() {
    if len(os.Args) != 3 {
        fmt.Printf("Usage: %s <message> <key>\n", path.Base(os.Args[0]))
        os.Exit(1)
    }

    message, err := os.ReadFile(os.Args[1])
    if err != nil {
        fmt.Printf("Error reading: '%s'\n", os.Args[1])
        return
    }

    key, err := os.ReadFile(os.Args[2])
    if err != nil {
        fmt.Printf("Error reading: '%s'\n", os.Args[2])
        return
    }

    dumpWordArray("key", key)
    dumpWordArray("message", message)

    mac := hmac.New(sha1.New, key)
    digest := mac.Sum(message)

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
