package main

import (
	"bufio"
	"crypto/hmac"
	"crypto/sha1"
	"crypto/sha256"
	"encoding/hex"
	"flag"
	"fmt"
	"hash"
	"os"
	"path"
	"strconv"

	"golang.org/x/crypto/pbkdf2"
	"golang.org/x/crypto/scrypt"
)

var DEBUG = false
var INNER_ALG = sha1.New

func main() {
    debug := flag.Bool("d", false, "Enable debug logging")
    innerAlgorithm := flag.String("H", "", "Inner hash algorithm [default: sha1]")
    flag.Parse()
    args := flag.Args()
    DEBUG = *debug
    INNER_ALG = stringToAlgorithm(*innerAlgorithm)

    switch path.Base(os.Args[0]) {
    case "hmac":
        runHmac(args)
    case "pbkdf2":
        runPbkdf2(args)
    case "scrypt":
        runScrypt(args)
    default:
        println("Bad program name")
        os.Exit(1)
    }
}

func runHmac(args []string) {
    if len(args) != 2 {
        fmt.Printf("Usage: %s <message> <key>\n", path.Base(os.Args[0]))
        os.Exit(1)
    }

    message, ok := loadFromFile(args[0])
    if !ok {
        return
    }

    key, ok := loadFromFile(args[1])
    if !ok {
        return
    }

    dumpWordArray("message", message)
    dumpWordArray("key", key)

    mac := hmac.New(INNER_ALG, key)
    mac.Write(message)
    digest := mac.Sum(nil)

    dumpWordArray("output", digest)

    writeResult(digest)
}

func runPbkdf2(args []string) {
    if len(args) != 4 {
        fmt.Printf("Usage: %s <password> <salt> <iterations> <length>\n", path.Base(os.Args[0]))
        os.Exit(1)
    }

    password, ok := loadFromFile(args[0])
    if !ok {
        return
    }
    dumpWordArray("password", password)

    salt, ok := loadFromFile(args[1])
    if !ok {
        return
    }
    dumpWordArray("salt", salt)

    iterations, _ := strconv.Atoi(args[2])
    length, _ := strconv.Atoi(args[3])

    dk := pbkdf2.Key(password, salt, iterations, length, INNER_ALG)
    dumpWordArray("output", dk)
    writeResult(dk)
}

func runScrypt(args []string) {
    if len(args) != 6 {
        fmt.Printf("Usage: %s <password> <salt> <memory-cost> <blocksize> <parallelisation> <length>\n", path.Base(os.Args[0]))
        os.Exit(1)
    }

    password, ok := loadFromFile(args[0])
    if !ok {
        return
    }
    dumpWordArray("password", password)

    salt, ok := loadFromFile(args[1])
    if !ok {
        return
    }
    dumpWordArray("salt", salt)

    memoryCost, _ := strconv.Atoi(args[2])
    blockSize, _ := strconv.Atoi(args[3])
    parallelisationParameter, _ := strconv.Atoi(args[4])
    keyLength, _ := strconv.Atoi(args[5])

    dk, err := scrypt.Key(password, salt, memoryCost, blockSize, parallelisationParameter, keyLength)
    if err != nil {
        fmt.Printf("Error in scrypt: '%s'\n", err.Error())
        return
    }
    dumpWordArray("output", dk)
    writeResult(dk)
}

func writeResult(result []byte) {
    hexstr := hex.EncodeToString(result)
    f := bufio.NewWriter(os.Stdout)
    f.WriteString(hexstr)
    f.Flush() // Make sure to flush the stream
    println()
}

////////////////////////////////////////////////////////////////////////////////

func loadFromFile(path string) ([]byte, bool) {
    out, err := os.ReadFile(path)
    if err != nil {
        fmt.Printf("Error reading: '%s'\n", path)
        return nil, false
    }
    return out, true
}

func logDebug(fmtstr string, args ...any) {
    if DEBUG {
        fmt.Fprintf(os.Stderr, fmtstr, args...)
    }
}

func stringToAlgorithm(name string) func() hash.Hash {
    if (name == "sha256") {
        return sha256.New
    } else {
        return sha1.New
    }
}

func dumpWordArray(label string, arr []byte) {
    logDebug("%s [%d byte(s)]: [ ", label, len(arr))
    for i := range arr {
        logDebug("0x%x ", arr[i])
    }
    logDebug("]\n")
}

