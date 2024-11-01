package main

import (
    "bufio"
    "crypto/hmac"
    "crypto/sha1"
    "encoding/hex"
    "flag"
    "fmt"
    "os"
    "path"
    "strconv"

    "golang.org/x/crypto/pbkdf2"
)

var DEBUG = false

func main() {
    debug := flag.Bool("d", false, "Enable debug logging")
    flag.Parse()
    args := flag.Args()
    DEBUG = *debug

    switch path.Base(os.Args[0]) {
    case "hmac":
        runHmac(args)
    case "pbkdf2":
        runPbkdf2(args)
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

    message, err := os.ReadFile(args[0])
    if err != nil {
        fmt.Printf("Error reading: '%s'\n", args[0])
        return
    }

    key, err := os.ReadFile(args[1])
    if err != nil {
        fmt.Printf("Error reading: '%s'\n", args[1])
        return
    }

    dumpWordArray("message", message)
    dumpWordArray("key", key)

    mac := hmac.New(sha1.New, key)
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

    password := []byte(args[0])
    salt := []byte(args[1])
    iterations, _ := strconv.Atoi(args[2])
    length, _ := strconv.Atoi(args[3])

    dk := pbkdf2.Key(password, salt, iterations, length, sha1.New)
    fmt.Printf("%+v\n", dk);
}

func writeResult(result []byte) {
    hexstr := hex.EncodeToString(result)
    f := bufio.NewWriter(os.Stdout)
    defer f.Flush() // Make sure to flush the stream
    f.WriteString(hexstr)
}

////////////////////////////////////////////////////////////////////////////////

func logDebug(fmtstr string, args ...any) {
    if DEBUG {
        fmt.Fprintf(os.Stderr, fmtstr, args...)
    }
}

func dumpWordArray(label string, arr []byte) {
    logDebug("%s [%d byte(s)]: [", label, len(arr))
    for i := range arr {
        logDebug("0x%x ", arr[i])
    }
    logDebug("]\n")
}

