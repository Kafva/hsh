package main

import (
	"bufio"
	"crypto/hmac"
	"crypto/sha1"
	"crypto/sha256"
	"encoding/binary"
	"encoding/hex"
	"flag"
	"fmt"
	"hash"
	"math/bits"
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

    // runSalsa()

    // 4*64 = 256 bytes 
    // * 8 (r) bytes per block
    // = 2048 bytes
    // intin := make([]uint32, 64*8) 
    // for i := 0; i < len(intin); i++ {
    //     intin[i] = uint32(i)
    // }
    // dumpWord32Array("intin", intin)
    // log.Printf("INTEGERIFY: %+v\n", integerify(intin, 8, 1024));

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
    //dumpWordArray("output", dk)
    writeResult(dk)
}

func runSalsa() {
    var tmp [16]uint32
    in := make([]uint32, 16)
    out := make([]uint32, 16)

    for i := 0; i < 16; i++ {
        val := []byte{ byte(i), 0x0, 0x0, 0x0}
        in[i] = binary.LittleEndian.Uint32(val)
        out[i] = uint32(0)
        tmp[i] = uint32(0)
    }

    salsaXOR(&tmp, in, out)
    dumpWord32Array("salsa in", in)
    dumpWord32Array("salsa out", out)
}

func writeResult(result []byte) {
    hexstr := hex.EncodeToString(result)
    f := bufio.NewWriter(os.Stdout)
    f.WriteString(hexstr)
    f.Flush() // Make sure to flush the stream
    println()
}

////////////////////////////////////////////////////////////////////////////////

//
// golang.org/x/crypto@v0.28.0/scrypt/scrypt.go
//
// salsaXOR applies Salsa20/8 to the XOR of 16 numbers from tmp and in,
// and puts the result into both tmp and out.
func salsaXOR(tmp *[16]uint32, in, out []uint32) {
	w0 := tmp[0] ^ in[0]
	w1 := tmp[1] ^ in[1]
	w2 := tmp[2] ^ in[2]
	w3 := tmp[3] ^ in[3]
	w4 := tmp[4] ^ in[4]
	w5 := tmp[5] ^ in[5]
	w6 := tmp[6] ^ in[6]
	w7 := tmp[7] ^ in[7]
	w8 := tmp[8] ^ in[8]
	w9 := tmp[9] ^ in[9]
	w10 := tmp[10] ^ in[10]
	w11 := tmp[11] ^ in[11]
	w12 := tmp[12] ^ in[12]
	w13 := tmp[13] ^ in[13]
	w14 := tmp[14] ^ in[14]
	w15 := tmp[15] ^ in[15]

	x0, x1, x2, x3, x4, x5, x6, x7, x8 := w0, w1, w2, w3, w4, w5, w6, w7, w8
	x9, x10, x11, x12, x13, x14, x15 := w9, w10, w11, w12, w13, w14, w15

	for i := 0; i < 8; i += 2 {
		x4 ^= bits.RotateLeft32(x0+x12, 7)
		x8 ^= bits.RotateLeft32(x4+x0, 9)
		x12 ^= bits.RotateLeft32(x8+x4, 13)
		x0 ^= bits.RotateLeft32(x12+x8, 18)

		x9 ^= bits.RotateLeft32(x5+x1, 7)
		x13 ^= bits.RotateLeft32(x9+x5, 9)
		x1 ^= bits.RotateLeft32(x13+x9, 13)
		x5 ^= bits.RotateLeft32(x1+x13, 18)

		x14 ^= bits.RotateLeft32(x10+x6, 7)
		x2 ^= bits.RotateLeft32(x14+x10, 9)
		x6 ^= bits.RotateLeft32(x2+x14, 13)
		x10 ^= bits.RotateLeft32(x6+x2, 18)

		x3 ^= bits.RotateLeft32(x15+x11, 7)
		x7 ^= bits.RotateLeft32(x3+x15, 9)
		x11 ^= bits.RotateLeft32(x7+x3, 13)
		x15 ^= bits.RotateLeft32(x11+x7, 18)

		x1 ^= bits.RotateLeft32(x0+x3, 7)
		x2 ^= bits.RotateLeft32(x1+x0, 9)
		x3 ^= bits.RotateLeft32(x2+x1, 13)
		x0 ^= bits.RotateLeft32(x3+x2, 18)

		x6 ^= bits.RotateLeft32(x5+x4, 7)
		x7 ^= bits.RotateLeft32(x6+x5, 9)
		x4 ^= bits.RotateLeft32(x7+x6, 13)
		x5 ^= bits.RotateLeft32(x4+x7, 18)

		x11 ^= bits.RotateLeft32(x10+x9, 7)
		x8 ^= bits.RotateLeft32(x11+x10, 9)
		x9 ^= bits.RotateLeft32(x8+x11, 13)
		x10 ^= bits.RotateLeft32(x9+x8, 18)

		x12 ^= bits.RotateLeft32(x15+x14, 7)
		x13 ^= bits.RotateLeft32(x12+x15, 9)
		x14 ^= bits.RotateLeft32(x13+x12, 13)
		x15 ^= bits.RotateLeft32(x14+x13, 18)
	}
	x0 += w0
	x1 += w1
	x2 += w2
	x3 += w3
	x4 += w4
	x5 += w5
	x6 += w6
	x7 += w7
	x8 += w8
	x9 += w9
	x10 += w10
	x11 += w11
	x12 += w12
	x13 += w13
	x14 += w14
	x15 += w15

	out[0], tmp[0] = x0, x0
	out[1], tmp[1] = x1, x1
	out[2], tmp[2] = x2, x2
	out[3], tmp[3] = x3, x3
	out[4], tmp[4] = x4, x4
	out[5], tmp[5] = x5, x5
	out[6], tmp[6] = x6, x6
	out[7], tmp[7] = x7, x7
	out[8], tmp[8] = x8, x8
	out[9], tmp[9] = x9, x9
	out[10], tmp[10] = x10, x10
	out[11], tmp[11] = x11, x11
	out[12], tmp[12] = x12, x12
	out[13], tmp[13] = x13, x13
	out[14], tmp[14] = x14, x14
	out[15], tmp[15] = x15, x15
}

func integerify(b []uint32, r int, N int) int {
	j := (2*r - 1) * 16
    x := uint64(b[j]) | uint64(b[j+1])<<32
    return int(x & uint64(N-1))
}

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

func dumpWord32Array(label string, arr []uint32) {
    logDebug("%s [%d byte(s)]: [ ", label, len(arr)*4)
    for i := range arr {
        logDebug("0x%x ", arr[i])
    }
    logDebug("]\n")
}

func dumpWordArray(label string, arr []byte) {
    logDebug("%s [%d byte(s)]: [ ", label, len(arr))
    for i := range arr {
        logDebug("0x%x ", arr[i])
    }
    logDebug("]\n")
}
