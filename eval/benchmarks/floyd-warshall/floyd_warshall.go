package main

import (
	"fmt"
	"os"
	"time"
)

const N = 1024

func initArray(path [][]int64) {
	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			path[i][j] = int64((i+1)*(j+1)) / int64(N)
		}
	}
}

func kernel(path [][]int64) {
	for k := 0; k < N; k++ {
		for i := 0; i < N; i++ {
			for j := 0; j < N; j++ {
				via := path[i][k] + path[k][j]
				if via < path[i][j] {
					path[i][j] = via
				}
			}
		}
	}
}

func checksum(path [][]int64) int64 {
	var s int64
	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			s += path[i][j]
		}
	}
	return s
}

func main() {
	path := make([][]int64, N)
	for i := range path {
		path[i] = make([]int64, N)
	}
	initArray(path)

	t0 := time.Now()
	kernel(path)
	elapsed := time.Since(t0).Seconds()

	fmt.Fprintf(os.Stderr, "checksum=%d\n", checksum(path))
	fmt.Printf("%.6f\n", elapsed)
}
