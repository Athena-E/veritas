| Kernel | Veritas ELF (bytes) | Veritas code-bearing (bytes) | GCC -O3 ELF (bytes) | GCC -O3 `.text` (bytes) | checked C -O3 ELF (bytes) | checked C -O3 `.text` (bytes) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `floyd_warshall_500` | 1775 | 1775 | 16128 | 793 | 16136 | 793 |
| `jacobi_1d_400_100` | 1420 | 1420 | 16000 | 1001 | 16008 | 1001 |
| `jacobi_2d_250_100` | 2416 | 2416 | 16000 | 1513 | 16008 | 1513 |
| `seidel_2d_400_100` | 2337 | 2337 | 15968 | 1161 | 15976 | 1161 |
| `mvt_400` | 2166 | 2166 | 16072 | 1209 | 16080 | 1209 |
| `atax_390_410` | 2045 | 2045 | 16040 | 1385 | 16048 | 1385 |
| `gesummv_250` | 1886 | 1886 | 16040 | 1449 | 16048 | 1449 |

Notes:
- For Veritas, the code-bearing size falls back to total ELF size because the freestanding binaries do not carry a conventional section table.
- GCC and checked-C `.text` sizes are taken from `objdump -h`.
