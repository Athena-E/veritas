# Type Error Examples

This directory contains example programs that demonstrate various type errors caught by the Veritas type checker. Each file is designed to trigger a specific error code.

## Error Codes

| File | Error Code | Description |
|------|------------|-------------|
| 01_type_mismatch.veri | E001 | Type mismatch (bool assigned to int) |
| 02_undefined_variable.veri | E002 | Undefined variable |
| 03_undefined_function.veri | E003 | Undefined function |
| 04_assign_immutable.veri | E004 | Assignment to immutable variable |
| 05_return_type_mismatch.veri | E001/E005 | Return type mismatch |
| 06_missing_return.veri | E006 | Missing return value |
| 07_not_an_array.veri | E007 | Indexing non-array type |
| 08_wrong_argument_count.veri | E009 | Wrong number of arguments |
| 09_argument_type_mismatch.veri | E001 | Argument type mismatch |
| 10_invalid_operation.veri | E001 | Invalid arithmetic operation |
| 11_bool_arithmetic.veri | E001 | Arithmetic on booleans |
| 12_non_bool_condition.veri | E001 | Non-boolean condition |
| 13_array_index_type.veri | E001 | Non-integer array index |
| 14_array_element_mismatch.veri | E001 | Array element type mismatch |
| 15_logical_on_int.veri | E001 | Logical operators on integers |
| 16_not_on_int.veri | E001 | Negation on integer |
| 17_for_loop_non_int.veri | E001 | Non-integer loop bounds |
| 18_multiple_errors.veri | Various | Multiple errors (first reported) |

## Running Examples

To test an error example:

```bash
cargo run -- src/examples/errors/01_type_mismatch.veri
```

Each example will produce a pretty-printed error with source highlighting.
