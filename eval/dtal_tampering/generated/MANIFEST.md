# Tampered DTAL Error Locations

This manifest records the current tampering sites in the generated `.dtal`
corpus under `eval/dtal_tampering/generated/`.

## T01 `return_signature_mismatch`

- File: `T01_return_signature_mismatch.dtal`
- Expected verifier error: `Return type mismatch`
- Modified location: line 5 in function `main`
- Change: `.returns int` -> `.returns bool`

## T02 `strengthened_precondition_breaks_call_site`

- File: `T02_strengthened_precondition_breaks_call_site.dtal`
- Expected verifier error: `Precondition not provable`
- Modified location: line 7 in function `read_offset`
- Change: `.precondition (v0 >= 0 && v0 < 4)` -> `.precondition (v0 >= 0 && v0 < 2)`
- Modified location: line 12 in block `.read_offset_bb0`
- Change: `.assume (v0 >= 0 && v0 < 4)` -> `.assume (v0 >= 0 && v0 < 2)`

## T03 `shared_borrow_out_of_bounds_index`

- File: `T03_shared_borrow_out_of_bounds_index.dtal`
- Expected verifier error: `Bounds check failed`
- Modified location: line 21 in block `.main_bb0`
- Change: `mov v4, 0    : int` -> `mov v4, 1    : int(1)`

## T04 `move_owned_while_shared_borrow_live`

- File: `T04_move_owned_while_shared_borrow_live.dtal`
- Expected verifier error: `Ownership violation`
- Inserted location: line 24 in block `.main_bb0`
- Inserted instruction: `move_owned v6, v1    : [int(7); 1]`
- Effect: inserted immediately before `borrow_end v3    : &[int(7); 1]`

## T05 `i64_add_overflow_from_tampered_constants`

- File: `T05_i64_add_overflow_from_tampered_constants.dtal`
- Expected verifier error: `Arithmetic overflow`
- Modified location: line 9 in block `.main_bb0`
- Change: `mov v0, 42    : int(42)` -> `mov v0, 9223372036854775807    : int(9223372036854775807)`
- Modified location: line 10 in block `.main_bb0`
- Change: `mov v1, 10    : int(10)` -> `mov v1, 1    : int(1)`
- Modified location: line 11 in block `.main_bb0`
- Change: `add v2, v0, v1    : int(52)` -> `add v2, v0, v1    : i64`

## T06 `shared_borrow_negative_index`

- File: `T06_shared_borrow_negative_index.dtal`
- Expected verifier error: `Bounds check failed`
- Modified location: line 21 in block `.main_bb0`
- Change: `mov v4, 0    : int` -> `mov v4, -1    : int(-1)`

## T07 `use_after_drop_owned`

- File: `T07_use_after_drop_owned.dtal`
- Expected verifier error: `used after ownership was consumed`
- Inserted location: line 25 in block `.main_bb0`
- Inserted instruction: `drop_owned v1    : [int(7); 1]`
- Inserted location: line 26 in block `.main_bb0`
- Inserted instruction: `move_owned v6, v1    : [int(7); 1]`
- Effect: both instructions were inserted immediately before `push v5    : int`

## T08 `plain_mov_duplicates_owned_value`

- File: `T08_plain_mov_duplicates_owned_value.dtal`
- Expected verifier error: `Ownership violation`
- Inserted location: line 25 in block `.main_bb0`
- Inserted instruction: `mov v6, v1    : [int(7); 1]`
- Effect: inserted immediately before `push v5    : int`

## T09 `alias_shared_while_mutable_borrow_live`

- File: `T09_alias_shared_while_mutable_borrow_live.dtal`
- Expected verifier error: `Ownership violation`
- Inserted location: line 34 in block `.main_bb0`
- Inserted instruction: `alias_borrow v9, v0    : &[int; 1]`
- Effect: inserted immediately after `borrow_mut r0, v0    : int`

## T10 `double_mutable_borrow`

- File: `T10_double_mutable_borrow.dtal`
- Expected verifier error: `Ownership violation`
- Inserted location: line 38 in block `.main_bb0`
- Inserted instruction: `borrow_mut r1, v1    : int`
- Effect: inserted immediately after `borrow_mut r0, v1    : int`

## T11 `mutable_store_out_of_bounds_index`

- File: `T11_mutable_store_out_of_bounds_index.dtal`
- Expected verifier error: `Bounds check failed`
- Modified location: line 11 in block `.touch_bb0`
- Change: `mov v1, 0    : int(0)` -> `mov v1, 1    : int(1)`

## T12 `entry_param_type_weakened_from_mutable_to_shared`

- File: `T12_entry_param_type_weakened_from_mutable_to_shared.dtal`
- Expected verifier error: `Type mismatch`
- Modified location: line 5 in function `touch`
- Change: `.params {v0: &mut [int; 1]}` -> `.params {v0: &[int; 1]}`

## T13 `tampered_entry_state_type`

- File: `T13_tampered_entry_state_type.dtal`
- Expected verifier error: `Type mismatch`
- Modified location: line 11 in block `.read_offset_bb0`
- Change: `.entry {v0: int}` -> `.entry {v0: bool}`

## T14 `impossible_assertion_inserted`

- File: `T14_impossible_assertion_inserted.dtal`
- Expected verifier error: `Cannot prove constraint`
- Inserted location: line 13 in block `.read_offset_bb0`
- Inserted instruction: `.assert (v0 < 0)`
- Effect: inserted immediately after `.assume (v0 >= 0 && v0 < 4)`

## T15 `branch_target_swap_breaks_edge_assumption`

- File: `T15_branch_target_swap_breaks_edge_assumption.dtal`
- Expected verifier error: `Cannot prove constraint`
- Modified location: block `.max_of_bb0`
- Change: swap `bgt .max_of_bb1` / `jmp .max_of_bb2` to `bgt .max_of_bb2` / `jmp .max_of_bb1`
- Effect: the taken edge reaches a successor whose first assumption contradicts the comparison fact.

## T16 `false_singleton_annotation_after_mov`

- File: `T16_false_singleton_annotation_after_mov.dtal`
- Expected verifier error: `Singleton type mismatch`
- Modified location: block `.main_bb0`
- Change: `mov v0, 42    : int(42)` -> `mov v0, 42    : int(41)`

## T17 `division_nonzero_evidence_removed`

- File: `T17_division_nonzero_evidence_removed.dtal`
- Expected verifier error: `Cannot prove constraint`
- Modified location: function `safe_divide`
- Change: `.params {v0: int, v1: {v: int | v != 0 }}` -> `.params {v0: int, v1: int}`
- Modified location: block `.safe_divide_bb0`
- Change: `.entry {v0: int, v1: {v: int | v != 0 }}` -> `.entry {v0: int, v1: int}`

## T18 `postcondition_corrupted_to_unprovable_fact`

- File: `T18_postcondition_corrupted_to_unprovable_fact.dtal`
- Expected verifier error: `Postcondition not provable`
- Modified location: function `make_sorted`
- Change: `.postcondition (forall i in 0..2 { v7[i] <= v7[(i + 1)] })` -> `.postcondition v7[0] > v7[1]`
