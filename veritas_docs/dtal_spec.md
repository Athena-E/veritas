### DTAL Instruction Set

#### Data Movement

```
mov rd, imm          -- rd := imm (immediate)
mov rd, rs           -- rd := rs (register copy)
load rd, [rb, ri]    -- rd := mem[rb + ri] (indexed load)
store [rb, ri], rs   -- mem[rb + ri] := rs (indexed store)
lea rd, label        -- rd := address of label
```

#### Arithmetic

```
add rd, rs1, rs2     -- rd := rs1 + rs2
sub rd, rs1, rs2     -- rd := rs1 - rs2
mul rd, rs1, rs2     -- rd := rs1 * rs2
addi rd, rs, imm     -- rd := rs + imm
```

#### Comparison and Logic

```
cmp rs1, rs2         -- set condition flags based on rs1 - rs2
cmpi rs, imm         -- compare register with immediate
and rd, rs1, rs2     -- rd := rs1 & rs2
or rd, rs1, rs2      -- rd := rs1 | rs2
not rd, rs           -- rd := !rs
```

#### Control Flow

```
jmp label            -- unconditional jump
beq label            -- branch if equal (after cmp)
bne label            -- branch if not equal
blt label            -- branch if less than
ble label            -- branch if less or equal
bgt label            -- branch if greater than
bge label            -- branch if greater or equal
call label           -- function call
ret                  -- return from function
```

#### Stack Operations

```
push rs              -- push register onto stack
pop rd               -- pop from stack into register
alloca rd, n         -- allocate n bytes on stack, rd := pointer
```

### DTAL Typing Rules

#### Instruction Typing Judgment

```
Φ; Γ ⊢ instr ⇒ Γ'
```

Under constraints Φ and register types Γ, executing `instr` yields register types Γ'.

#### Key Typing Rules

**Move Immediate (Singleton Type)**:

```
────────────────────────────────────────
Φ; Γ ⊢ mov rd, n ⇒ Γ[rd ↦ int(n)]
```

**Move Register**:

```
Γ(rs) = τ
────────────────────────────────────────
Φ; Γ ⊢ mov rd, rs ⇒ Γ[rd ↦ τ]
```

**Add (Singleton Types)**:

```
Γ(rs1) = int(i1)    Γ(rs2) = int(i2)
────────────────────────────────────────
Φ; Γ ⊢ add rd, rs1, rs2 ⇒ Γ[rd ↦ int(i1 + i2)]
```

**Add (General Case with Refinement Preservation)**:

```
Γ(rs1) = {a: int | φ1}    Γ(rs2) = {b: int | φ2}
────────────────────────────────────────
Φ; Γ ⊢ add rd, rs1, rs2 ⇒ Γ[rd ↦ {c: int | ∃a,b. φ1 ∧ φ2 ∧ c = a + b}]
```

**Load (Array Bounds Check)**:

```
Γ(rb) = ptr(array(τ, n))    Γ(ri) = int(i)    Φ ⊢ 0 ≤ i < n
────────────────────────────────────────────────────────────────
Φ; Γ ⊢ load rd, [rb, ri] ⇒ Γ[rd ↦ τ]
```

**Conditional Branch (Type Refinement)**:

```
Γ(rs1) = {a: int | φa}    Γ(rs2) = {b: int | φb}
Φ ∧ (a < b); Γ ⊢ label : code{Γ'; Φ'}    Φ ⊢ Γ <: Γ'[with a < b]
────────────────────────────────────────────────────────────────────
Φ; Γ ⊢ cmp rs1, rs2; blt label ⇒ Γ[with a ≥ b]
```

**Jump**:

```
Φ; Γ ⊢ label : code{Γ'; Φ'}    Φ ⊢ Γ <: Γ'
────────────────────────────────────────────
Φ; Γ ⊢ jmp label ⇒ ⊥  (unreachable after jump)
```
