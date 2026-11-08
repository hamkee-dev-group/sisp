#!/usr/bin/env python3
# Emit two SISP test corpora:
#   intrat.lsp  - pure int/rational arithmetic (also valid without float support)
#   float.lsp   - float / mixed arithmetic (requires float support)
import sys

ints = [0, 1, 2, 3, 5, 7, -1, -2, -4, 8, 12, 100, -100, 1024, -7]
rats = ["1/2", "3/4", "-2/3", "7/5", "-11/6", "22/7", "1/3", "-5/2"]
floats = ["0.5", "1.5", "2.25", "3.14159", "0.1", "0.2", "-0.75", "100.5",
          "0.001", "-2.5", "1.25", "0.3333"]

def w(f, exprs):
    for e in exprs:
        f.write(e + "\n")

def bin_exprs(operands, ops):
    out = []
    for op in ops:
        for a in operands:
            for b in operands:
                out.append(f"({op} {a} {b})")
    return out

def vararg_exprs(operands, ops):
    out = []
    for op in ops:
        for a in operands:
            for b in operands:
                for c in operands[:5]:
                    out.append(f"({op} {a} {b} {c})")
    return out

# --- int/rat corpus: everything that must be byte-identical to the baseline ---
intrat = []
operands = [str(i) for i in ints] + rats
intrat += bin_exprs(operands, ["+", "-", "*"])
# division: skip zero divisors
for a in operands:
    for b in operands:
        if b == "0":
            continue
        intrat.append(f"(/ {a} {b})")
# power: integer exponents only
for a in operands:
    for e in [0, 1, 2, 3, -1, -2, 4]:
        intrat.append(f"(^ {a} {e})")
# mod: integers only
for a in [str(i) for i in ints]:
    for b in [str(i) for i in ints]:
        if b == "0":
            continue
        intrat.append(f"(mod {a} {b})")
# comparisons
intrat += bin_exprs(operands, ["<", "<=", ">", ">=", "=", "/="])
# min / max (varargs)
intrat += vararg_exprs(operands, ["min", "max"])
# nested
intrat += [
    "(+ (* 3 4) (/ 10 2) (- 8 3))",
    "(* (+ 1/2 1/3) (- 5 2))",
    "(^ (+ 1 2) 10)",
    "(/ (* 22 7) (+ 3 4))",
    "(max (min 3 7 2) (min 9 1 4) 5)",
    "(- (^ 2 20) (^ 3 10))",
]

# --- float corpus: golden for reproducibility ---
flt = []
fops = [str(i) for i in ints] + rats + floats
flt += bin_exprs(fops, ["+", "-", "*"])
for a in fops:
    for b in fops:
        if b in ("0",):
            continue
        flt.append(f"(/ {a} {b})")
for a in floats:
    for e in [0, 1, 2, 3, -1, -2, 4]:
        flt.append(f"(^ {a} {e})")
for a in floats + ["5", "-7"]:
    for b in ["2", "3", "1.5", "-2.5", "0.5"]:
        flt.append(f"(mod {a} {b})")
flt += bin_exprs(fops, ["<", "<=", ">", ">=", "=", "/="])
flt += vararg_exprs(floats + ["1", "1/2"], ["min", "max"])
flt += [
    "(+ 0.1 0.2)",
    "(* 0.1 0.1)",
    "(- 0.3 0.1)",
    "(/ 1.0 3)",
    "(* 0.001 0.001)",
    "(+ 1.5 1.5)",
    "(+ 1.5 1/2)",
    "(* 2.5 (+ 1 1))",
    "(^ 1.5 10)",
    "(typeof 1.5)",
    "(typeof 2.0)",
    "(typeof (+ 1.5 0.5))",
    "(typeof (+ 1.5 1.5))",
    "(mod 5.5 2)",
    "(mod -5.5 2)",
    "(max 1.5 2 0.5 3.25)",
    "(min 1.5 2 0.5 3.25)",
]

with open(sys.argv[1], "w") as f:
    w(f, intrat)
with open(sys.argv[2], "w") as f:
    w(f, flt)
print(f"intrat: {len(intrat)} exprs, float: {len(flt)} exprs")
