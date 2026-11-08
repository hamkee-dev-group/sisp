# A quantum computer (and a little quantum OS) in pure SISP

A gate-level quantum computer simulated entirely in SISP, with real,
textbook quantum algorithms running on it as actual circuits.

## What's real

- **A universal gate set**, applied to a genuine `2^n` complex amplitude
  vector: H, X, Y, Z, S, T, phase, controlled-phase, CNOT, CZ, SWAP, Toffoli.
  Real superposition, entanglement, interference, and measurement.
- **Real algorithms, as circuits** (verified in `tests.lsp`):
  - Bell & GHZ entanglement
  - **Bernstein–Vazirani** — recover an n-bit hidden string in *one* query
  - **Deutsch–Jozsa** — constant vs balanced in *one* query
  - **Grover** — search 2^n items in ~√(2^n) steps (finds `|101>` at p≈0.95)
  - **Quantum Fourier Transform**
  - **Teleportation** — move a qubit's state via entanglement + corrections
  - **Superdense coding** — 2 classical bits through 1 qubit
- **A quantum-OS layer** (`qos.lsp`): allocate a register, run a program, read
  out the state, and perform a probabilistic **measurement** that collapses the
  superposition by sampling `|amplitude|²`.

## What's not real (and can't be)

This is a **simulation**. It reproduces quantum behavior exactly but:

- Cost is `2^n` — it stores and updates every amplitude, so it gets **no
  quantum speedup** and is practical only for a handful of qubits (≲12).
- It is **not a physical quantum computer.** Real qubits exploit quantum
  mechanics in hardware (superconducting circuits, trapped ions, photons).
  No program in any language can *be* one — that barrier is physics, not code.

Simulating a real quantum computer would take a real quantum computer.

## Running

```sh
examples/quantum/run.sh          # narrated demo of all algorithms
examples/quantum/run.sh tests    # PASS/FAIL algorithm suite
```

## Files

| file | role |
|------|------|
| `qvm.lsp` | complex arithmetic, state vector, gates, circuit runner, measurement |
| `algorithms.lsp` | the quantum algorithms as gate circuits |
| `qos.lsp` | register/measurement/readout layer + job reporting |
| `tests.lsp` | correctness checks for every algorithm |
| `demo.lsp` | narrated end-to-end run |
| `run.sh` | runner |
