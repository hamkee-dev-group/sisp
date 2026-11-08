#!/usr/bin/env python3
"""Crash fuzzer for the SISP interpreter.

Oracle: a child process that dies by signal (SIGSEGV/SIGABRT/SIGKILL/SIGXCPU) or
hangs past a timeout is a bug. A clean result, a "; ..." error message, or a
"; STACK EXHAUSTED." recovery is a PASS -- we only hunt hard crashes.

Isolation is out-of-process: terms are generated here (the interpreter has no RNG),
run in the interpreter as batches over stdin, and a dead child is bisected by
prefix length to the offending term. Each child runs under address-space, stack and
CPU rlimits so a runaway allocation or loop cannot take down the machine (and it
exercises the graceful OOM / stack-exhaustion paths).
"""
import argparse
import os
import re
import random
import resource
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SISP = ROOT / "src" / "sisp"

EXCLUDE = {"quit"}  # terminates the batch process; not a crash


def load_targets():
    funcs_c = (ROOT / "src" / "builtins.c").read_text()
    block = re.search(r"functions\[\]\s*=\s*\{(.*?)\n\};", funcs_c, re.S)
    names = set(re.findall(r'\{\s*"([^"]+)"', block.group(1))) if block else set()
    # prelude.lsp is the canonical standard library (libsisp.h is a
    # gitignored build artifact generated from it).
    lib = (ROOT / "src" / "prelude.lsp").read_text()
    names |= set(re.findall(r"\(define\s+\((\S+)", lib))
    return sorted(names - EXCLUDE)


def gen_atom(rng):
    kind = rng.random()
    if kind < 0.30:
        return str(rng.choice([0, 1, -1, 2, 7, 42, -13, 100,
                               9223372036854775807, -9223372036854775808,
                               rng.randint(-10**6, 10**6)]))
    if kind < 0.42:
        d = rng.choice([2, 3, 4, 0, 7, rng.randint(1, 99)])
        n = rng.choice([1, -3, 5, rng.randint(-50, 50)])
        return f"{n}/{d}"
    if kind < 0.55:
        s = rng.choice(["", "a", "hello", "x y z", "\\\"q\\\"",
                        "".join(rng.choice("ab*+<>-# ") for _ in range(rng.randint(0, 80)))])
        return '"' + s + '"'
    if kind < 0.70:
        return rng.choice(["nil", "t", "tau"])
    if kind < 0.82:
        return "".join(rng.choice("abcdefghijklmnopqrstuvwxyz*+-<>=") for _ in range(rng.randint(1, 6)))
    if kind < 0.92:
        return rng.choice([
            "(quote ())", "(quote (1 2 3))", "(quote (a . b))", "(quote (1 (2 (3))))",
            "(quote (a b c d e))", "(quote ((1 . 2) (3 . 4)))",
        ])
    return rng.choice([
        "{}", "{1 2 3}", "{a b}", "{1 1 2}", "{tau : (< tau 3)}",
        "{tau : (in tau {1 2 3})}", "{{1} {2}}",
    ])


def gen_term(rng, depth, targets, maxargs):
    if depth <= 0 or rng.random() < 0.45:
        return gen_atom(rng)
    f = rng.choice(targets)
    k = rng.randint(0, maxargs)
    args = " ".join(gen_term(rng, depth - 1, targets, maxargs) for _ in range(k))
    return f"({f} {args})".replace("  ", " ").strip()


def gen_rooted(rng, f, depth, targets, maxargs):
    k = rng.randint(0, maxargs)
    args = " ".join(gen_term(rng, depth - 1, targets, maxargs) for _ in range(k))
    return f"({f} {args})".replace("  ", " ").strip()


def child_limits(mem_mb, stack_mb, cpu_s):
    def apply():
        limits = [
            (resource.RLIMIT_AS, mem_mb * 1024 * 1024),
            (resource.RLIMIT_STACK, stack_mb * 1024 * 1024),
            (resource.RLIMIT_CPU, cpu_s),
        ]
        for kind, value in limits:
            try:
                resource.setrlimit(kind, (value, value))
            except (OSError, ValueError):
                # Darwin exposes RLIMIT_AS but may reject it in a spawned
                # child. The wall timeout and remaining supported limits
                # still isolate that run.
                pass
    return apply


def run_batch(terms, cfg, mem=None):
    inp = ("\n".join(terms) + "\n(quit 0)\n").encode("utf-8", "replace")
    try:
        p = subprocess.run(
            [str(SISP)], input=inp,
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
            timeout=cfg.timeout, preexec_fn=child_limits(mem or cfg.mem, cfg.stack, cfg.cpu))
        rc = p.returncode
    except subprocess.TimeoutExpired:
        return ("timeout", None)
    if rc == 0:
        return None
    if rc < 0:
        return ("signal", -rc)
    return ("exit", rc)


def minimize(terms, cfg):
    lo, hi = 1, len(terms)
    while lo < hi:
        mid = (lo + hi) // 2
        if run_batch(terms[:mid], cfg) is not None:
            hi = mid
        else:
            lo = mid + 1
    prefix = terms[:lo]
    culprit = prefix[-1]
    if run_batch([culprit], cfg) is not None:
        return [culprit]
    cur = prefix
    i = 0
    while i < len(cur) - 1:
        cand = cur[:i] + cur[i + 1:]
        if run_batch(cand, cfg) is not None:
            cur = cand
        else:
            i += 1
    return cur


def head_of(term):
    m = re.match(r"\(\s*([^\s()]+)", term)
    return m.group(1) if m else "?"


def _groups(s):
    stack, spans = [], []
    for i, c in enumerate(s):
        if c in "({":
            stack.append(i)
        elif c in ")}" and stack:
            spans.append((stack.pop(), i + 1))
    return spans


def shrink_expr(term, cfg):
    def fails(t):
        return run_batch([t], cfg) is not None
    if not fails(term):
        return term
    cur = term
    changed = True
    while changed:
        changed = False
        for a, b in sorted(_groups(cur), key=lambda ab: ab[0] - ab[1]):
            if a == 0 and b == len(cur):
                continue
            cand = " ".join((cur[:a] + "0" + cur[b:]).split())
            if fails(cand):
                cur = cand
                changed = True
                break
    return cur


def main():
    ap = argparse.ArgumentParser(description="SISP crash fuzzer")
    ap.add_argument("-n", "--per-func", type=int, default=1000,
                    help="tests generated per function (default 1000)")
    ap.add_argument("-b", "--batch", type=int, default=200, help="terms per child process")
    ap.add_argument("-d", "--depth", type=int, default=4, help="max expression nesting depth")
    ap.add_argument("-a", "--maxargs", type=int, default=5, help="max args per call")
    ap.add_argument("-s", "--seed", type=int, default=1234, help="RNG seed (reproducible)")
    ap.add_argument("--timeout", type=float, default=15.0, help="wall timeout per batch (s)")
    ap.add_argument("--cpu", type=int, default=10, help="CPU-seconds rlimit per child")
    ap.add_argument("--mem", type=int, default=768, help="address-space rlimit per child (MB)")
    ap.add_argument("--stack", type=int, default=64, help="stack rlimit per child (MB)")
    ap.add_argument("--verify-mem", type=int, default=8192,
                    help="re-run each crasher at this MB to split logic vs OOM-flaky")
    ap.add_argument("--only", default=None, help="comma-separated subset of functions to fuzz")
    args = ap.parse_args()

    if not SISP.exists():
        sys.exit(f"interpreter not built: {SISP} (run `make`)")

    targets = load_targets()
    fuzz_set = targets
    if args.only:
        want = set(args.only.split(","))
        fuzz_set = [f for f in targets if f in want]
        if not fuzz_set:
            sys.exit(f"none of {want} are known targets")

    rng = random.Random(args.seed)
    print(f"targets: {len(fuzz_set)} functions x {args.per_func} tests = "
          f"{len(fuzz_set) * args.per_func} terms; seed={args.seed}")

    terms = [gen_rooted(rng, f, args.depth, targets, args.maxargs)
             for f in fuzz_set for _ in range(args.per_func)]
    rng.shuffle(terms)

    outdir = ROOT / "tests" / "fuzz-out"
    outdir.mkdir(exist_ok=True)
    crash_log = outdir / "crashes.txt"

    crashes = {}
    nbatches = (len(terms) + args.batch - 1) // args.batch
    t0 = time.time()
    for bi in range(nbatches):
        batch = terms[bi * args.batch:(bi + 1) * args.batch]
        res = run_batch(batch, args)
        if res is not None:
            repro = minimize(batch, args)
            culprit = repro[-1]
            f = head_of(culprit)
            if f not in crashes:
                if len(repro) == 1 and run_batch([culprit], args) is not None:
                    culprit = shrink_expr(culprit, args)
                    repro = [culprit]
                    f = head_of(culprit)
                if f not in crashes:
                    kind = run_batch(repro, args) or res
                    crashes[f] = {"kind": kind, "func": f,
                                  "repro": repro, "term": culprit}
        if (bi + 1) % 25 == 0 or bi + 1 == nbatches:
            el = time.time() - t0
            print(f"  batch {bi + 1}/{nbatches}  ({el:.0f}s)  unique-crashers={len(crashes)}",
                  flush=True)

    dt = time.time() - t0
    for c in crashes.values():
        c["real"] = run_batch(c["repro"], args, mem=args.verify_mem) is not None
    real = {k: c for k, c in crashes.items() if c["real"]}
    flaky = {k: c for k, c in crashes.items() if not c["real"]}
    by_func = {}
    by_kind = {}
    for c in real.values():
        by_func[c["func"]] = by_func.get(c["func"], 0) + 1
        k = f"{c['kind'][0]}:{c['kind'][1]}"
        by_kind[k] = by_kind.get(k, 0) + 1

    with crash_log.open("w") as fh:
        for c in sorted(real.values(), key=lambda x: x["func"]):
            fh.write(f"# func={c['func']} kind={c['kind']} class=logic\n")
            for line in c["repro"]:
                fh.write(line + "\n")
            fh.write("(quit 0)\n\n")
        for c in sorted(flaky.values(), key=lambda x: x["func"]):
            fh.write(f"# func={c['func']} kind={c['kind']} class=oom-flaky\n")
            for line in c["repro"]:
                fh.write(line + "\n")
            fh.write("(quit 0)\n\n")

    print("\n==== FUZZ SUMMARY ====")
    print(f"terms run: {len(terms)}  batches: {nbatches}  wall: {dt:.0f}s")
    print(f"deterministic logic crashers: {len(real)}   oom-flaky: {len(flaky)}")
    print(f"logic by kind: {by_kind}")
    if by_func:
        print("functions with logic crashers:")
        for f, n in sorted(by_func.items(), key=lambda x: -x[1]):
            print(f"  {f}: {n}")
    print(f"reproducers written to {crash_log}")
    sys.exit(1 if real else 0)


if __name__ == "__main__":
    main()
