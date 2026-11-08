# A computer and an operating system, in SISP

Programs that build a working stored-program computer and a
preemptive multitasking Unix on top of it — entirely in SISP.

## The computer

```
src/sisp examples/machine/computer.lsp
```

boots the machine and drops you into a live session (the REPL is your
terminal):

```
: (login "root" "sisp")
: (sh "ls /")
: (sh "cat /etc/motd")
: (sh "mkdir /home/root")
: (sh "echo it works > /home/root/notes")
: (sh "run count")
: (ed-open "/home/root/todo")
: (pm-install "fortune")
: (build-file "/usr/demo.s" "demo")
: (sh "run demo")
: (sysmon)
: (save)
```

What is real in it:

- **CPU (hardware/cpu.lsp)** — 24-bit V2.1: RAM + register file, MMU,
  vectored interrupts with IRET, timer + keyboard IRQs, condition
  codes, shifts/logic, character console, sector disk, hypercalls,
  two-pass assembler. 20 checks.
- **Kernel (os/sisp-unix.lsp)** — preemptive multitasking with a full
  16-register context switch, per-process memory windows, a
  machine-code timer ISR, fork/exec/wait/exit/kill, blocking kbd
  reads, and a hierarchical filesystem on the sector disk
  (superblock, inodes, block bitmaps, directories as files).
- **Persistence** — the whole platter backs up to a host file
  (`save`); it is restored at boot. Your files survive restarts.
- **Users/login (os/users.lsp)** — `/etc/passwd` on disk, sessions,
  root-gated administration.
- **Shell** — `ls cd pwd mkdir rmdir cat echo rm ps kill run jobs`,
  pipes and `>` redirection, on the real disk.
- **Tools (os/tools.lsp)** — line editor, cron (driven by the
  scheduler's tick), a dependency-resolving package manager that
  installs files and programs, sysmon, a text→machine-code build
  tool, and a disassembler/tracer debugger.
- **Network (os/net.lsp)** — a NIC whose frames are real files under
  `/tmp/sispnet-<id>.in`; netd dispatches to port listeners:
  ping (7), a web server (80) serving pages from its own disk, and a
  disk-backed key-value store (42).

## Two machines talking

Two sisp processes are two computers. The spool files are the wire.

```
# terminal 1
echo bob > examples/machine/MACHINE-ID
src/sisp examples/machine/computer.lsp
: (netd-serve 30000000)      ; pump the server

# terminal 2 (id defaults to ada)
src/sisp examples/machine/computer.lsp
: (web-get "bob" "/index.html")   ; -> "200 <html>..."
: (ping "bob")                    ; -> "bob: pong"
: (kv-put "bob" "lang" "sisp")    ; -> "OK"
: (kv-get "bob" "lang")           ; -> "sisp"
```

## Verification

```
src/sisp examples/machine/hardware/cpu.lsp        ; 20 checks
src/sisp examples/machine/os/sisp-unix-demo.lsp   ; 66 checks
src/sisp examples/machine/os/computer-checks.lsp  ; 56 checks
```

## Layout

| Path | What |
|---|---|
| `computer.lsp` | the boot script: OS + users + tools + net in one computer |
| `MACHINE-ID` | this machine's network name |
| `hardware/cpu.lsp` | V2.1 CPU + assembler |
| `os/sisp-unix.lsp` | kernel, hierarchical fs, shell (quiet, includable) |
| `os/sisp-unix-demo.lsp` | the kernel/fs/shell demo + checks |
| `os/users.lsp` | login and accounts |
| `os/tools.lsp` | editor, cron, packages, sysmon, build, debugger |
| `os/net.lsp` | spool NIC, netd, ping, web, kv |
| `os/computer-checks.lsp` | integration checks |
| `hardware/`, `boot/`, `compilers/`, `network/`, `programs/`, `shell/`, `storage/`, `tools/`, `interpreters/` | earlier experiments and alternate models |
