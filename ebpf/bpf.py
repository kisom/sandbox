#!/usr/bin/env python3

import argparse
import sys

from bcc import BPF


DEBUG_FLAGS = {
    "ir": 0x1,  # debug output compiled LLVM IR
    "bpf": 0x2,  # debug output loaded BPF bytecode and register state on branches.
    "pp": 0x4,  # debug output pre-processor result.
    "src": 0x8,  # debug output ASM instructions embedded with source.
    "reg": 0x10,  # debug output register state on all instructions in addition to DEBUG_BPF.
    "btf": 0x20,  # debug BTF
}


def get_debug_flags(args: str) -> int:
    if not args.strip():
        return 0

    flags = [arg.strip() for arg in args.split(",")]
    debug = 0
    for flag in flags:
        debug |= DEBUG_FLAGS[flag]
    return debug


def load_program(debug_args: str, program_source: str) -> BPF:
    debug_flags = get_debug_flags(debug_args)

    program: str
    if program_source == "-":
        print('LD: STDIN')
        program = sys.stdin.read()
    else:
        print('LD: {}'.format(program_source))
        with open(program_source) as source_file:
            program = source_file.read()

    bpf = BPF(text=program, debug=debug_flags)
    print('LD: OK')
    return bpf


def main(args):
    bpf = load_program(args.debug, args.path)
    print('RUN')
    bpf.trace_print()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="bpf", description="Run BPF programs")
    parser.add_argument("--debug", "-d", help="debug flags", default="")
    parser.add_argument("path", help="program source")

    args = parser.parse_args()
    main(args)
