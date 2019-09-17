#!/usr/bin/env python3

from bcc import BPF

program = """
int
kprobe__sys_clone(void *ctx)
{
    bpf_trace_printk("Hello, world\\n");
    return 0;
}
"""

print('LD PROG')
b = BPF(text=program)
print('TRC PRN')
b.trace_print()
