int kprobe__sys_clone(void *ctx)
{
	bpf_trace_printk("Hello, world\n");
	return 0;
}
