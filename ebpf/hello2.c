int kprobe__sys_read(void *ctx)
{
	bpf_trace_printk("Hello, world\n");
	return 0;
}
