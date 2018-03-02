#include "defs.h"
#include "system.h"

#include <string.h>

constexpr static char	STATE_STR_OK[] = "ok";
constexpr static char   STATE_STR_STACK_OVERFLOW[] = "stack overflow";
constexpr static char   STATE_STR_STACK_UNDERFLOW[] = "stack underflow";
constexpr static char   STATE_STR_EXECUTION_FAILURE[] = "execution failure";
constexpr static char	STATE_STR_UNKNOWN_WORD[] = "unknown word";
constexpr static char	STATE_STR_RSTACK_OVERFLOW[] = "return stack overflow";
constexpr static char	STATE_STR_RSTACK_UNDERFLOW[] = "return stack underflow";
constexpr static char	STATE_STR_UNKNOWN_STATE[] = "undefined state";
constexpr static char	STATE_STR_ERROR_CODE[] = " (error code ";

void
system_clear_error(System *sys)
{
	sys->status = STATUS_OK;
}

void
system_write_status(System *sys)
{
	char	*buf = nullptr;
	size_t	 len = 0;

	if (sys->interface == nullptr) {
		return;
	}
	
	switch (sys->status) {
	case STATUS_OK:
		buf = (char *)(STATE_STR_OK);
		len = sizeof STATE_STR_OK;
		break;
	case STATUS_STACK_OVERFLOW:
		buf = (char *)(STATE_STR_STACK_OVERFLOW);
		len = sizeof STATE_STR_STACK_OVERFLOW;
		break;
	case STATUS_STACK_UNDERFLOW:
		buf = (char *)(STATE_STR_STACK_UNDERFLOW);
		len = sizeof STATE_STR_STACK_UNDERFLOW;
		break;
	case STATUS_EXECUTION_FAILURE:
		buf = (char *)(STATE_STR_EXECUTION_FAILURE);
		len = sizeof STATE_STR_EXECUTION_FAILURE;
		break;
	case STATUS_UNKNOWN_WORD:
		buf = (char *)(STATE_STR_UNKNOWN_WORD);
		len = sizeof STATE_STR_UNKNOWN_WORD;
		break;
	case STATUS_RSTACK_OVERFLOW:
		buf = (char *)(STATE_STR_RSTACK_OVERFLOW);
		len = sizeof STATE_STR_RSTACK_OVERFLOW;
		break;
	case STATUS_RSTACK_UNDERFLOW:
		buf = (char *)(STATE_STR_RSTACK_UNDERFLOW);
		len = sizeof STATE_STR_RSTACK_UNDERFLOW;
		break;
	default:
		buf = (char *)(STATE_STR_UNKNOWN_STATE);
		len = sizeof STATE_STR_UNKNOWN_STATE;
		break;
	}
	
	sys->interface->wrbuf(buf, len);
	if (sys->status != STATUS_OK) {
		sys->interface->wrbuf((char *)STATE_STR_ERROR_CODE, sizeof STATE_STR_ERROR_CODE);
		write_num(sys->interface, (KF_INT)sys->status);
		sys->interface->wrch(')');
	}
	sys->interface->wrch('.');
}