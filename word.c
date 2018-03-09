#include "defs.h"
#include "eval.h"
#include "word.h"

#include <string.h>

static uint8_t	dict[DICT_SIZE] = {0};
static size_t	last = 0;

void
append_native_word(const char *name, const uint8_t len, void(*target)(void))
{
	store_native(dict+last, name, len, target);
}

bool
execute(const char *name, const uint8_t len)
{
	size_t	offset = 0;
	size_t	body = 0;
	while (true) {
		if (!match_word(dict+offset, name, len)) {
			if ((offset = word_link(dict+offset)) == 0) {
				return false;
			}
			continue;
		}

		body = word_body(dict+offset);
		cwexec((uintptr_t)(dict + body + offset));
		return true;
	}
}

bool
lookup(const char *name, const uint8_t len, uintptr_t *ptr)
{
	size_t	offset = 0;
	size_t	body = 0;
	while (true) {
		if (!match_word(dict+offset, name, len)) {
			if ((offset = word_link(dict+offset)) == 0) {
				return false;
			}
			continue;
		}

		body = word_body(dict+offset);
		*ptr = (uintptr_t)(dict + offset + body);
		return true;
	}

}

void
store_native(uint8_t *entry, const char *name, const uint8_t len, void(*target)(void))
{
	uintptr_t	target_p = (uintptr_t)target;
	size_t		offset = 2 + len + sizeof(size_t);
	size_t		link = offset + (2 * sizeof(uintptr_t));

	/* write the header */
	entry[0] = len;
	entry[1] = 0; // flags aren't used yet
	memcpy(entry+2, name, len);
	memcpy(entry+2+len, &link, sizeof(link));

	/* write the native executor codeword and the function pointer */
	memcpy(entry+offset, (uint8_t *)(&nexec_p), sizeof(uintptr_t));
	offset += sizeof(uintptr_t);
	memcpy(entry+offset, (uint8_t *)(&target_p), sizeof(uintptr_t));
}

bool
match_word(uint8_t *entry, const char *name, const uint8_t len)
{
	if (entry[0] != len) {
		return false;
	}

	if (memcmp(entry+2, name, len) != 0) {
		return false;
	}

	return true;
}

size_t
word_link(uint8_t *entry)
{
	size_t	link;

	if (entry[0] == 0) {
		return 0;
	}
	memcpy(&link, entry+2+entry[0], sizeof(link));
	return link;
}

size_t
word_body(uint8_t *entry)
{
	return 2 + entry[0] + sizeof(size_t);
}
