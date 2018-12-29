#include <stdio.h>

int
is_theorem_valid(char *candidate)
{
	int	 x = 0;
	int	 y = 0;
	int	 z = 0;
	int	 l = 0;
	char	*p = candidate;

	while (*p != 0) {
		if (*p == '-') {
			switch (l) {
			case 0:
				x++;
				break;
			case 1:
				y++;
				break;
			case 2:
				z++;
				break;
			default:
				return 0;
			}
		}
		else if (*p == 'p') {
			l = 1;
		}
		else if (*p == 'q') {
			l = 2;
		}
		p++;
	}

	if ((x + y) == z) {
		return 1;
	}
	return 0;
}

int
main(int argc, char *argv[])
{
	for (int i = 1; i < argc; i++) {
		if (is_theorem_valid(argv[i])) {
			printf("  VALID: %s\n", argv[i]);
		}
		else {
			printf("INVALID: %s\n", argv[i]);
		}
	}
}
