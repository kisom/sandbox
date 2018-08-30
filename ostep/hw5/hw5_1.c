#include <stdio.h>
#include <unistd.h>


int
main(void)
{
	int	x = 10;
	pid_t	pid;

	pid = fork();
	if (pid < 0) {

	}
	else if (pid > 0) {
		for (int i = 0; i < 5; i++) {
			printf("child: x=%d\n", x);
			x++;
			sleep(1);
		}
	} else {
		for (int i = 0; i < 5; i++) {
			printf("parent: x=%d\n", x);
			x--;
			sleep(1);
		}
	}
}
