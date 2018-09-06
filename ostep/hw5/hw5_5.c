#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int
main(void)
{
	int	status = 0;

	pid_t	pid = fork();
	if (pid < 0) {
		perror("fork");
		exit(EXIT_FAILURE);
	}
	else if (pid > 0) {
		wait(&status);
		if (!WIFEXITED(status)) {
			fprintf(stderr, "child exited due to an error\n");
		}
		printf("goodbye: %d\n", status);
	}
	else {
		wait(&status);
		if (!WIFEXITED(status)) {
			fprintf(stderr, "child exited due to an error\n");
		}
		printf("hello: %d\n", status);
	}

	exit(EXIT_SUCCESS);
}
