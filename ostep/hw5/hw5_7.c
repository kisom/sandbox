#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int
main(void)
{
	pid_t	pid;

	pid = fork();
	if (pid < 0) {
		perror("fork");
		exit(1);
	}
	// child
	else if (pid > 0) {
		close(STDOUT_FILENO);
		printf("screaming into the void...\n");
	}
	else {
		wait(NULL);
		printf("the darkness\n");
	}

	exit(EXIT_SUCCESS);
}
