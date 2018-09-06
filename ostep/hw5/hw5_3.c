#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int
main(void)
{
	int	pipefd[2];

	if (pipe(pipefd) == -1) {
		perror("pipe");
		exit(1);
	}

	pid_t	pid = fork();
	if (pid < 0) {
		perror("fork");
		exit(1);
	}
	else if (pid > 0) {
		close(pipefd[1]);

		char	buf[1];
		read(pipefd[0], buf, 1);
		printf("goodbye\n");	
	}
	else {
		sleep(1);
		close(pipefd[0]);

		printf("hello\n");
		close(pipefd[1]);
	}
}
