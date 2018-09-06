#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int	pipefd[2];

int
main(void)
{
	if (pipe(pipefd) != 0) {
		perror("pipe");
		return EXIT_FAILURE;
	}
	int	pwrite = pipefd[1];
	int	pread = pipefd[0];

	pid_t	pid = fork();
	if (pid < 0) {
		perror("fork");
		return EXIT_FAILURE;
	}
	/* the first child */
	else if (pid == 0) {
		/*
		 * Close standard output and reassign it to the pipe.
		 * The close is done automagically by dup2.
		 */
		dup2(pwrite, STDOUT_FILENO);

		/* Close unnecessary pipe read file descriptor. */
		close(pread);

		/* Now we do the thing. */
		printf("Hello, world.\n");

		/* ¡Adios! */
		return EXIT_SUCCESS;
	}
	
	/*
	 * We are now in the parent. We must fork again, and do the
	 * child dance.
	 */
	if ((pid = fork()) < 0) {
		perror("fork");
		return EXIT_FAILURE;
	}
	/* the second of the children */
	else if (pid == 0) {
		/* Close standard input and reassign it to the pipe. */
		dup2(pread, STDIN_FILENO);

		/* Close unnecessary pipe write file descriptor. */
		close(pwrite);

		/* Read the thing. */
		char	data[16];
		ssize_t	rl;

		memset(data, 0, 16);
		rl = read(STDIN_FILENO, data, 15);
		if (rl < 1) {
			perror("read");
			return EXIT_FAILURE;
		}

		printf("%s", data);
		return EXIT_SUCCESS;
	}

	/* wait for the children to die so we can reap them */
	/* there are two, they should drop dead on their own. */
	int status1, status2;
	waitpid(-1, &status1, 0);
	waitpid(-1, &status2, 0);

	if (WIFEXITED(status1) && WIFEXITED(status2)) {
		printf("Goodbye.\n");
		return EXIT_SUCCESS;
	}
	fprintf(stderr, "At least one child died abnormally.\n");
	return EXIT_FAILURE;
}
