#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

const char	default_path[] = "hello.txt";

int
main(int argc, char *argv[])
{
	char	 buf[2] = {0x41, 0x0a};
	char	*pathname = (char *)default_path;
	int	 fd = -1;

	if (argc > 1) {
		pathname = argv[1];
	}

	fd = open(pathname, O_WRONLY|O_CREAT, S_IWUSR|S_IRUSR);
	if (fd == -1) {
		perror("open");
		exit(1);
	}

	pid_t	pid = fork();
	if (pid < 0) {
		perror("fork");
		exit(1);
	}
	else if (pid > 0) {
		for (int i = 0; i < 5; i++) {
			write(fd, buf, 2);
			sleep(1);
		}
	}
	else {
		buf[0] = 0x42;
		for (int i = 0; i < 5; i++) {
			write(fd, buf, 2);
			sleep(1);
		}
	}

	close(fd);
	exit(0);
}

