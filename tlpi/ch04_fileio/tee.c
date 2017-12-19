#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>

#define MAX_READ 512

int main(int argc, char *argv[]) {
  char buff[MAX_READ];
  ssize_t nr, nw, rem;
  int fd;

  if (argc < 2) {
    printf("USAGE: %s file\n", argv[0]);
    return 1;
  }

  fd = open(argv[1], O_WRONLY | O_CREAT | O_TRUNC,
            S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  if (fd == -1) {
    return errExit("open");
  }

  while ((nr = read(STDIN_FILENO, buff, MAX_READ)) > 0) {
    for (rem = nr; rem > 0; rem = rem - nw) {
      if ((nw = write(STDOUT_FILENO, buff, rem)) == -1) {
        return errExit("write stdout");
      }
    }
    for (rem = nr; rem > 0; rem = rem - nw) {
      if ((nw = write(fd, buff, rem)) == -1) {
        return errExit("write %s", argv[1]);
      }
    }
  }
  if (nr == -1) {
    return errExit("read");
  }
}

int errExit(char *call) {
  perror(call);
  return 1;
}
