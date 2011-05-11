#include<stdio.h>
#include<string.h>
#include<sys/types.h>
#include<fcntl.h>
#include<sys/select.h>
#include<sys/time.h>

static void
usageError(const char *progName)
{
  fprintf(stderr,"Use \"%s\" wisely, you moron", progName);
}

int main(int argc, char *argv[])
{
  fd_set readfds, writefds;
  struct timeval timeout;
  struct timeval *pto;
  char mode;
  int fd;
  char buf[10];
  int ready;
  if (argc < 2 || strcmp(argv[1], "--help") == 0)
    usageError(argv[0]);
  /* Timeout */
  if (strcmp(argv[1], "-") == 0){
    pto = NULL;
  }
  else {
    pto = &timeout;
    timeout.tv_sec = atoi(argv[1]);
    timeout.tv_usec = 0;
  }
  FD_ZERO(&readfds);
  FD_ZERO(&writefds);
  mode = argv[3][0];
  fd = open(argv[2], ((mode == 'r')? O_RDONLY : ((mode == 'w')? O_WRONLY: O_RDWR)));
  if(fd < 0){
    fprintf(stderr, "%s file/FIFO doesn't exist", argv[1]);
  }
  if(mode == 'r'){
    FD_SET(fd, &readfds);
  }else if(mode == 'w'){
    FD_SET(fd, &writefds);
  }
  ready = select((fd + 1), &readfds, &writefds, NULL, pto);
  if (ready == -1){
    fprintf(stderr,"Select returned -1");
  }

  fprintf(stdout, "Obtained the %c permission :)\n", mode);
  fprintf(stdout, "Timeout %ld.%03ld\n", (long)timeout.tv_sec, (long) timeout.tv_usec/10000);
  return 0;
}
