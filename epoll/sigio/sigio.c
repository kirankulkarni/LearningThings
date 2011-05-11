#include <signal.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <error.h>
#include <sys/types.h>


static volatile sig_atomic_t gotSigio = 0;

void sigIoHandler(int sig, siginfo_t *info, void *ptr)
{
  gotSigio = 1;
}

int main (int argc, char *argv[])
{
  int fd;
  struct sigaction sa;
  char buffer[10];
  int j;
  int pid, flags;
  sigemptyset(&sa.sa_mask);
  
  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = sigIoHandler;
  if (sigaction(SIGIO, &sa, NULL) == -1){
    perror("Failed to register Signal Handler");
  }

  fd = open(argv[1], O_RDONLY);
  if(fd < 0){
    perror("Failed to open the file");
  }

  if(fcntl(fd, F_SETOWN, getpid()) == -1){
    perror("Failed to set the Ownership");
  }
  flags = fcntl(fd, F_GETFL);
  printf("Sync: %d, NONBLOCK: %d\n", (flags & O_ASYNC), (flags & O_NONBLOCK));
  if(fcntl(fd, F_SETFL, flags | O_NONBLOCK | FASYNC) == -1){
    perror("Failed to set Flags");
  }
    
  while(1){
    for(j=0; j < 100000000; j++)
      continue;
    /* printf("Here %d\n", gotSigio); */
    if(gotSigio){
      while(read(fd, buffer, 10) > 0){
        printf("message: %s\n", buffer);
      }
      gotSigio = 0;
    }
  }
  
}
