#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>


int main (int argc, char *argv[])
{
  int fifo_fd;
  int n;
  char MSG[]= "This is a sample message, Read this man!!!!";
  fifo_fd = open(argv[1], O_WRONLY);
  if(fifo_fd < 0){
    fifo_fd = mkfifo(argv[1], S_IRWXU);
    if(fifo_fd < 0){
      printf("ERROR while creating file\n");
      return -1;
    }
  }
  n = 0;
  sleep(6);
  printf("starting to write \n");
  while(1){
    if(n == 100){
      n = 0;
      sleep(1);
    }
    write(fifo_fd, MSG, strlen(MSG) + 1);
    n++;
  }
  return 0;
}
