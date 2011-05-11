#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>


int main (int argc, char *argv[])
{
  int fifo_fd;
  char buffer[40];
  fifo_fd = open(argv[1], O_RDONLY);
  if(fifo_fd < 0){
    printf("First run the fifo_write to create the fifo\n");
      return -1;
  }
  while(1){
    read(fifo_fd, buffer, 40);
    printf("Recieved message: %s\n", buffer);
  }
  return 0;
}
