#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <error.h>
#include <errno.h>
#include <sys/epoll.h>
#include <sys/types.h>
#include <sys/time.h>

#define MAX_BUF 1000
#define MAX_EVENTS 5

int main(int argc, char *argv[])
{
  int epfd, fd, open_fds;
  struct epoll_event ev;
  struct epoll_event evlist[MAX_EVENTS];
  char buffer[MAX_BUF];
  int i, events, cnt, ready;
  if(argc < 2 || (!strcmp(argv[1], "--help"))){
    fprintf(stdout, "%s File needs FIFO files as argument\n", argv[0]);
    return 0;
  }
  epfd = epoll_create((argc - 1));
  if(epfd < 0){
    perror("Unable to create an epoll instance");
  }

  for(i=1, open_fds=0;i < argc;i++){
    fd = open(argv[i], O_RDWR);
    if(fd < 0){
      perror("unable to open file");
      continue;
    }
    open_fds++;
    fprintf(stdout, "%s File Opened with fd: %d\n", argv[i], fd);
    ev.events = EPOLLIN;
    ev.data.fd = fd;
    if(epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &ev) == -1){
      perror("Failed to add fd");
      continue;
    }
  }
  fprintf(stdout, "Starting to EPOLL :)\n");
  while(open_fds){

    ready = epoll_wait(epfd, evlist, MAX_EVENTS, -1);
    if(ready == -1){
      if(errno == EINTR)
        continue;
      else{
        perror("Failed to epoll");
        return -1;
      }
    }
    for(i=0;i < ready;i++){
      events = evlist[i].events;
      fprintf(stdout, "fd=%d events= %s%s%s%s\n", evlist[i].data.fd,
             (events & EPOLLIN)? "EPOLLIN " : "",
             (events & EPOLLOUT)? "EPOLLOUT " : "",
             (events & EPOLLHUP)? "EPOLLHUP " : "",
             (events & EPOLLERR)? "EPOLLERR " : "");
      if (events & EPOLLIN){
        memset(buffer, 0, MAX_BUF);
        cnt = read(evlist[i].data.fd, buffer, MAX_BUF);
        if(cnt < 0){
          perror("Failed To read");
          continue;
        }
        fprintf(stdout, "message: %s\n", buffer);
      }else if (events & (EPOLLHUP | EPOLLERR)){
        fprintf(stdout, "Closing fd=%d\n", evlist[i].data.fd);
        if(close(evlist[i].data.fd) < 0){
          perror("Error in closing file");
          return -1;
        }
        open_fds--;
      }
    }
  }
  printf("PEACE \\m/\n");
}
