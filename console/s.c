#include <sys/uio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <termios.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <errno.h>

/*
 * Protocal: plain-text string length terminated by a message type byte,
 * followed by data
 */

enum msg_type {
   OUTPUT = 'A',
   MESSAGE,
   HUNGER,
   ALIGN,
   END,
   TEST,
   PAUSED
};

int last_msg_type = TEST;

int fd;
int done = 0;
int paused = 0;
int big_format = 0;
struct termios old;
int width = 112;
int height = 34;

char sbuf[256];

struct {
   enum msg_type type;
   int len;
   char buf[4096];
} msg;

struct {
   int x;
   int y;
} cur;

void screen_init(void);
void screen_end(void);
void draw_box(void);
void read_msg(int fd);
void reset_position(void);
void restore_position(void);
void handle_msg(void);
void clear(int y, int x1, int x2);
int get_num(char **str);
void process_output(char *str, int len);
void print_attr(char *str, int line);
void echon(char *str, int len);
void echo(char *str);

void screen_init() {
   struct termios buf;

   memset(&buf, 0, sizeof(struct termios));
   memset(&old, 0, sizeof(struct termios));
   buf.c_iflag &= ~(ICRNL|IGNCR);
   buf.c_lflag &= ~(ICANON|ECHO|ISIG);
   buf.c_cc[VMIN] = 1;
   buf.c_cc[VTIME] = 0;
   tcgetattr(0, &old);
   tcsetattr(0, TCSANOW, &buf);
   echo("\e[2J\e[H\e(B\e)0");
   cur.x = cur.y = 1;
}

void screen_end() {
   tcsetattr(0, TCSANOW, &old);
}

void echon(char *str, int len) {
   while (len > 0) {
      int i = write(1, str, len);
      if (i == -1) {
         if (errno == EAGAIN)
            continue;
         sprintf(sbuf, "write error(%d)", errno);
         perror(sbuf);
         exit(1);
      }
      len -= i;
      str += i;
   }
}

void echo(char *str) {
   echon(str, strlen(str));
}

void draw_box() {
   int i;

   echo("\016l");
   for (i = 2; i < width; i++)
      echo("q");
   echo("k\r\n");
   for (i = 0; i < 9; i++)
      echo("x\r\n");
   echo("t");
   for (i = 2; i < width; i++)
      echo(i == 81 ? "w" : "q");
   for (i = 12; i < height; i++)
      echo("\r\nx");
   echo("\r\nm");
   for (i = 2; i < width; i++)
      echo(i == 81 ? "v" : "q");
   echo("j");
   for (i = 2; i < height; i++) {
      sprintf(sbuf, "\e[%d;112H", i);
      echo(sbuf);
      echo(i == 11 ? "u" : "x");
   }
   for (i = 12; i < height; i++) {
      sprintf(sbuf, "\e[%d;81Hx", i);
      echo(sbuf);
   }
   echo("\e[12;2H\017");
   reset_position();
}

void read_msg(int fd) {
   int len, i, n;
   char c;

   len = -1;
   for (;;) {
      if ((read(fd, &c, 1)) == -1) {
         perror("bridey-console");
         exit(1);
      }
      if (c < '0' || c > '9')
         break;
      len = (len == -1) ? c-'0' : (len*10 + c-'0');
   }

   if (len == -1) {
      fprintf(stderr, "message out of order\n");
      exit(1);
   }

   if (c > last_msg_type) {
      fprintf(stderr, "perverse msg_type: %d\n", c);
      exit(1);
   }

   assert(len < 4096);

   msg.type = c;
   msg.len = len;
      
   while (len > 0) {
      char *offset = msg.buf + msg.len - len;

      n = read(fd, offset, len);
      if (n == -1)
         perror("bridey-console");
      else if (!n)
         fprintf(stderr, "data dried up.\n");
      else {
         len -= n;
         continue;
      }
      exit(1);
   }
}

void handle_msg() {
   switch (msg.type) {
   case END:
      done++;
      break;;
   case TEST:
      msg.buf[msg.len] = '\0';
      printf("Testing won too thee. Got: %s\n", msg.buf);
      break;
   case OUTPUT:
      if (big_format)
         process_output(msg.buf, msg.len);
      else
         write(1, msg.buf, msg.len);
      break;
   case PAUSED:
      write(fd, paused ? "#t " : "#f ", 3);
      paused = 0;
      break;
   }
}

void handle_cmd(int c) {
   if (c == 'p')
      paused = 1;
}

void clear(y, x1, x2) {
   int i, len;

   if (x1 >= x2) {
      fprintf(stderr, "printing backwards! from %d to %d\n", x1, x2);
      return;
   }
   sprintf(sbuf, "\e[%d;%dH", y, x1);
   echo(sbuf);

   memset(sbuf, ' ', 80);
   echon(sbuf, x2-x1);
}

int get_num(char **str) {
   int acc = 0, got_num = 0;
   char *p = *str;

   while (*p && *p >= '0' && *p <= '9') {
      got_num = 1;
      acc = acc*10 + *p-'0';
      *p++;
   }
   
   *str = p;
   return got_num ? acc : -1;
}


void process_output(char *str, int len) {
   int i = 0, a, b;
   char *p = str, *esc_start, *end = str+len-1;

   while (p <= end) {
      if (*p == 033) {
         esc_start = p;
         if (*++p != '[')
            goto print_esc;
         p++;
         a = b = -1;
         a = get_num(&p);
         if (a != -1 && *p == ';') {
            p++;
            b = get_num(&p);
         }
         switch (*p) {
         case 'A':
         case 'B':
            if (a == -1) a = 1;
            cur.y += (*p == 'A') ? -a : a;
            goto print_esc;
         case 'C':
         case 'D':
            if (a == -1) a = 1;
            cur.x += (*p == 'C') ? a : -a;
            goto print_esc;
         case 'H':
            if (a == -1) a = 1;
            if (b == -1) b = 1;
            a += 11;
            b++;
            sprintf(sbuf, "\e[%d;%dH", a, b);
            echo(sbuf);
            cur.y = a;
            cur.x = b;
            break;
         case 'G':
            assert(a++ != -1);
            sprintf(sbuf, "\e[%dG", a);
            echo(sbuf);
            cur.x = a;
            break;
         case 'J':
            if (!a || a == -1) {
               clear(cur.y, cur.x, 81);
               for (i = cur.y+1; i <= height-1; i++)
                  clear(i, 2, 81);
            } else if (a == 1) {
               for (i = 12; i < cur.y; i++)
                  clear(i, 2, 81);
               clear(cur.y, 2, cur.x);
            } else {
               assert(a == 2);
               for (i = 12; i < height; i++)
                  clear(i, 2, 81);
            }
            restore_position();
            break;
         case 'K':
            if (cur.y >= height-1)
               break;
            if (!a || a == -1)
               clear(cur.y, cur.x, 81);
            else if (a == 1)
               clear(cur.y, 2, cur.x);
            else {
               assert(a == 2);
               clear(cur.y, 2, 81);
            }
            restore_position();
            break;
         default: 
            goto print_esc;
         }
         fflush(stdout);
         p++;
         continue;
print_esc:
         fflush(stdout);
         echon(esc_start, p-esc_start+1);
         p++;
         reset_position();
      } else if (*p == '\r') {
         echo("\e[2G");
         p++;
         cur.x = 2;
      } else if (*p == '\b') {
         echo("\b");
         p++;
         if (cur.x) cur.x--;
      } else {
         if (isprint(*p) && cur.y < height-1 && cur.x < 81) {
            if (*p == '\n')
               cur.y++;
            else
               cur.x++;
            echon(p, 1);
         }
         p++;
         fflush(stdout);
      }
      fflush(stdout);
   }
}

void reset_position() {
   int acc = 0;
   char c = 0;

   echo("\e[6n");
   while (c != 033)
      read(0, &c, 1);
   read(0, &c, 1);
   if (c != '[')
      goto fail;
   read(0, &c, 1);
   while (c >= '0' && c <= '9') {
      acc = acc*10 + c-'0';
      read(0, &c, 1);
   }
   if (!acc || c != ';')
      goto fail;
   cur.y = acc;
   read(0, &c, 1);
   acc = 0;
   while (c >= '0' && c <= '9') {
      acc = acc*10 + c-'0';
      read(0, &c, 1);
   }
   if (!acc || c != 'R')
      goto fail;
   cur.x = acc;
   return;

fail:
   fprintf(stderr, "weirdness in reset_position\n");
}

void restore_position() {
   sprintf(sbuf, "\e[%d;%dH", cur.y, cur.x);
   echo(sbuf);
}

void print_attr(char *str, int line) {
   char buf[30];

   memset(buf, ' ', 29);
   buf[30-strlen(str)] = '\0';
   sprintf(sbuf, "\e[%d;82H%s%s", line+11, str, buf);
   echo(sbuf);
   restore_position();
}

int main(void)
{
   char c, buf[256];
   int i, lis_sock, sock;
   struct sockaddr_in addr, mukou;
   socklen_t len;
   struct winsize ws;

   ioctl(1, TIOCGWINSZ, &ws);
   if (ws.ws_col == 80 && ws.ws_row == 24)
      big_format = 0;
   else if (ws.ws_col == 112 && ws.ws_row == 34)
      big_format = 1;
   else {
      fprintf(stderr, "bridey-console: wrong window size.\n");
      exit(1);
   }

   lis_sock = socket(PF_INET, SOCK_STREAM, 0);
   if (lis_sock == -1)
       printf("blahblahblah\n");

   memset(&addr, 0, sizeof(struct sockaddr));
   addr.sin_family = AF_INET;
   addr.sin_port = 30282;
   addr.sin_addr.s_addr = INADDR_ANY;
   i = bind(lis_sock, (struct sockaddr *)&addr, sizeof(struct sockaddr));
   if (i == -1) {
      perror("bridey-console");
      exit(1);
   }

   listen(lis_sock, 1);
   fd = accept(lis_sock, (struct sockaddr *)&mukou, &len);

   screen_init();
   if (big_format)
      draw_box();
   fcntl(0, F_SETFL, O_NONBLOCK);
   /*fcntl(1, F_SETFL, O_NONBLOCK);*/

   while (!done) {
      sprintf(buf, "Position: (%d %d)", cur.x - 1, cur.y - 11);
      print_attr(buf, 1);
      read_msg(fd);
      handle_msg();
      if (read(0, &c, 1))
         handle_cmd(c);
   }

   close(fd);
   close(lis_sock);
   screen_end();
   printf("\e[2J");
}
