/********************************************************/
/*                                                      */
/*               OO7 Benchmark                          */
/*                                                      */
/*              COPYRIGHT (C) 1993                      */
/*                                                      */
/*                Michael J. Carey 		        */
/*                David J. DeWitt 		        */
/*                Jeffrey Naughton 		        */
/*               Madison, WI U.S.A.                     */
/*                                                      */
/*	         ALL RIGHTS RESERVED                    */
/*                                                      */
/********************************************************/

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

/* read a purge file, 1000 bytes at a time */

#define BUFSIZE 1000

main(argc, argv)
int argc; char **argv;
{
	int  	i=0;
	int	purgeFileFd;
	char	buf[BUFSIZE];
	int	status;
	char	c;
	int	cnt;

	if (argc < 2) {
	     printf("usage: %s  purgeFileName\n",argv[0]);
	     exit(1);
	}
	purgeFileFd = open(argv[1], O_RDONLY);
	if (purgeFileFd < 0) {
	  fprintf(stderr, "Couldn't open purge file: %s\n", argv[1]);
	  exit(1);
	}
	printf("Beginning to read purge file %s\n",argv[1]);

	while ((cnt = read(purgeFileFd,buf, BUFSIZE)) == BUFSIZE)
	{
		c = buf[100]; /* insure compiler isn't too smart */
		i++;
	}
	printf("Beginning to read purge file %s backwards\n",argv[1]);
	while (i--)
	{
		status = lseek(purgeFileFd,-BUFSIZE,SEEK_CUR);
		if (lseek <=0) printf("lseek error\n");
		cnt = read(purgeFileFd,buf, BUFSIZE);
		c = buf[100]; /* insure compiler isn't too smart */
	}
	close(purgeFileFd);
	
}
