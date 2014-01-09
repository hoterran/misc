/*
** process.c
**
** Copyright (c) 1994-1997 Peter Eriksson <pen@signum.se>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <unistd.h>
#include <wait.h>
#include <errno.h>
#include <signal.h>
#include <syslog.h>
#include <string.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdlib.h>

#include "phttpd.h"



int proc_run(char *path,
	     uid_t uid, gid_t gid, char *cgi_newroot,
	     rlim_t niceval, rlim_t vmem, rlim_t fd_max, rlim_t maxcputime,
	     int stdin_fd, int stdout_fd, int stderr_fd,
	     char **argv, char **envp,
	     char *wdir, char *cgi_basedir)
{
    pid_t pid;
    int i, s;
    char *rpath = NULL;
    char *err, *err2;
    struct rlimit rlp;


    if (debug > 4)
	fprintf(stderr,
	"proc_run(\"%s\", uid=%d, gid=%d, stdin=%d, stdout=%d, stderr=%d, ..., wdir=%s, cgi_basedir=%s)\n",
		path, (int) uid, (int) gid, stdin_fd, stdout_fd, stderr_fd,
		wdir ? wdir : "<null>", cgi_basedir ? cgi_basedir : "<null>" );
    
    /* Create subprocess */
    pid = fork1();

    if (pid == 0)
    {

	/* Close all open file descriptors */
	for (i = 0; i < max_fds; i++)
	    if (i != stdin_fd &&
		i != stdout_fd &&
		i != stderr_fd)
		s_close(i);
	
	s = 3;
	
	/* Make sure stdin, stdout and stderr don't interfere */
	if (stdin_fd >= s)
	    s = stdin_fd+1;
	if (stdout_fd >= s)
	    s = stdout_fd+1;
	if (stderr_fd >= s+1)
	    s = stderr_fd;
	
	/* Stdin_Fd at Stdout_Fd or Stderr_Fd? Relocate out of way */
	if (stdin_fd == 1 || stdin_fd == 2)
	{
	    s_dup2(stdin_fd, ++s);
	    s_close(stdin_fd);
	    stdin_fd = s;
	}
	
	/* Stdout_Fd at Stdin_Fd or Stderr_Fd?, Relocate out of way */
	if (stdout_fd == 0 || stdout_fd == 2)
	{
	    s_dup2(stdout_fd, ++s);
	    s_close(stdout_fd);
	    stdout_fd = s;
	}
	
	/* Stderr_Fd at Stdin_Fd or Stdout_Fd?, Relocate out of way */
	if (stderr_fd == 0 || stderr_fd == 1)
	{
	    s_dup2(stderr_fd, ++s);
	    s_close(stderr_fd);
	    stderr_fd = s;
	}
	
	/* Move Stdin_Fd to fd #0 */
	if (stdin_fd > 0)
	{
	    s_dup2(stdin_fd, 0);
	    s_close(stdin_fd);
	}
	
	/* Move Stdout_Fd to fd #1 */
	if (stdout_fd != -1 && stdout_fd != 1)
	{
	    s_dup2(stdout_fd, 1);
	    s_close(stdout_fd);
	}
	
	/* Move Stderr_Fd to fd #2 */
	if (stderr_fd != -1 && stderr_fd != 2)
	{
	    s_dup2(stderr_fd, 2);
	    s_close(stderr_fd);
	}
	
	/* in case we have vmem limit ...*/
	if ( vmem != 0 ) 
	{
	    rlp.rlim_cur=vmem;
	    rlp.rlim_max=vmem;
	    setrlimit(RLIMIT_VMEM,&rlp);
	}
	
	/* and do we have a fd_max ? */
	if ( fd_max != 0 )
	{
	    rlp.rlim_cur=fd_max;
	    rlp.rlim_max=fd_max;
	    setrlimit(RLIMIT_NOFILE,&rlp);
	}
	
	/* and do we have CPU-time-limit ? */
	if ( maxcputime != 0 )
	{
	    rlp.rlim_cur=maxcputime;
	    rlp.rlim_max=maxcputime;
	    setrlimit(RLIMIT_CPU,&rlp);
	    signal(SIGXCPU,exit); /* set abort on SIGXCPU :-) */
	}
	
	/* in case we have nicevalue, set it ! */
	if ( niceval != 0 )
	    s_nice(niceval);
	
	/* In case we have a newroot ... */
	if ( cgi_newroot != NULL )
	{
	    s_chdir(cgi_newroot);
            seteuid(0);
	    if ( s_chroot(cgi_newroot) == -1 )
	    {
		err = "chroot()";
		goto Fail;
	    }
	    if ( cgi_basedir == NULL )
	    {
	      if ( rkmultimode || softvirtserver ) rpath=strchr(path,'/');
	      else rpath=path;
	      if ( softvirtserver && rkmultimode && rpath!=NULL ) rpath=strchr(rpath+1,'/');
	    }
	    else
	      rpath=path;
	}
	else /* we do not have chroot */
 	{ 
	  if (wdir)
	  {
	    s_chdir(wdir);
            if ( path[0] != '/' )
		rpath=path+strlen(wdir)+1;
	    else
		rpath=path;
          }
	  else
	    rpath=path;
         } 
	
	/* Fix the user and group id's for the process */
	if ((uid != -1 || gid != -1) && getuid() == 0)
	{
	    if (uid == -1)
		uid = geteuid();
	    if (gid == -1)
		gid = getegid();
	    
	    seteuid(0);
	    setegid(getgid());
	    
	    if (gid != -1)
	    {
		s = setgid(gid);
		if (s < 0)
		{
		    err = "setgid()";
		    goto Fail;
		}
	    }
	    
	    if (uid != -1)
	    {
		s = setuid(uid);
		if (s < 0)
		{
		    err = "setuid()";
		    goto Fail;
		}
	    }
	}
	
	s_execve(rpath, argv, envp);
	
	err = "s_execve()";
	
      Fail:
	err2 = strerror(errno);
	s_write(2, "proc_run(\"", 10);
	s_write(2, rpath, strlen(rpath));
	s_write(2, "\") failed: ", 11);
	s_write(2, err, strlen(err));
	s_write(2, ": ", 2);
	s_write(2, err2, strlen(err2));
	s_write(2, "\r\n", 2);
	_exit(1);
    }
    
    return pid;
}


/* Returns 0 = normal exit(), 1 = Died because of a  Signal, 2 = Unknown */
int proc_wait(pid_t pid, int *code)
{
    int status;

    if (debug > 4)
	fprintf(stderr, "proc_wait(): Before s_waitpid()\n");
    
  Again:
    s_waitpid(pid, &status, 0);
    if (debug > 1)
	fprintf(stderr,
		"Process %d exited, status = %d\n",
		(int) pid,
		status);
    
    if (WIFEXITED(status))
    {
	/* Process called exit() */
	
	*code = WEXITSTATUS(status);
	return PROC_EXIT;
    }
    
    else if (WIFSIGNALED(status))
    {
	/* Process died with a Signal */
	
	*code = WTERMSIG(status);
	return PROC_SIGNAL;
    }
    else if (WIFSTOPPED(status) ||
	     WIFCONTINUED(status))
    {
	goto Again;
    }

    /* Unknown termination reason */
    return PROC_OTHER;
}
