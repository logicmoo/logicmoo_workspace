/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
			 CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*
This file is derived from ssh_server_fork.c from the libssh distributing
holding the following copyright.

Copyright 2014 Audrius Butkevicius

This file is part of the SSH Library

You are free to copy this file, modify  it in any way, consider it being
public domain. This does not apply to   the  rest of the library though,
but it is allowed to cut-and-paste working   code  from this file to any
license of program. The goal is to show the API in action.
*/

#include "config.h"

#define _GNU_SOURCE
#define PL_ARITY_AS_SIZE			/* get gettid() */
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <pthread.h>
#include <errno.h>
#include <unistd.h>

static atom_t ATOM_name;
static atom_t ATOM_port;
static atom_t ATOM_bind_address;
static atom_t ATOM_host_key_file;
static atom_t ATOM_authorized_keys_file;
static atom_t ATOM_auth_methods;
static atom_t ATOM_password;
static atom_t ATOM_public_key;

static int debugging = 0;

#define DEBUG(n,g) do { if ( debugging >= n ) { g; } } while(0)

#define PID_STARTING 0
#define PID_RUNNING  1
#define PID_DIED     2

#include <libssh/callbacks.h>
#include <libssh/server.h>

#include <poll.h>
#include <fcntl.h>
#ifdef HAVE_PTY_H
#include <pty.h>
#elif defined(HAVE_UTIL_H)
#include <util.h>
#endif
#include <signal.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <stdio.h>

#define BUF_SIZE 8192
#define PTY_NAME_MAX 64
#define SESSION_END (SSH_CLOSED | SSH_CLOSED_ERROR)
#define SFTP_SERVER_PATH "/usr/lib/sftp-server"

/* Keep track of a server */
typedef struct ssh_server
{ atom_t name;				/* Name of the server */
  char *authorizedkeys;			/* Authorized keys */
  int auth_methods;			/* SSH_AUTH_METHOD_* */
} ssh_server;

/* A userdata struct for session. */
struct session_data_struct
{ ssh_server *server;			/* overall server */
  const char *user;			/* Logged in user */
  ssh_channel channel;			/* Pointer to the session channel */
  int auth_attempts;
  int authenticated;
};

/* A userdata struct for channel. */
struct channel_data_struct
{  pid_t pid;				/* Client pid (psuedo: PID_*) */
   socket_t pty_master;			/* For PTY allocation */
   socket_t pty_slave;
   socket_t child_stdin;		/* communication with the child process */
   socket_t child_stdout;
   socket_t child_stderr;		/* subsystem and exec requests */
   ssh_event event;			/* Event used to poll above descriptors */
   struct winsize *winsize;		/* Terminal size struct. */
   struct session_data_struct *sdata;	/* Session data */
   pthread_t tid;			/* Thread handling this channel */
   int pltid;				/* Prolog thread handling this channel */
   int retcode;				/* Final return code */
   const char *term;			/* TERM variable */
   char pty_name[PTY_NAME_MAX];		/* Terminal name */
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handle input. We cannot use signalling as  provided by Unix terminals as
this only sends signals to the   controlling terminal. Therefore we scan
for ^C and send the ^C  directly   to  the  associated Prolog thread and
remove it from the input. This  is   not  completely correct, but should
deal with most interaction.

The `skipped` value ensures we claim to our caller that we processed the
^C.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
data_function(ssh_session session, ssh_channel channel, void *data,
	      uint32_t len, int is_stderr, void *userdata)
{ struct channel_data_struct *cdata = (struct channel_data_struct *) userdata;
  char *s = data;
  uint32_t i;
  uint32_t skipped = 0;
  ssize_t rc;

  (void)session;
  (void)channel;
  (void)is_stderr;

  if ( len == 0 || cdata->pid != PID_RUNNING )
    return 0;

  for(i=0; i<len; i++)
  { if ( s[i] == 3 )				/* ^C */
    { DEBUG(1, Sdprintf("Got ^C\n"));
      if ( cdata->pltid )
      { DEBUG(1, Sdprintf("Interrupt Prolog thread %d\n", cdata->pltid));
	PL_thread_raise(cdata->pltid, SIGINT);
	skipped++;
	memmove(&s[i], &s[i+1], len-i);
	len--;
	i--;
      }
    }
  }
  if ( len == 0 )
    return skipped;

  rc = write(cdata->child_stdin, data, len);
  if ( rc < 0 )
    return rc;
  else
    return rc+skipped;
}


static int
pty_request(ssh_session session, ssh_channel channel,
	    const char *term, int cols, int rows, int py, int px,
	    void *userdata)
{ struct channel_data_struct *cdata = (struct channel_data_struct *)userdata;

  (void) session;
  (void) channel;

  cdata->winsize->ws_row = rows;
  cdata->winsize->ws_col = cols;
  cdata->winsize->ws_xpixel = px;
  cdata->winsize->ws_ypixel = py;
  cdata->term = strdup(term);

  if ( openpty(&cdata->pty_master, &cdata->pty_slave, cdata->pty_name, NULL,
	       cdata->winsize) != 0)
  { Sdprintf("Failed to open pty\n");
    return SSH_ERROR;
  }

  return SSH_OK;
}

static int
pty_resize(ssh_session session, ssh_channel channel, int cols,
	   int rows, int py, int px, void *userdata)
{ struct channel_data_struct *cdata = (struct channel_data_struct *)userdata;

  (void) session;
  (void) channel;

  cdata->winsize->ws_row = rows;
  cdata->winsize->ws_col = cols;
  cdata->winsize->ws_xpixel = px;
  cdata->winsize->ws_ypixel = py;

  if ( cdata->pty_master != -1 )
    return ioctl(cdata->pty_master, TIOCSWINSZ, cdata->winsize);

  /* TBD: Send signal to the thread? */

  return SSH_ERROR;
}


typedef struct cmd_context
{ int	in;					/* I/O stream */
  int	out;					/* I/O stream */
  int	err;
  struct channel_data_struct *cdata;
  const char *command;				/* command to execute */
  int   pty;					/* Use a PTY? */
} cmd_context;


		 /*******************************
		 *	     STREAM		*
		 *******************************/

static ssize_t
Sssh_read(void *handle, char *buffer, size_t count)
{ int fd = (int)(intptr_t)handle;
  ssize_t bytes;

  Sflush(Suser_output);
  PL_write_prompt(TRUE);

  for(;;)
  {
#ifdef __WINDOWS__
    bytes = read(fd, buffer, (int)count);
#else
    bytes = read(fd, buffer, count);
#endif

    if ( bytes == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
      { errno = EPLEXCEPTION;
	return -1;
      }

      continue;
    }

    if ( bytes == 0 )
    { Sclearerr(Suser_input);
      PL_prompt_next(0);
    } else if ( bytes > 0 && buffer[bytes-1] == '\n' )
    { PL_prompt_next(0);
    }

    return bytes;
  }
}


static ssize_t
Sssh_write(void *handle, char *buffer, size_t count)
{ int fd = (int)(intptr_t)handle;
  ssize_t size;

  do
  { size = write(fd, buffer, count);

    if ( size < 0 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
      { errno = EPLEXCEPTION;
	return -1;
      }
    }
  } while(0);

  return size;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Normally we kill the xterm if all   file-descriptors  are closed. If the
thread  cannot  be  killed  however,   dieIO()    will   kill  only  the
file-descriptors that are not in use and may   fail to kill all of them.
Therefore we kill on the first occasion.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
Sssh_close(void *handle)
{ int fd = (int)(intptr_t)handle;
  int rc;

  do
  { rc = close(fd);
  } while ( rc == -1 && errno == EINTR );

  return rc;
}


static int
Sssh_control(void *handle, int action, void *arg)
{ int fd = (int)(intptr_t)handle;

  switch(action)
  { case SIO_SETENCODING:
    case SIO_FLUSHOUTPUT:
      return 0;
    case SIO_GETFILENO:
    { int *p = arg;
      *p = fd;
      return 0;
    }
    default:
      return -1;
  }
}


static IOFUNCTIONS Sssh_functions =
{ Sssh_read,
  Sssh_write,
  NULL,
  Sssh_close,
  Sssh_control
};


#define SIO_STDIO (SIO_FILE|SIO_NOCLOSE|SIO_TEXT|SIO_RECORDPOS)

static void *
run_command(void *ptr)
{ cmd_context *ctx = ptr;
  int sflags = ctx->pty ? SIO_ISATTY : 0;
  IOSTREAM *in  = Snew((void*)(intptr_t)ctx->in,
		       SIO_STDIO|SIO_LBUF|SIO_INPUT|SIO_NOFEOF|sflags,
		       &Sssh_functions);
  IOSTREAM *out = Snew((void*)(intptr_t)ctx->out,
		       SIO_STDIO|SIO_LBUF|SIO_OUTPUT|SIO_REPPL|sflags,
		       &Sssh_functions);
  IOSTREAM *err = Snew((void*)(intptr_t)ctx->err,
		       SIO_STDIO|SIO_NBUF|SIO_OUTPUT|SIO_REPPL|sflags,
		       &Sssh_functions);
  PL_thread_attr_t attr = { .flags = PL_THREAD_NO_DEBUG };
  const char *command = ctx->command ? ctx->command : "prolog";
  int pltid;

  out->position = in->position;
  err->position = in->position;

  if ( (pltid=PL_thread_attach_engine(&attr)) > 0 )
  { fid_t fid = PL_open_foreign_frame();
    term_t av = PL_new_term_refs(6);
    static predicate_t pred = 0;
    atom_t sname = ctx->cdata->sdata->server->name;

    if ( !pred )
      pred = PL_predicate("run_client", 6, "ssh_server");

    if ( ctx->cdata->term )
      PL_set_prolog_flag("ssh_term", PL_ATOM, ctx->cdata->term);
    if ( ctx->cdata->pty_name[0] )
      PL_set_prolog_flag("ssh_tty", PL_ATOM, ctx->cdata->pty_name);
    if ( ctx->cdata->sdata->user )
      PL_set_prolog_flag("ssh_user", PL_ATOM, ctx->cdata->sdata->user);

    if ( !sname )
      sname = ATOM_nil;

    ctx->cdata->pltid = pltid;
    if ( PL_unify_atom(av+0, sname) &&
	 PL_unify_stream(av+1, in) &&
	 PL_unify_stream(av+2, out) &&
	 PL_unify_stream(av+3, err) &&
	 PL_unify_chars(av+4, PL_ATOM, (size_t)-1, command) &&
	 PL_call_predicate(NULL, PL_Q_NORMAL, pred, av) )
    { DEBUG(1, Sdprintf("Prolog client done\n"));
      if ( !PL_get_integer(av+5, &ctx->cdata->retcode) )
	ctx->cdata->retcode = 0;
    } else
    { if ( PL_exception(0) )
	ctx->cdata->retcode = 2;
      else
	ctx->cdata->retcode = 1;
    }

    Sclose(in);
    Sclose(out);
    Sclose(err);
    PL_clear_exception();

    PL_close_foreign_frame(fid);
    ctx->cdata->pltid = 0;
    PL_thread_destroy_engine();
  } else
  { Sfprintf(err, "ERROR: Failed to create Prolog thread\n");
    Sclose(in);
    Sclose(out);
    Sclose(err);
  }

  ctx->cdata->pid = PID_DIED;
  if ( ctx->command )
    free((void*)ctx->command);
  free(ctx);

  return NULL;
}

static int
create_detached_thread(pthread_t *tp, void *(*start_routine) (void *), void *arg)
{ pthread_attr_t attr;
  int rc;

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  rc = pthread_create(tp, NULL, start_routine, arg);
  pthread_attr_destroy(&attr);

  return rc;
}


static int
exec_pty(const char *mode, const char *command,
	 struct channel_data_struct *cdata)
{ cmd_context *ctx = malloc(sizeof(*ctx));
  pthread_t tid;

  DEBUG(1, Sdprintf("Running %s with a TTY\n", command));

  memset(ctx, 0, sizeof(*ctx));
  ctx->in      = cdata->pty_slave;
  ctx->out     = cdata->pty_slave;
  ctx->err     = cdata->pty_slave;
  ctx->cdata   = cdata;
  ctx->command = command ? strdup(command) : NULL;
  ctx->pty     = TRUE;

  create_detached_thread(&tid, run_command, ctx);

  cdata->child_stdout = cdata->child_stdin = cdata->pty_master;
  cdata->tid = tid;
  cdata->pid = PID_RUNNING;

  return SSH_OK;
}

static int
exec_nopty(const char *command, struct channel_data_struct *cdata)
{ int in[2], out[2], err[2];
  cmd_context *ctx;
  pthread_t tid;

  DEBUG(1, Sdprintf("Running %s without a TTY\n", command));

  if ( strcmp(command, "abort") == 0 )
    abort();

  /* Do the plumbing to be able to talk with the child process. */
  if ( pipe(in) != 0 )
    goto stdin_failed;
  if ( pipe(out) != 0 )
    goto stdout_failed;
  if ( pipe(err) != 0 )
    goto stderr_failed;

  ctx = malloc(sizeof(*ctx));
  memset(ctx, 0, sizeof(*ctx));
  ctx->in      = in[0];
  ctx->out     = out[1];
  ctx->err     = err[1];
  ctx->cdata   = cdata;
  ctx->command = command ? strdup(command) : NULL;
  ctx->pty     = FALSE;

  if ( create_detached_thread(&tid, run_command, ctx) )
  { Sdprintf("pthread_create failed\n");
    goto fork_failed;
  }

  cdata->child_stdin = in[1];
  cdata->child_stdout = out[0];
  cdata->child_stderr = err[0];
  cdata->tid = tid;
  cdata->pid = PID_RUNNING;

  return SSH_OK;

fork_failed:					/* TBD: also close other side */
  close(err[0]);
  close(err[1]);
stderr_failed:
  close(out[0]);
  close(out[1]);
stdout_failed:
  close(in[0]);
  close(in[1]);
stdin_failed:
  return SSH_ERROR;
}

static int
exec_request(ssh_session session, ssh_channel channel,
	     const char *command, void *userdata)
{ struct channel_data_struct *cdata = (struct channel_data_struct *) userdata;

  (void) session;
  (void) channel;

  if ( cdata->pid > 0 )
    return SSH_ERROR;

  if ( cdata->pty_master != -1 && cdata->pty_slave != -1 )
    return exec_pty("-c", command, cdata);

  return exec_nopty(command, cdata);
}

static int
shell_request(ssh_session session, ssh_channel channel, void *userdata)
{ struct channel_data_struct *cdata = (struct channel_data_struct *) userdata;

  (void) session;
  (void) channel;

  if ( cdata->pid > 0 )
    return SSH_ERROR;

  if ( cdata->pty_master != -1 && cdata->pty_slave != -1 )
    return exec_pty("-l", NULL, cdata);

    /* Client requested a shell without a pty, let's pretend we allow that */
  return SSH_OK;
}

static int
env_request(ssh_session session, ssh_channel channel,
	    const char *env_name, const char *env_value,
	    void *userdata)
{ struct channel_data_struct *cdata = (struct channel_data_struct *) userdata;

  DEBUG(2, Sdprintf("ENV %s=%s\n", env_name, env_value));
  return SSH_OK;
}

static int
subsystem_request(ssh_session session, ssh_channel channel,
		  const char *subsystem, void *userdata)
{ /* subsystem requests behave simillarly to exec requests. */
  if ( strcmp(subsystem, "sftp") == 0 )
    return exec_request(session, channel, SFTP_SERVER_PATH, userdata);

  return SSH_ERROR;
}

static int
auth_password(ssh_session session, const char *user,
	      const char *pass, void *userdata)
{ struct session_data_struct *sdata = (struct session_data_struct *) userdata;
  PL_thread_attr_t attr = { .flags = PL_THREAD_NO_DEBUG };

  (void) session;

  if ( PL_thread_attach_engine(&attr) )
  { fid_t fid = PL_open_foreign_frame();
    term_t av = PL_new_term_refs(3);
    static predicate_t pred = 0;
    int accepted = FALSE;

    if ( !pred )
      pred = PL_predicate("verify_password", 3, "ssh_server");

    if ( PL_unify_atom(av+0, sdata->server->name ? sdata->server->name
						 : ATOM_nil) &&
	 PL_unify_chars(av+1, PL_ATOM,   (size_t)-1, user) &&
	 PL_unify_chars(av+2, PL_STRING, (size_t)-1, pass) &&
	 PL_call_predicate(NULL, PL_Q_NORMAL, pred, av) )
    { DEBUG(1, Sdprintf("Password accepted\n"));
      accepted = TRUE;
    } else
    { DEBUG(1, Sdprintf("Password rejected\n"));
    }

    PL_close_foreign_frame(fid);
    PL_thread_destroy_engine();

    if ( accepted )
    { sdata->authenticated = 1;
      sdata->user = strdup(user);
      return SSH_AUTH_SUCCESS;
    }
  }

  sdata->auth_attempts++;
  return SSH_AUTH_DENIED;
}


static int
auth_publickey(ssh_session session,
	       const char *user,
	       struct ssh_key_struct *pubkey,
	       char signature_state,
	       void *userdata)
{ struct session_data_struct *sdata = (struct session_data_struct *) userdata;

  (void) user;
  (void) session;

  if ( signature_state == SSH_PUBLICKEY_STATE_NONE )
    return SSH_AUTH_SUCCESS;

  if ( signature_state != SSH_PUBLICKEY_STATE_VALID )
    return SSH_AUTH_DENIED;

  // valid so far.  Now look through authorized keys for a match
  if ( sdata->server->authorizedkeys )
  { FILE *fd;

    DEBUG(1, Sdprintf("Trying authorized keys from %s\n",
		      sdata->server->authorizedkeys));

    if ( (fd=fopen(sdata->server->authorizedkeys, "r")) )
    { char blob[1024];


      while(fgets(blob, sizeof(blob), fd))
      { ssh_key key = NULL;
	char *from, *to;
	enum ssh_keytypes_e type = SSH_KEYTYPE_UNKNOWN;

	if ( (from = strchr(blob, ' ')) )
	{ *from++ = '\0';

	  if ( strcmp(blob, "ssh-rsa") == 0 )
	    type = SSH_KEYTYPE_RSA;
	  else if ( strcmp(blob, "ssh-ed25519") == 0 )
	    type = SSH_KEYTYPE_ED25519;
	  else
	  { DEBUG(1, Sdprintf("Unknown type = %s\n", blob));
	    continue;
	  }

	  while( *from == ' ' )
	    from++;
	  if ( *from )
	  { for(to=from; *to > ' '; to++)
	      ;
	    *to = '\0';

	    DEBUG(2, Sdprintf("Trying key [%s] %s\n", blob, from));

	    if ( ssh_pki_import_pubkey_base64(from, type, &key) == SSH_OK && key )
	    { int cmp = ssh_key_cmp(key, pubkey, SSH_KEY_CMP_PUBLIC);

	      DEBUG(2, Sdprintf("Key [%s] %s [%d]\n", blob, from, cmp));

	      ssh_key_free(key);

	      if ( cmp == 0 )
	      { sdata->authenticated = 1;
		fclose(fd);
		sdata->user = strdup(user);
		return SSH_AUTH_SUCCESS;
	      }
	    }
	  }
	} else
	{ DEBUG(1, Sdprintf("Invalid key: %s\n", blob));
	}
      }

      fclose(fd);
    }
  }

  // no matches
  sdata->authenticated = 0;
  return SSH_AUTH_DENIED;
}

static ssh_channel
channel_open(ssh_session session, void *userdata)
{ struct session_data_struct *sdata = (struct session_data_struct *) userdata;

  sdata->channel = ssh_channel_new(session);
  return sdata->channel;
}

static int
process_stdout(socket_t fd, int revents, void *userdata)
{ char buf[BUF_SIZE];
  int n = -1;
  ssh_channel channel = (ssh_channel) userdata;

  if (channel != NULL && (revents & POLLIN) != 0) {
    n = read(fd, buf, BUF_SIZE);
    if (n > 0) {
      ssh_channel_write(channel, buf, n);
    }
  }

  return n;
}

static int
process_stderr(socket_t fd, int revents, void *userdata)
{ char buf[BUF_SIZE];
  int n = -1;
  ssh_channel channel = (ssh_channel) userdata;

  if (channel != NULL && (revents & POLLIN) != 0) {
    n = read(fd, buf, BUF_SIZE);
    if (n > 0) {
      ssh_channel_write_stderr(channel, buf, n);
    }
  }

  return n;
}

static void
handle_session(ssh_event event, ssh_session session, ssh_server *server)
{ int n;

  /* Structure for storing the pty size. */
  struct winsize wsize =
  { .ws_row    = 0,
    .ws_col    = 0,
    .ws_xpixel = 0,
    .ws_ypixel = 0
  };

  /* Our struct holding information about the session. */
  struct session_data_struct sdata =
  { .server        = server,
    .channel       = NULL,
    .auth_attempts = 0,
    .authenticated = 0
  };

  /* Our struct holding information about the channel. */
  struct channel_data_struct cdata =
  { .pid	  = 0,
    .pty_master   = -1,
    .pty_slave    = -1,
    .child_stdin  = -1,
    .child_stdout = -1,
    .child_stderr = -1,
    .event        = NULL,
    .winsize      = &wsize,
    .sdata        = &sdata,
    .tid          = 0,
    .pltid        = 0,
    .term         = NULL
  };

  struct ssh_channel_callbacks_struct channel_cb =
  { .userdata                           = &cdata,
    .channel_pty_request_function       = pty_request,
    .channel_pty_window_change_function = pty_resize,
    .channel_shell_request_function     = shell_request,
    .channel_exec_request_function      = exec_request,
    .channel_data_function              = data_function,
    .channel_env_request_function       = env_request,
    .channel_subsystem_request_function = subsystem_request,
  };

  struct ssh_server_callbacks_struct server_cb =
  { .userdata				   = &sdata,
    .auth_password_function                = auth_password,
    .channel_open_request_session_function = channel_open,
  };

  ssh_set_auth_methods(session, server->auth_methods);
  if ( (server->auth_methods & SSH_AUTH_METHOD_PUBLICKEY) )
    server_cb.auth_pubkey_function = auth_publickey;

  ssh_callbacks_init(&server_cb);
  ssh_callbacks_init(&channel_cb);

  ssh_set_server_callbacks(session, &server_cb);

  if ( ssh_handle_key_exchange(session) != SSH_OK )
  { fprintf(stderr, "%s\n", ssh_get_error(session));
    return;
  }

  ssh_event_add_session(event, session);

  n = 0;
  while (sdata.authenticated == 0 || sdata.channel == NULL)
  { /* If the user has used up all attempts, or if he hasn't been able to
     * authenticate in 10 seconds (n * 100ms), disconnect. */
    if ( sdata.auth_attempts >= 3 || n >= 100 )
      return;

    if ( ssh_event_dopoll(event, 100) == SSH_ERROR )
    { fprintf(stderr, "%s\n", ssh_get_error(session));
      return;
    }
    n++;
  }

  ssh_set_channel_callbacks(sdata.channel, &channel_cb);

  do
  { /* Poll the main event which takes care of the session, the channel and
     * even our child process's stdout/stderr (once it's started). */
    if ( ssh_event_dopoll(event, -1) == SSH_ERROR )
       ssh_channel_close(sdata.channel);

    if ( ssh_channel_is_eof(sdata.channel) )
    { DEBUG(1, Sdprintf("Got EOF\n"));
      close(cdata.child_stdin);
      cdata.child_stdin = -1;
    }

    /* If child process's stdout/stderr has been registered with the event,
     * or the child process hasn't started yet, continue. */
    if ( cdata.event != NULL || cdata.pid == 0 )
      continue;

    /* Executed only once, once the child process starts. */
    cdata.event = event;
    /* If stdout valid, add stdout to be monitored by the poll event. */
    if ( cdata.child_stdout != -1 )
    { if ( ssh_event_add_fd(event, cdata.child_stdout, POLLIN, process_stdout,
			    sdata.channel) != SSH_OK)
      { fprintf(stderr, "Failed to register stdout to poll context\n");
	ssh_channel_close(sdata.channel);
      }
    }

    /* If stderr valid, add stderr to be monitored by the poll event. */
    if ( cdata.child_stderr != -1 )
    { if ( ssh_event_add_fd(event, cdata.child_stderr, POLLIN, process_stderr,
			    sdata.channel) != SSH_OK)
      { fprintf(stderr, "Failed to register stderr to poll context\n");
	ssh_channel_close(sdata.channel);
      }
    }
  } while(ssh_channel_is_open(sdata.channel) &&
	  cdata.pid < PID_DIED );

  if ( cdata.pty_master >= 0 ) close(cdata.pty_master);
  if ( cdata.child_stdin > 0 ) close(cdata.child_stdin);
  close(cdata.child_stdout);
  close(cdata.child_stderr);

  if ( cdata.term ) free((void*)cdata.term);
  if ( sdata.user ) free((void*)sdata.user);

  /* Remove the descriptors from the polling context, since they are now
   * closed, they will always trigger during the poll calls. */
  ssh_event_remove_fd(event, cdata.child_stdout);
  ssh_event_remove_fd(event, cdata.child_stderr);

  /* How should we use this? Was the exit status of the child process */
  ssh_channel_request_send_exit_status(sdata.channel, cdata.retcode);
  ssh_channel_send_eof(sdata.channel);
  ssh_channel_close(sdata.channel);

  /* Wait up to 5 seconds for the client to terminate the session. */
  for (n = 0; n < 50 && (ssh_get_status(session) & SESSION_END) == 0; n++)
    ssh_event_dopoll(event, 100);
}

typedef struct start_session
{ ssh_session session;
  ssh_server *server;
} start_session;


static void *
run_session(void *ptr)
{ start_session *ss = ptr;
  ssh_event event;

  if ( (event = ssh_event_new()) )
  { handle_session(event, ss->session, ss->server);
    ssh_event_free(event);
  } else
  { PL_warning("Could not create polling context\n");
  }

  ssh_disconnect(ss->session);
  ssh_free(ss->session);
  free(ss);

  return NULL;
}


static void
start_session_thread(start_session *ss)
{ pthread_t tid;

  create_detached_thread(&tid, run_session, ss);
}

#define INST_HOST_KEY 0x0001
#define INST_BINDADDR 0x0002
#define INST_PORT     0x0004
#define INST_AUTHKEYS 0x0008

#define INST_NEEDED (INST_HOST_KEY|INST_BINDADDR|INST_PORT)

static foreign_t
pl_ssh_server(term_t options)
{ ssh_bind sshbind;
  ssh_session session;
  ssh_server server = {0};
  int rc;
  term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg  = PL_new_term_ref();
  int installed = 0;

  rc = ssh_init();
  if ( rc < 0 )
    return PL_warning("ssh_init failed\n");

  sshbind = ssh_bind_new();
  if (sshbind == NULL)
    return PL_warning("ssh_bind_new failed\n");

  while( PL_get_list_ex(tail, head, tail) )
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(head, &name, &arity) && arity == 1 )
    { _PL_get_arg(1, head, arg);

      if ( name == ATOM_name )
      { atom_t a;

	if ( !PL_get_atom_ex(arg, &a) )
	  return FALSE;

	PL_register_atom(a);
	server.name = a;
      } else if ( name == ATOM_port )
      { int port;

	if ( !PL_get_integer_ex(arg, &port) )
	  return FALSE;

	ssh_bind_options_set(sshbind, SSH_BIND_OPTIONS_BINDPORT, &port);
	installed |= INST_PORT;
      } else if ( name == ATOM_bind_address )
      { char *adr;

	if ( !PL_get_chars(arg, &adr, REP_MB|CVT_ATOMIC|CVT_EXCEPTION) )
	  return FALSE;
	ssh_bind_options_set(sshbind, SSH_BIND_OPTIONS_BINDADDR, adr);
	installed |= INST_BINDADDR;
      } else if ( name == ATOM_host_key_file )
      { char *adr;

	if ( !PL_get_chars(arg, &adr, REP_MB|CVT_ATOMIC|CVT_EXCEPTION) )
	  return FALSE;
	ssh_bind_options_set(sshbind, SSH_BIND_OPTIONS_HOSTKEY, adr);
	DEBUG(1, Sdprintf("Set host key to %s\n", adr));
	installed |= INST_HOST_KEY;
      } else if ( name == ATOM_authorized_keys_file )
      { char *adr;

	if ( !PL_get_chars(arg, &adr, REP_MB|CVT_ATOMIC|CVT_EXCEPTION) )
	  return FALSE;
	DEBUG(1, Sdprintf("Set authorizedkeys file to %s\n", adr));
	server.authorizedkeys = strdup(adr);
	installed |= INST_AUTHKEYS;
      } else if ( name == ATOM_auth_methods )
      { term_t tail2 = PL_copy_term_ref(arg);
	term_t head2 = PL_new_term_ref();

	while( PL_get_list_ex(tail2, head2, tail2) )
	{ atom_t a;

	  if ( !PL_get_atom_ex(head2, &a) )
	    return FALSE;
	  if ( a == ATOM_password )
	    server.auth_methods |= SSH_AUTH_METHOD_PASSWORD;
	  else if ( a == ATOM_public_key )
	    server.auth_methods |= SSH_AUTH_METHOD_PUBLICKEY;
	}
	if ( !PL_get_nil_ex(tail2) )
	  return FALSE;
      }
    } else
    { return PL_type_error("option", head);
    }
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( (installed & INST_NEEDED) != INST_NEEDED )
    return PL_warning("Missing options (got 0x%x)\n", installed);

  if ( ssh_bind_listen(sshbind) < 0 )
    return PL_warning("%s\n", ssh_get_error(sshbind));

  while (TRUE)
  { session = ssh_new();
    if ( !session )
    { Sdprintf("Failed to allocate session\n");
      continue;
    }

    /* Blocks until there is a new incoming connection. */
    if ( ssh_bind_accept(sshbind, session) != SSH_ERROR )
    { start_session *ss = malloc(sizeof(*ss));

      memset(ss, 0, sizeof(*ss));
      ss->session = session;
      ss->server  = &server;

      start_session_thread(ss);
    } else
    { ssh_disconnect(session);
      ssh_free(session);
      if ( PL_handle_signals() < 0 )
	break;
    }
  }

  ssh_bind_free(sshbind);
  ssh_finalize();

  return FALSE;
}


static foreign_t
pl_ssh_debug(term_t old, term_t new)
{ return ( PL_unify_integer(old, debugging) &&
	   PL_get_integer_ex(new, &debugging) );
}


		 /*******************************
		 *	   INITIALIZAION	*
		 *******************************/

#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n)

install_t
install_sshd4pl(void)
{ MKATOM(name);
  MKATOM(port);
  MKATOM(bind_address);
  MKATOM(host_key_file);
  MKATOM(authorized_keys_file);
  MKATOM(auth_methods);
  MKATOM(password);
  MKATOM(public_key);

  PL_register_foreign("ssh_server_nt", 1, pl_ssh_server, 0);
  PL_register_foreign("ssh_debug",     2, pl_ssh_debug,  0);
}
