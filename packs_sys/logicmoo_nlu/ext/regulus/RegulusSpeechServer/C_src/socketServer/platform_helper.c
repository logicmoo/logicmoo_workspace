
int strcmp_case_insensitive(const char *s1, const char *s2){
#ifdef WIN32
  return _stricmp(s1, s2);
#else
  return strcasecmp(s1, s2);
#endif
}

#ifdef WIN32

#else

HANDLE CreateThread(void *x1, int x2, void *(*start_routine)(void *) , void *thread_arg, int x5, void *x6){
	pthread_t tid;
	int result = pthread_create(&tid, NULL, start_routine, thread_arg);

	if(result != 0){
	  string error_message = "Could not create thread: ";
	  error_message += strerror(errno);
	  cerr << error_message << endl;
	  return NULL;
	}
	return &tid;
}

void InitializeCriticalSection(CRITICAL_SECTION *cs){
  if(mutex_init(cs, USYNC_PROCESS_ROBUST, NULL) != 0){
    string error_message = "Could not initialize critical section: ";
    error_message += strerror(errno);
    cerr << error_message << endl;
    cs = NULL;
  }
}

void DestroyCriticalSection(CRITICAL_SECTION *cs){
  if(mutex_destroy(cs) != 0){
    string error_message = "Could not destroy critical section: ";
    error_message += strerror(errno);
    cerr << error_message << endl;
  }

}


void EnterCriticalSection(CRITICAL_SECTION *cs){
  if(mutex_lock(cs) != 0){
    string error_message = "Could not enter critical section: ";
    error_message += strerror(errno);
    cerr << error_message << endl;
  }
}

void LeaveCriticalSection(CRITICAL_SECTION *cs){
  if(mutex_unlock(cs) != 0){
    string error_message = "Could not leave critical section: ";
    error_message += strerror(errno);
    cerr << error_message << endl;
  }
}

void closesocket(int socket){
  close(socket);
}

#endif
