#include <iostream>

using namespace std;

class plsbuf : public streambuf {
private:
    io_stream *pls;
    int release;
public:
    plsbuf(IOSTREAM *plstream, bool rel=0)
   : pls(plstream), release(rel) {}
    
    ~plsbuf() {
   if (release && pls)
       PL_release_stream(pls);
    }
    
    int_type underflow() {
   return EOF;
    }
    
    int_type uflow() {
   return Sgetc(pls);
    }
    
    int overflow(int ch=EOF) {
   return Sputc(ch, pls) <0 ? EOF : 0;
    }
    
    inline int sync() {
   return 0;
    }

    streamsize xsputn(char *data, streamsize n) {
   return Sfwrite(data, 1, n, pls);
    }

    streamsize xsgetn(char* data, streamsize n) {
   return Sfread(data, 1, n, pls);
    }
};

class oplstream : public ostream {
private:
    plsbuf buf;
public:
    oplstream(IOSTREAM *pls, int release=1)
   : ostream(NULL), buf(pls, release) { init(&buf); }
};

class iplstream : public istream {
private:
    plsbuf buf;
public:
    iplstream(IOSTREAM *pls, int release=1)
   : istream(NULL), buf(pls, release) { init(&buf); }
};

class ioplstream : public iostream {
private:
    plsbuf buf;
public:
    ioplstream(IOSTREAM *pls, int release=1)
   : iostream(NULL), buf(pls, release) { init(&buf); }
};

inline foreign_t PL_get_istream(term_t t0, istream **s) {
    IOSTREAM *tmp;
    // if (PL_get_nil(t0)) {
    // *s=NULL;
    // PL_succeed;
    // }
    if (PL_get_stream_handle(t0, &tmp)) {
   *s=new iplstream(tmp, 1);
   PL_succeed;
    }
    PL_fail;
}

inline foreign_t PL_get_ostream(term_t t0, ostream **s) {
    IOSTREAM *tmp;
    // if (PL_get_nil(t0)) {
    // *s=NULL;
    // PL_succeed;
    // }
    if (PL_get_stream_handle(t0, &tmp)) {
   *s=new oplstream(tmp, 1);
   PL_succeed;
    }
    PL_fail;
}

inline foreign_t PL_get_iostream(term_t t0, iostream **s) {
    IOSTREAM *tmp;
    // if (PL_get_nil(t0)) {
    // *s=NULL;
    // PL_succeed;
    //}
    if (PL_get_stream_handle(t0, &tmp)) {
   *s=new ioplstream(tmp, 1);
   PL_succeed;
    }
    PL_fail;
}
