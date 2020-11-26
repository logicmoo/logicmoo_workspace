typedef int xsd__int;

class vector
{ public:
  xsd__int *__ptr;
  int __size;
  struct soap *soap;
  vector();
  vector(int n);
  ~vector();
  void resize(int n);
  int& operator[](int i);
};

class matrix
{ public:
  vector *__ptr;
  int __size;
  struct soap *soap;
  matrix();
  matrix(int n, int m);
  ~matrix();
  void resize(int n, int m);
  vector& operator[](int i);
};

int ns1__magic(xsd__int rank, matrix *result);
