class ns__Object
{ public:
  char *name;
  ns__Object();
  ns__Object(const char *name);
  virtual ~ns__Object();
  virtual void print();
};
class ns__Shape: public ns__Object
{ public:
  char *name; // test overriding, both the ns__Object:name and ns__Shape:name are encoded
  int sides;
  ns__Shape();
  ns__Shape(const char *name, int sides);
  virtual ~ns__Shape();
  virtual void print();
};
class ns__Square: public ns__Shape
{ public:
  char *name; // test overriding
  static const int sides = 4; // will not be endoded and decoded
  int size;
  ns__Square();
  ns__Square(const char *name, int size);
  virtual ~ns__Square();
  virtual void print();
};
class ns__List: public ns__Object // ns__List is a dynamic array
{ public:
  ns__Object **__ptr; // array of pointers to objects
  int __size;
  ns__List();
  ns__List(int size);
  virtual ~ns__List();
  virtual ns__Object*& operator[](int i);
  virtual void print();
};
ns__polytest(ns__Object *in, struct ns__polytestResponse { ns__Object *out; } &result);
