// ...

#undef NDEBUG
#include <assert.h>
#include <iostream>
#include <fstream>
#include <string>
#include "test.pb.h"


void read_wire(const std::string& path, test::Scalars1* scalars1) {
  std::ifstream file1;
  file1.open(path);
  scalars1->ParseFromIstream(&file1);
  file1.close();
}

bool read_scalars1a() {
  test::Scalars1 scalars1a;
  read_wire("scalars1a_from_prolog.wire", &scalars1a);
  assert(scalars1a.v_double()   ==  1.5);
  assert(scalars1a.v_float()    ==  2.5);
  assert(scalars1a.v_int32()    ==  3);
  assert(scalars1a.v_int64()    ==  4);
  assert(scalars1a.v_uint32()   ==  5);
  assert(scalars1a.v_uint64()   ==  6);
  assert(scalars1a.v_sint32()   ==  7);
  assert(scalars1a.v_sint64()   ==  8);
  assert(scalars1a.v_fixed32()  ==  9);
  assert(scalars1a.v_fixed64()  == 10);
  assert(scalars1a.v_sfixed32() == 11);
  assert(scalars1a.v_sfixed64() == 12);
  assert(scalars1a.v_bool()     ==  false);
  assert(scalars1a.v_string()   == "écran 網目錦蛇");
  assert(scalars1a.v_bytes()    == "\xc3\x28");  // See https://stackoverflow.com/questions/1301402/example-invalid-utf8-string
  assert(scalars1a.v_enum()     == test::MyEnum::E1);
  return true;
}

bool read_scalars1b() {
  test::Scalars1 scalars1b;
  read_wire("scalars1b_from_prolog.wire", &scalars1b);
  // std::cerr << scalars1b.DebugString() << std::endl;
  assert(scalars1b.v_double()   ==  -1.5);
  assert(scalars1b.v_float()    ==  -2.5);
  assert(scalars1b.v_int32()    ==  -3);
  assert(scalars1b.v_int64()    ==  -4);
  assert(scalars1b.v_uint32()   ==  5+10000000);
  assert(scalars1b.v_uint64()   ==  6+10000000);
  assert(scalars1b.v_sint32()   ==  -7);
  assert(scalars1b.v_sint64()   ==  -8);
  assert(scalars1b.v_fixed32()  ==   9+1000);
  assert(scalars1b.v_fixed64()  ==  10+1000);
  assert(scalars1b.v_sfixed32() == -11);
  assert(scalars1b.v_sfixed64() == -12);
  assert(scalars1b.v_bool()     ==  true);
  assert(scalars1b.v_string()   == "[àmímé níshíkíhéꜜbì] reticulated python");
  assert(scalars1b.v_bytes()    == "\xf0\x28\x8c\x28");  // See https://stackoverflow.com/questions/1301402/example-invalid-utf8-string
  assert(scalars1b.v_enum()     ==  test::MyEnum::AnotherEnum);
  return true;
}

int test_read() {
  if (read_scalars1a() && read_scalars1b()) {
    return 0;
  } else {
    return 1;
  }
}


int main(int argc, char* argv[]) {
  GOOGLE_PROTOBUF_VERIFY_VERSION;
  return test_read();
}

