// Not really a test - mainly to see how negative numbers are
// handled for int32, int64 protobuf fields

#include <iostream>
#include <fstream>
#include <string>
#include "test.pb.h"

void write_wire(const std::string& path, const test::Scalars1 scalars1) {
  std::ofstream file1;
  file1.open(path);
  scalars1.SerializeToOstream(&file1);
  file1.close();
}

void make_scalars1a(test::Scalars1 *scalars1a) {
  scalars1a->set_v_double(     1.5);
  scalars1a->set_v_float(      2.5);
  scalars1a->set_v_int32(      3);
  scalars1a->set_v_int64(      4);
  scalars1a->set_v_uint32(     5);
  scalars1a->set_v_uint64(     6);
  scalars1a->set_v_sint32(     7);
  scalars1a->set_v_sint64(     8);
  scalars1a->set_v_fixed32(    9);
  scalars1a->set_v_fixed64(   10);
  scalars1a->set_v_sfixed32(  11);
  scalars1a->set_v_sfixed64(  12);
  scalars1a->set_v_bool(      false);
  scalars1a->set_v_string(    "écran 網目錦蛇");
  scalars1a->set_v_bytes(     "\xc3\x28");  // See https://stackoverflow.com/questions/1301402/example-invalid-utf8-string
  scalars1a->set_v_enum(      test::MyEnum::E1);
  scalars1a->set_v_utf8_codes("écran 網目錦蛇");
  scalars1a->mutable_v_key_value()->set_key("reticulated python");
  scalars1a->mutable_v_key_value()->set_value("網目錦蛇");
}

void make_scalars1b(test::Scalars1 *scalars1b) {
  scalars1b->set_v_double(     -1.5);
  scalars1b->set_v_float(      -2.5);
  scalars1b->set_v_int32(      -3);
  scalars1b->set_v_int64(      -4);
  scalars1b->set_v_uint32(      5+10000000);
  scalars1b->set_v_uint64(      6+10000000);
  scalars1b->set_v_sint32(     -7);
  scalars1b->set_v_sint64(     -8);
  scalars1b->set_v_fixed32(     9+1000);
  scalars1b->set_v_fixed64(    10+1000);
  scalars1b->set_v_sfixed32(  -11);
  scalars1b->set_v_sfixed64(  -12);
  scalars1b->set_v_bool(      true);
  scalars1b->set_v_string(    "[àmímé níshíkíhéꜜbì] reticulated python");
  scalars1b->set_v_bytes(     "\xf0\x28\x8c\x28");  // See https://stackoverflow.com/questions/1301402/example-invalid-utf8-string
  scalars1b->set_v_enum(      test::MyEnum::AnotherEnum);
  scalars1b->set_v_utf8_codes("[àmímé níshíkíhéꜜbì] reticulated python");
  scalars1b->mutable_v_key_value()->set_key("foo");
  scalars1b->mutable_v_key_value()->set_value("");
}

void make_scalars1c(test::Scalars1 *scalars1c) {
}

void test_write() {
  test::Scalars1 scalars1a;
  make_scalars1a(&scalars1a);
  write_wire("scalars1a_from_cc.wire", scalars1a);

  test::Scalars1 scalars1b;
  make_scalars1b(&scalars1b);
  // std::cout << scalars1b.DebugString() << std::endl;
  write_wire("scalars1b_from_cc.wire", scalars1b);

  test::Scalars1 scalars1c;
  make_scalars1c(&scalars1c);
  write_wire("scalars1c_from_cc.wire", scalars1c);
}

int main(int argc, char* argv[]) {
  GOOGLE_PROTOBUF_VERIFY_VERSION;
  test_write();
  return 0;
}
