#!/usr/bin/env python3

"""Test of test.proto, test_interop.pl"""

import os
import unittest
from test_pb2 import Scalars1, MyEnum, Repeated1, Packed1

class TestScalar(unittest.TestCase):

    def test_scalar1_enum(self):
        self.assertEqual(0, MyEnum.E1)  # crashes with 3.6.1 -- needs 3.8.0 or later
        self.assertEqual(1, MyEnum.Enum2)
        self.assertEqual(2, MyEnum.AnotherEnum)

    # TODO: also test scalars1a_from_prolog_template.wire, etc.

    def test_scalar1a(self):
        m1a = Scalars1()
        with open("scalars1a_from_prolog.wire", "rb") as f:
            m1a.ParseFromString(f.read())
        self.assertEqual(m1a.v_double,    1.5)
        self.assertEqual(m1a.v_float,     2.5)
        self.assertEqual(m1a.v_int32,     3)
        self.assertEqual(m1a.v_int64,     4)
        self.assertEqual(m1a.v_uint32,    5)
        self.assertEqual(m1a.v_uint64,    6)
        self.assertEqual(m1a.v_sint32,    7)
        self.assertEqual(m1a.v_sint64,    8)
        self.assertEqual(m1a.v_fixed32,   9)
        self.assertEqual(m1a.v_fixed64,   10)
        self.assertEqual(m1a.v_sfixed32,  11)
        self.assertEqual(m1a.v_sfixed64,  12)
        self.assertEqual(m1a.v_bool,      False)
        self.assertEqual(m1a.v_string,    "écran 網目錦蛇")
        self.assertEqual(m1a.v_bytes,     b"\xc3\x28")
        self.assertEqual(m1a.v_enum,      MyEnum.E1)

    def test_scalar1b(self):
        m1b = Scalars1()
        with open("scalars1b_from_prolog.wire", "rb") as f:
            m1b.ParseFromString(f.read())
        self.assertEqual(m1b.v_double,    -1.5)
        self.assertEqual(m1b.v_float,     -2.5)
        self.assertEqual(m1b.v_int32,     -3)
        self.assertEqual(m1b.v_int64,     -4)
        self.assertEqual(m1b.v_uint32,     5+10000000)
        self.assertEqual(m1b.v_uint64,     6+10000000)
        self.assertEqual(m1b.v_sint32,    -7)
        self.assertEqual(m1b.v_sint64,    -8)
        self.assertEqual(m1b.v_fixed32,    9+1000)
        self.assertEqual(m1b.v_fixed64,   10+1000)
        self.assertEqual(m1b.v_sfixed32, -11)
        self.assertEqual(m1b.v_sfixed64, -12)
        self.assertEqual(m1b.v_bool,      True)
        self.assertEqual(m1b.v_string,    "[àmímé níshíkíhéꜜbì] reticulated python")
        self.assertEqual(m1b.v_bytes,     b"\xf0\x28\x8c\x28")  # See https://stackoverflow.com/questions/1301402/example-invalid-utf8-string
        self.assertEqual(m1b.v_enum,      MyEnum.AnotherEnum)

    def test_repeated1a(self):
        r1a = Repeated1()
        with open("repeated1a_from_prolog.wire", "rb") as f:
            r1a.ParseFromString(f.read())
        self.assertEqual(r1a.v_double,   [ 1.5, 0.0, -1.5])
        self.assertEqual(r1a.v_float,    [ 2.5, 0.0, -2.5])
        self.assertEqual(r1a.v_int32,    [ 3, -3, 555, 0, 2147483647, -2147483648])
        self.assertEqual(r1a.v_int64,    [ 4, -4, 0, 9223372036854775807, -9223372036854775808])
        self.assertEqual(r1a.v_uint32,   [ 5, 0, 4294967295])
        self.assertEqual(r1a.v_uint64,   [ 6, 7, 8, 9, 0, 18446744073709551615])
        self.assertEqual(r1a.v_sint32,   [ 7, -7, 0, 2147483647, -2147483648])
        self.assertEqual(r1a.v_sint64,   [ -8, 8, 0, 9223372036854775807, -9223372036854775808])
        self.assertEqual(r1a.v_fixed32,  [ 9, 0, 4294967295])
        self.assertEqual(r1a.v_fixed64,  [10, 0, 18446744073709551615])
        self.assertEqual(r1a.v_sfixed32, [-11, 11, 0, 2147483647, -2147483648])
        self.assertEqual(r1a.v_sfixed64, [-12, 12, 0, 9223372036854775807, -9223372036854775808])
        self.assertEqual(r1a.v_bool,     [False, True])
        self.assertEqual(r1a.v_string,   ["écran 網目錦蛇", "Hello world"])
        self.assertEqual(r1a.v_bytes,    [b"\xc3\x28", b"\x00\x01\x02"])
        self.assertEqual(r1a.v_enum,     [MyEnum.E1, MyEnum.Enum2, MyEnum.E1])
        kv = r1a.v_key_value
        self.assertEqual(len(kv), 2)
        self.assertEqual(kv[0].key, "foo")
        self.assertEqual(kv[0].value, "")
        self.assertEqual(kv[1].key, "àmímé níshíkíhéꜜbì")
        self.assertEqual(kv[1].value, "reticulated python")

    def test_packed1a(self):
        p1a = Packed1()
        with open("packed1a_from_prolog.wire", "rb") as f:
            p1a.ParseFromString(f.read())
        self.assertEqual(p1a.v_double,   [ 1.5, 0.0, -1.5])
        self.assertEqual(p1a.v_float,    [ 2.5, 0.0, -2.5])
        self.assertEqual(p1a.v_int32,    [ 3, -3, 555, 0, 2147483647, -2147483648])
        self.assertEqual(p1a.v_int64,    [ 4, -4, 0, 9223372036854775807, -9223372036854775808])
        self.assertEqual(p1a.v_uint32,   [ 5, 0, 4294967295])
        self.assertEqual(p1a.v_uint64,   [ 6, 7, 8, 9, 0, 18446744073709551615])
        self.assertEqual(p1a.v_sint32,   [ 7, -7, 0, 2147483647, -2147483648])
        self.assertEqual(p1a.v_sint64,   [ -8, 8, 0, 9223372036854775807, -9223372036854775808])
        self.assertEqual(p1a.v_fixed32,  [ 9, 0, 4294967295])
        self.assertEqual(p1a.v_fixed64,  [10, 0, 18446744073709551615])
        self.assertEqual(p1a.v_sfixed32, [-11, 11, 0, 2147483647, -2147483648])
        self.assertEqual(p1a.v_sfixed64, [-12, 12, 0, 9223372036854775807, -9223372036854775808])
        self.assertEqual(p1a.v_bool,     [False, True])
        self.assertEqual(p1a.v_string,   ["écran 網目錦蛇", "Hello world"])
        self.assertEqual(p1a.v_bytes,    [b"\xc3\x28", b"\x00\x01\x02"])
        self.assertEqual(p1a.v_enum,     [MyEnum.E1, MyEnum.Enum2, MyEnum.E1])
        kv = p1a.v_key_value
        self.assertEqual(len(kv), 2)
        self.assertEqual(kv[0].key, "foo")
        self.assertEqual(kv[0].value, "")
        self.assertEqual(kv[1].key, "àmímé níshíkíhéꜜbì")
        self.assertEqual(kv[1].value, "reticulated python")


if __name__ == "__main__":
    unittest.main()
