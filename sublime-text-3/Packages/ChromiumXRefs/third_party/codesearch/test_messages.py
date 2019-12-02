# Copyright 2017 The Chromium Authors.
#
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file or at
# https://developers.google.com/open-source/licenses/bsd.

from __future__ import absolute_import

import unittest

from .messages import Message, message, XrefSignature, InternalLink, TextRange


@message
class Foo(Message):
  DESCRIPTOR = {'x': int}


class Bar(Message):
  DESCRIPTOR = {'x': int, 'y': Message.PARENT_TYPE}


class Baz(Message):
  DESCRIPTOR = {'x': int, 'y': [Message.PARENT_TYPE]}


class Qux(Message):
  FOO = 1
  BAR = 2

  DESCRIPTOR = int


class Quux(Message):
  DESCRIPTOR = {'x': Qux}


class S(Message):
  DESCRIPTOR = {'s': str}


class TestProto(unittest.TestCase):

  def test_proto_bridge(self):
    v = Foo()
    v.x = 3
    self.assertEqual(v.AsQueryString(), [('x', '3')])

  def test_from_json_string_1(self):
    v = Message.FromJsonString('{"x": 3}')
    self.assertEqual(v.x, 3)

  def test_from_json_string_2(self):
    v = Foo.FromJsonString('{"x": 3}')
    self.assertTrue(isinstance(v, Foo))
    self.assertTrue(isinstance(v.x, int))
    self.assertEqual(v.x, 3)

  def test_from_json_string_3(self):
    v = Bar.FromJsonString('{"x": 3, "y": {"x": 4}}')
    self.assertTrue(isinstance(v, Bar))
    self.assertTrue(isinstance(v.y, Bar))
    self.assertEqual(v.x, 3)
    self.assertEqual(v.y.x, 4)

  def test_from_json_string_4(self):
    v = Foo.FromJsonString('{"y": 3}')
    self.assertTrue(isinstance(v, Foo))

  def test_from_json_string_5(self):
    v = Foo.FromJsonString('{"y": 3}')
    self.assertTrue(isinstance(v, Foo))
    self.assertEqual(v.y, 3)

  def test_from_json_string_6(self):
    v = Quux.FromJsonString('{"x": 3}')
    self.assertTrue(isinstance(v, Quux))
    self.assertTrue(isinstance(v.x, int))
    self.assertEqual(v.x, 3)

  def test_from_json_string_7(self):
    v = Quux.FromJsonString('{"x": "FOO"}')
    self.assertTrue(isinstance(v, Quux))
    self.assertTrue(isinstance(v.x, int))
    self.assertEqual(v.x, 1)

  def test_from_json_string_invalid(self):
    s = bytearray('{"s": "abcdefghijklmnop"}'.encode('utf-8'))
    s[8] = 0xf8
    v = S.FromJsonString(s)
    self.assertTrue(isinstance(v, S))
    self.assertGreater(len(v.s), 0)

  def test_from_shallow_dict_1(self):
    v = Baz.FromShallowDict({'x': 3, 'y': [{'x': 4}, {'x': 5}]})
    self.assertTrue(isinstance(v, Baz))
    self.assertTrue(isinstance(v.y, list))
    self.assertTrue(isinstance(v.y[0], Baz))
    self.assertTrue(isinstance(v.y[1], Baz))


class TestConstructor(unittest.TestCase):

  def test_empty_class(self):
    f = Foo()
    self.assertFalse(hasattr(f, 'x'))

  def test_class_with_known_keyword(self):
    f = Foo(x=10)
    self.assertTrue(hasattr(f, 'x'))
    self.assertEqual(10, f.x)

  def test_class_with_unknown_keyword(self):
    f = Foo(x=10, y=9)
    self.assertTrue(hasattr(f, 'x'))
    self.assertTrue(hasattr(f, 'y'))
    self.assertEqual(9, f.y)


class TestXrefSignature(unittest.TestCase):

  def test_basic_with_single_signature(self):
    xsig = XrefSignature.FromJsonString(
        '{"highlight_signature":"abc", "signature":"sig","signature_hash":"hash"}'
    )
    self.assertEqual('abc', xsig.highlight_signature)
    self.assertEqual('sig', xsig.signature)
    self.assertEqual('hash', xsig.signature_hash)
    self.assertSetEqual(set(['abc', 'sig']), set(xsig.GetSignatures()))
    self.assertEqual('sig', xsig.GetSignature())

  def test_multi_strings(self):
    xsig = XrefSignature.FromJsonString('''{
        "signature": "foo bar baz",
        "highlight_signature": "hifoo hibar"
      }''')
    self.assertSetEqual(
        set(['foo', 'bar', 'baz', 'hifoo', 'hibar']), set(xsig.GetSignatures()))
    self.assertEqual('foo', xsig.GetSignature())


class TestInternalLink(unittest.TestCase):

  def test_basic_with_single_signature(self):
    ilink = InternalLink.FromJsonString(
        '{"highlight_signature":"abc", "signature":"sig","signature_hash":"hash"}'
    )
    self.assertEqual('abc', ilink.highlight_signature)
    self.assertEqual('sig', ilink.signature)
    self.assertEqual('hash', ilink.signature_hash)
    self.assertSetEqual(set(['abc', 'sig']), set(ilink.GetSignatures()))
    self.assertEqual('sig', ilink.GetSignature())

  def test_multi_strings(self):
    ilink = InternalLink.FromJsonString('''{
        "signature": "foo bar baz",
        "highlight_signature": "hifoo hibar"
      }''')
    self.assertSetEqual(
        set(['foo', 'bar', 'baz', 'hifoo', 'hibar']),
        set(ilink.GetSignatures()))
    self.assertEqual('foo', ilink.GetSignature())


class TestTextRange(unittest.TestCase):

  def test_contains(self):
    r = TextRange(start_line=1, start_column=8, end_line=3, end_column=1)
    self.assertTrue(r.Contains(1, 8))
    self.assertTrue(r.Contains(3, 1))
    self.assertTrue(r.Contains(2, 100))
    self.assertFalse(r.Contains(1, 7))
    self.assertFalse(r.Contains(3, 2))

  def test_overlaps(self):

    def _QuadToRange(q):
      return TextRange(
          start_line=q[0], start_column=q[1], end_line=q[2], end_column=q[3])

    TestCases = [
        {
            "r1": (2, 8, 2, 9),
            "r2": (1, 1, 1, 100),
            "result": False
        },
        {
            "r1": (2, 8, 2, 9),
            "r2": (2, 6, 2, 7),
            "result": False
        },
        {
            "r1": (2, 8, 2, 9),
            "r2": (2, 6, 2, 8),
            "result": True
        },
        {
            "r1": (2, 8, 3, 9),
            "r2": (2, 6, 2, 8),
            "result": True
        },
        {
            "r1": (2, 8, 4, 9),
            "r2": (3, 6, 3, 800),
            "result": True
        },
        {
            "r1": (2, 8, 4, 9),
            "r2": (1, 6, 3, 800),
            "result": True
        },
        {
            "r1": (2, 8, 4, 9),
            "r2": (3, 6, 300, 800),
            "result": True
        },
        {
            "r1": (2, 8, 4, 9),
            "r2": (1, 6, 2, 7),
            "result": False
        },
    ]
    for t in TestCases:
      r1 = _QuadToRange(t["r1"])
      r2 = _QuadToRange(t["r2"])
      self.assertEqual(t["result"], r1.Overlaps(r2))
      self.assertEqual(t["result"], r2.Overlaps(r1))


if __name__ == '__main__':
  unittest.main()
