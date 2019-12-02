# Copyright 2017 The Chromium Authors.
#
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file or at
# https://developers.google.com/open-source/licenses/bsd.

import unittest

from .client_api import CsFile, CodeSearch
from .messages import FileInfo, TextRange, NodeEnumKind, CodeBlock, CodeBlockType, AnnotationTypeValue
from .testing_support import InstallTestRequestHandler


class TestCsFile(unittest.TestCase):

  def setUp(self):
    InstallTestRequestHandler()

  def test_text_range(self):
    cs = CodeSearch(source_root='/src/chrome/')
    cs_file = cs.GetFileInfo('/src/chrome/src/LICENSE')

    self.assertEqual(
        "// Redistribution and use",
        cs_file.Text(
            TextRange(start_line=3, start_column=1, end_line=3, end_column=25)))

    self.assertEqual(
        "CONTRIBUTORS\n/",
        cs_file.Text(
            TextRange(
                start_line=17, start_column=59, end_line=18, end_column=1)))

    self.assertEqual(
        """CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//""",
        cs_file.Text(
            TextRange(
                start_line=17, start_column=59, end_line=19, end_column=2)))

  def test_path(self):
    cs = CodeSearch(source_root='/src/chrome/')
    cs_file = cs.GetFileInfo('/src/chrome/src/LICENSE')
    self.assertEqual(cs_file.Path(), 'src/LICENSE')

  def test_display_name(self):
    cs = CodeSearch(source_root='/src/chrome/')
    cs_file = cs.GetFileInfo('/src/chrome/src/net/http/http_auth.h')
    self.assertEqual(
        cs_file.GetAnchorText(
            'kythe://chromium?lang=c%2B%2B?path=src/net/http/http_auth.h#HttpAuth%3Anet%23c%23hUTvau_Z32C'
        ), 'HttpAuth')

  def test_get_codeblock(self):
    cs = CodeSearch(source_root='/src/chrome/')
    cs_file = cs.GetFileInfo('/src/chrome/src/net/http/http_auth.h')
    block = cs_file.GetCodeBlock()
    self.assertIsInstance(block, CodeBlock)
    self.assertNotEqual(0, len(block.child))
    self.assertIsInstance(block.child[0], CodeBlock)

  def test_get_signature_for_codeblock(self):
    cs = CodeSearch(source_root='/src/chrome/')
    cs_file = cs.GetFileInfo('/src/chrome/src/net/http/http_auth.h')
    block = cs_file.FindCodeBlock(
        name="AuthorizationResult", type=CodeBlockType.ENUM)
    self.assertIsNotNone(block)
    sig = cs_file.GetSignatureForCodeBlock(block)
    self.assertIsNotNone(sig)
    self.assertNotEqual("", sig)

  def test_get_annotations_types(self):
    cs = CodeSearch(source_root='/src/chrome/')
    cs_file = cs.GetFileInfo('/src/chrome/src/net/http/http_auth.h')
    annotations = cs_file.GetAnnotations()
    found_xref_signature = False
    found_link_to_definition = False
    for annotation in annotations:
      if annotation.type.id == AnnotationTypeValue.XREF_SIGNATURE:
        found_xref_signature = True
      if annotation.type.id == AnnotationTypeValue.LINK_TO_DEFINITION:
        found_link_to_definition = True
    self.assertTrue(found_xref_signature)
    self.assertTrue(found_link_to_definition)


if __name__ == '__main__':
  unittest.main()
