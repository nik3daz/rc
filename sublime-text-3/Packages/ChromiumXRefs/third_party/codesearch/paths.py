# Copyright 2017 The Chromium Authors.
#
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file or at
# https://developers.google.com/open-source/licenses/bsd.

import os


class NoSourceRootError(Exception):
  """Exception raise when the CodeSearch library can't determine the location
of the local Chromium checkout."""
  pass


def GetPackageRelativePath(filename):
  """GetPackageRelativePath returns the path to |filename| relative to the root
  of the package as determined by GetSourceRoot()."""

  return os.path.relpath(filename, GetSourceRoot(filename)).replace('\\', '/')


def GetSourceRoot(filename):
  """Try to determine the root of the package which contains |filename|.

  The current heuristic attempts to determine the root of the Chromium source
  tree by searching up the directory hierarchy until we find a directory
  containing src/.gn.
  """

  # If filename is not absolute, then we are going to assume that it is
  # relative to the current directory.
  if not os.path.isabs(filename):
    filename = os.path.abspath(filename)
  if not os.path.exists(filename):
    raise NoSourceRootError('File not found: {}'.format(filename))
  source_root = os.path.dirname(filename)
  while True:
    gnfile = os.path.join(source_root, 'src', '.gn')
    if os.path.exists(gnfile):
      return source_root

    new_package_root = os.path.dirname(source_root)
    if new_package_root == source_root:
      raise NoSourceRootError("Can't determine package root")
    source_root = new_package_root
