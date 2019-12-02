# Copyright 2017 The Chromium Authors.
#
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file or at
# https://developers.google.com/open-source/licenses/bsd.
"""This file defines the entry point for most external consumers of the Python
Codesearch library.
"""

from __future__ import absolute_import

import logging
import os

from .file_cache import FileCache
from .messages import \
        Annotation, \
        AnnotationRequest, \
        AnnotationType, \
        AnnotationTypeValue, \
        CallGraphRequest, \
        CallGraphResponse, \
        CodeBlock, \
        CodeBlockType, \
        CompoundRequest, \
        CompoundResponse, \
        EdgeEnumKind, \
        FileInfo, \
        FileInfoRequest, \
        FileSpec, \
        KytheNodeKind, \
        KytheXrefKind, \
        Node, \
        NodeEnumKind, \
        SearchRequest, \
        SearchResult, \
        TextRange, \
        XrefSearchRequest, \
        XrefSearchResult, \
        XrefSingleMatch
from .paths import GetSourceRoot
from .compat import StringFromBytes
from .language_utils import SymbolSuffixMatcher, IsIdentifier

try:
  from urllib.request import urlopen, Request
  from urllib.parse import urlencode, urlparse, unquote
except ImportError:
  from urllib2 import urlopen, Request
  from urllib import urlencode
  from urlparse import urlparse, unquote


class ServerError(Exception):
  """An error that occurred when talking to the CodeSearch backend."""
  pass


class NoFileSpecError(Exception):
  """XrefNode object didn't have an associated FileSpec.

  A valid FileSpec is required for traversing node relationships.
  """
  pass


class NotFoundError(Exception):
  """Something you were looking for was not found."""
  pass


class CsFile(object):
  """Represents a source file known to CodeSearch and allows looking up annotations."""

  def __init__(self, cs, file_info):
    assert isinstance(cs, CodeSearch)
    assert isinstance(file_info, FileInfo)

    self.cs = cs
    self.file_info = file_info

    if hasattr(self.file_info, 'content'):
      self.lines = self.file_info.content.text.splitlines()
    else:
      self.lines = []

    if hasattr(self.file_info, 'codeblock'):
      codeblocks = self.file_info.codeblock
      assert isinstance(codeblocks, list)
      assert len(codeblocks) == 0 or isinstance(codeblocks[0], CodeBlock)
      self.codeblock = CodeBlock.Make(
          child=codeblocks, type=CodeBlockType.ROOT, name="")
    else:
      self.codeblock = None
    self.annotations = None

  def Path(self):
    """Return the path to the file relative to the root of the source directory."""
    return self.file_info.name

  def Text(self, text_range):
    """Given a TextRange, returns the text corresponding to said range in this file.

    Any intervening newlines will be represented with a single '\n'."""

    assert isinstance(text_range, TextRange)

    if len(self.lines) == 0:
      raise IndexError('no text received for file contents. path:{}'.format(
          self.file_info.name))

    if text_range.start_line <= 0 or text_range.start_line > len(self.lines) or \
            text_range.end_line <= 0 or text_range.end_line > len(self.lines) or \
            text_range.start_line > text_range.end_line:
      raise IndexError(
          'invalid range specified: ({},{}) - ({},{}) : {} lines'.format(
              text_range.start_line, text_range.start_column,
              text_range.end_line, text_range.end_column, len(self.lines)))
    if text_range.start_line == text_range.end_line:
      return self.lines[text_range.start_line
                        - 1][text_range.start_column - 1:text_range.end_column]

    first = [
        self.lines[text_range.start_line - 1][text_range.start_column - 1:]
    ]
    middle = self.lines[text_range.start_line:text_range.end_line - 1]
    end = [self.lines[text_range.end_line - 1][:text_range.end_column]]

    return '\n'.join(first + middle + end)

  def GetCodeBlock(self):
    """Retrieves a CodeBlocks of type ROOT for the file or None if one is not found.

    Note that a CodeBlock list is only preset if one is requested explicitly at
    the time the FileInfo request was made by setting the ``fetch_outline```
    field to True.
    """
    return self.codeblock

  def FindCodeBlock(self, name="", type=CodeBlockType.ROOT):
    """Find a codeblock in this file that matches the name and type. If there
    are more than one, could return any."""
    if self.codeblock is None:
      return None
    return self.codeblock.Find(name, type)

  def GetSignatureForCodeBlock(self, codeblock):
    """Return a signature for a CodeBlock or None if no signature could be determined.

    The ``signature`` field included included in the CodeBlock is the function
    signature for a FUNCTION type CodeBlock and not the CodeSearch signature.
    Also, the ``text_range`` property refers to the entire code block that
    encompasses the definition or declaration of the symbol.

    So this function looks through annotations that fall within ``text_range``
    where the annotation text matches the ``name`` property of the CodeBlock.
    If one is found and if that annotation has a signature, then that signature
    is returned.
    """
    assert isinstance(codeblock, CodeBlock)

    # Can't do much with this codeblock if it doesn't have a range or a name.
    # This is the case for a root CodeBlock, but we are generalizing it to
    # include any CodeBlock without a name or a range. It's not considered an
    # error since we expect this to be the case for some code blocks that don't
    # map to a signature.
    if not hasattr(codeblock, 'name') or not hasattr(codeblock, 'text_range'):
      return None

    assert isinstance(codeblock.text_range, TextRange)

    annotations = self.GetAnnotations()

    for annotation in annotations:
      if not annotation.HasSignature():
        continue
      if not codeblock.text_range.Overlaps(annotation.range):
        continue
      if self.Text(annotation.range) == codeblock.name:
        return annotation.GetSignature()
    return None

  def GetFileSpec(self):
    return FileSpec(
        name=self.file_info.name, package_name=self.file_info.package_name)

  def GetAnnotations(self):
    if self.annotations == None:
      response = self.cs.GetAnnotationsForFile(self.GetFileSpec())
      if not hasattr(response.annotation_response[0], 'annotation'):
        return []
      self.annotations = response.annotation_response[0].annotation
      assert isinstance(self.annotations, list)
      assert isinstance(self.annotations[0], Annotation)

    return self.annotations

  def GetAnchorText(self, signature):
    # Fetch annotations if we haven't already.
    self.GetAnnotations()
    assert isinstance(self.annotations, list)
    assert len(self.annotations) == 0 or isinstance(self.annotations[0],
                                                    Annotation)

    for annotation in self.annotations:
      if annotation.MatchesSignature(signature):
        return self.Text(annotation.range)

    raise NotFoundError(
        'can\'t determine display name for {}'.format(signature))


class XrefNode(object):
  """A cross-reference node.

  The codesearch data is represented as a graph of nodes where locations in
  source or symbols are are connected to other symbols or locations in source
  via edges.

  In the abstract, a location or symbol in the source is represented using
  what's called a "signature". In practice the signature is a string whose
  exact contents is determined by the indexer. It's purpose is to act as an
  identifier for the "thing" its referring to.

  For example, the declaration of HttpNetworkTransaction in
  net/http_network_transaction.cc (currently) has the signature
  "cpp:net::class-HttpNetworkTransaction@chromium/../../net/http/http_network_transaction.h|def".

  HttpNetworkTransaction declares a number of symols (more than 200 at the time
  of writing). So you can figure out its declared symbols as follows:

  >>> import codesearch

  # Replace the path with where you have the Chromium sources checked out:
  >>> cs = codesearch.CodeSearch(source_root='.')

  # Ditto for the path to source:
  >>> sig = cs.GetSignatureForSymbol('src/net/http/http_network_transaction.cc', 'HttpNetworkTransaction')
  >>> node = codesearch.XrefNode.FromSignature(cs, sig)
  >>> node.Traverse(codesearch.KytheXrefKind.REFERENCE)

  This should dump out a list of XrefNode objects corresponding to the declared
  symbols. You can inspect the .filespec and .single_match members to figure
  out what the symols are.

  Note that the members of the |node| object that was created by
  XrefNode.FromSignature() doesn't have anything interesting in the |filespec|
  and |single_match| members other than the signature.
  """

  def __init__(self, cs, single_match, filespec=None, parent=None):
    """Constructs an XrefNode.

    This is probably not what you are looking for. Instead figure out the
    signature of the node you want to start with using one of the
    GetSignatureFor* methods in CodeSearch, and then use the
    XrefNode.FromSignature() static method to construct a starter node.

    From there, you can use the GetEdge() and/or GetAllEdges() methods to
    explore the cross references."""

    self.cs = cs
    self.filespec = filespec
    self.single_match = single_match
    self.parent = parent

    assert isinstance(self.cs, CodeSearch)
    assert isinstance(self.single_match, XrefSingleMatch)
    assert self.filespec is None or isinstance(self.filespec, FileSpec)
    assert self.parent is None or isinstance(self.parent, XrefNode)

  def Traverse(self, xref_kinds=None, max_num_results=500):
    """Gets outgoing edges for this node.

    Returns a list of XrefNode objects. If there are no results, then returns
    an empty list.

    |xref_kinds|, if specified could either be a KytheXrefKind value or a list
    of KytheXrefKind values. The list specifies the types of cross references
    to return.
    """

    results = self.cs.GetXrefsFor(self.single_match.signature, max_num_results)
    if not results:
      return []

    results = XrefNode.FromSearchResults(self.cs, results, self)
    if xref_kinds is None:
      return results

    if isinstance(xref_kinds, list):
      xrset = set(xref_kinds)
    else:
      xrset = set()
      xrset.add(xref_kinds)

    # Make sure that the incoming filter is based on KytheXrefKind and not the
    # deprecated NodeEnumKind. Fortunately, the valid numbers for each type are
    # disjoint despite being sparse.
    for v in xrset:
      assert KytheXrefKind.ToSymbol(v) != v

    cg = []
    if KytheXrefKind.CALLED_BY in xrset:
      cg.extend([
          XrefNode.FromNode(self.cs, n)
          for n in self._GetCallGraphNode(max_num_results).children
      ])

    return list(filter(lambda n: n.single_match.type_id in xrset, results)) + cg

  def _GetCallGraphNode(self, max_num_results=500):
    """Returns a single Node containing one level of the incoming call graph."""

    cg_response = self.cs.GetCallGraph(
        signature=self.single_match.signature, max_num_results=max_num_results)

    if not isinstance(cg_response, CompoundResponse) or not hasattr(
        cg_response, 'call_graph_response'):
      raise ServerError("Unexpected response. {}".format(cg_response))

    response = cg_response.call_graph_response[0]

    if not hasattr(response, 'node') or not hasattr(response.node, 'children'):
      raise NotFoundError("No callers for signature {} ({})".format(
          self.single_match.signature, self.GetDisplayName()))

    return response.node

  def GetFile(self):
    """Return the file containing this XrefNode as a CsFile."""
    if not self.filespec:
      raise NoFileSpecError('no filespec found for XrefNode')
    return self.cs.GetFileInfo(self.filespec)

  def GetDisplayName(self, try_via_references=True):
    """Return the display name for this XrefNode.

    It is possible for there to be no associated displayname. E.g. if the
    XrefNode corresponds to a template specialization. In that case, this
    method will throw.
    """

    if not self.filespec:
      raise NoFileSpecError('no filespec found for XrefNode')

    try:
      return self.cs.GetFileInfo(self.filespec).GetAnchorText(
          self.single_match.signature)
    except ServerError as e:
      # Backend index is incomplete :-(. Unfortunately, this happens often,
      # and is generally expected when following links to STL symbols.
      if 'Could not resolve' in e.message and try_via_references:
        pass
      raise e

    # Backup logic. Try to lookup a reference that works. Only try 5 times.
    for ref in self.Traverse(KytheXrefKind.REFERENCE)[:5]:
      try:
        # Setting try_via_references to False since we don't want to fan
        # out a tree of retries. Something should work at this level or we
        # should just give up instead of DoSing the server.
        return ref.GetDisplayName(try_via_references=False)
      except ServerError as e:
        pass
    raise NotFoundError('display name could not be resolved for {}'.format(
        self.single_match.signature))

  def GetRelatedAnnotations(self):
    """Get related annotations. Currently this is defined to be annotations
    that surround the current xref location."""

    if not self.filespec:
      raise NoFileSpecError('no filespec found for XrefNode')

    annotations = self.cs.GetFileInfo(self.filespec).GetAnnotations()
    target_range = None
    for annotation in annotations:
      if annotation.MatchesSignature(self.single_match.signature):
        target_range = annotation.range
        break

    if not target_range:
      raise NotFoundError('no related annotations')

    related = []
    for annotation in annotations:
      if annotation.range.end_line < target_range.start_line or \
              annotation.range.start_line > target_range.end_line:
        continue
      if annotation.range == target_range:
        continue
      related.append(annotation)
    return related

  def GetRelatedDefinitions(self):
    """Get related definitions. Currently this is defined to be linked
    definitions that surround the current xref location.

    This is a hack that can be used to get at the type of a class member. E.g.:

        class Foo {
          public:
            BarType bar_;
        };

    The signature of |bar_|'s definition should have a cross reference of type
    HAS_TYPE which should link to the type. However, in practice, this
    reference type may not be available. Instead, we use a heuristic where we
    look for definitions that are on the same lines as the target. In this
    case, we look for a LINK_TO_DEFINITION type annotation on the same line as
    |bar_|. Such a link is likely to point to the type of |bar_| if it's a
    compound type with cross reference information.

    If any such definitions are found, a further HAS_DEFINITION edge lookup is
    performed so that the resulting XrefNode corresponds to the definition of
    the type.
    """

    if not self.filespec:
      raise NoFileSpecError('no filespec found for XrefNode')
    annotations = self.cs.GetFileInfo(self.filespec).GetAnnotations()
    target_range = None
    for annotation in annotations:
      sig = getattr(annotation, 'xref_signature', None)
      if sig and sig.signature == self.single_match.signature:
        target_range = annotation.range
        break

    if not target_range:
      raise NotFoundError('no related annotations')

    related = []
    for annotation in annotations:
      if annotation.type.id != AnnotationTypeValue.LINK_TO_DEFINITION:
        continue
      if annotation.range.end_line < target_range.start_line or \
              annotation.range.start_line > target_range.end_line:
        continue

      abstract_node = XrefNode.FromAnnotation(self.cs, annotation)
      def_list = abstract_node.Traverse(KytheXrefKind.DEFINITION)
      if def_list:
        related.append(def_list[0])
        continue

      dcl_list = abstract_node.Traverse(KytheXrefKind.DECLARATION)
      if dcl_list:
        related.append(dcl_list[0])
        continue

      related.append(abstract_node)
    return related

  def GetSignature(self):
    """Return the signature for this node"""
    return self.single_match.signature.split(' ')[0]

  def GetSignatures(self):
    return self.single_match.GetSignatures()

  def __str__(self):
    s = "{"
    if self.filespec:
      s += "filespec: {}, ".format(str(self.filespec))
    s += "single_match: {}".format(str(self.single_match))
    s += "}"
    return s

  @staticmethod
  def FromSignature(cs, signature, filename=None):
    """Construct a XrefNode object for |signature|.

    Other than the |signature| the constructured node will have no other
    interesting fields. It can, however, be used to query outgoing edges.
    """

    assert isinstance(cs, CodeSearch)
    assert signature

    if filename is None:
      filespec = cs.GetFileSpecFromSignature(signature)
    else:
      filespec = cs.GetFileSpec(filename)

    return XrefNode(
        cs,
        filespec=filespec,
        single_match=XrefSingleMatch(signature=signature))

  @staticmethod
  def FromNode(cs, node):
    """Construct a XrefNode based on a Node.
    """

    assert isinstance(cs, CodeSearch)
    assert isinstance(node, Node)

    line_text = ""
    line_number = node.call_site_range.start_line

    # Snippets for Node results only contain one line of text.
    if hasattr(node, 'snippet'):
      line_text = node.snippet.text.text
      line_number = node.snippet.first_line_number

    match = XrefSingleMatch(
        line_number=line_number, line_text=line_text, signature=node.signature)
    return XrefNode(
        cs=cs,
        single_match=match,
        filespec=FileSpec(name=node.file_path, package_name=node.package_name))

  @staticmethod
  def FromAnnotation(cs, annotation):
    """Construct a XrefNode based on an Annotation.

    This is currently limited to annotations that have a LINK_TO_DEFINITION."""

    assert isinstance(annotation, Annotation)
    assert annotation.type.id == AnnotationTypeValue.LINK_TO_DEFINITION

    return XrefNode.FromSignature(
        cs,
        annotation.internal_link.signature,
        filename=FileSpec(
            name=annotation.internal_link.path,
            package_name=annotation.internal_link.package_name))

  @staticmethod
  def FromSearchResults(cs, results, parent=None):
    """Construct a *list* of XrefNode objects from a list of XrefSearchResult
    objects.
    """
    nodes = []

    assert isinstance(cs, CodeSearch)
    assert isinstance(results, list)

    for result in results:
      assert isinstance(result, XrefSearchResult)

      for match in result.match:
        assert isinstance(match, XrefSingleMatch)

        nodes.append(XrefNode(cs, match, result.file, parent))

    return nodes


class CodeSearch(object):

  class Stats(object):
    """Used internally to track how many requests are being made."""

    def __init__(self):
      self.cache_hits = 0
      self.cache_misses = 0  # == number of network requests made

  def __init__(self,
               should_cache=False,
               cache_dir=None,
               cache_timeout_in_seconds=1800,
               source_root=None,
               a_path_inside_source_dir=None,
               package_name='chromium',
               codesearch_host='https://cs.chromium.org',
               request_timeout_in_seconds=3,
               user_agent_string='Python-CodeSearch-Client'):
    """Initialize a CodeSearch object.

    Creating a CodeSearch object is probably the first thing you are going to
    do since all the other classes that make requests to the codesearch
    backened depend on it.

    Arguments:
        should_cache -- Should be set to True in order to use a disk cache. See
            documentation for cache_dir for how the cache is organized.

        cache_dir -- Directory where the disk cache is located. Only considered
            if |should_cache| is True.

            The directory is created if it doesn't already exist. Any files
            contained therein are considered to be part of the cache. Hence you
            can reuse a single cache across multiple sessions / instantiations
            of the library. The exact contents of the directory is an
            implementation detail of FileCache.

            Set this to None to use an ephemeral disk cache that's only used
            during a single session.

        cache_timeout_in_seconds -- The amount of time a request should be
            cached before being sent out to the network again.

        source_root -- The CodeSearch backend refers to files using paths that
            are relative to the root of a source tree. This argument specifies
            this root in the local filesystem.

            E.g.: if source_root == '/src/chrome/', then a the library can look
            at a path like '/src/chrome/src/base/logging.h' and rewrite it to be
            'src/base/logging.h'. The latter is the source-relative path known
            to CodeSearch.

            Set this to '.' in order to use source-relative paths directly with
            not modification.

            Or leave out source_root and specify a_path_inside_source_dir
            instead if that's easier. Either works.

        a_path_inside_source_dir -- As the name implies, this shoud be a path to
            a file inside the Chromium source directory. The library will
            examing the path and determine the root.

        user_agent_string -- The UA string to use when making requests to the
            CodeSearch backend.

        request_timeout_in_seconds -- Timeout to be applied to requests made to
            the server.

    In general, you only need to set the source_root or a_path_inside_source_dir
    in order to construct a functional CodeSearch object.

    E.g.:
    >>> cs = CodeSearch(source_root='.')
    """

    # An instance of FileCache or None if no caching is to be performed.
    self.file_cache = None

    # A cache mapping path -> CsFile objects.
    self.file_info_cache = {}

    self.logger = logging.getLogger('codesearch')

    self.package_name = package_name

    self.codesearch_host = codesearch_host

    self.request_timeout_in_seconds = request_timeout_in_seconds

    self.source_root = source_root if source_root else GetSourceRoot(
        a_path_inside_source_dir)

    self.extra_headers = {'User-Agent': user_agent_string}

    self.stats = CodeSearch.Stats()

    if not should_cache:
      self.file_cache = None
      return
    self.file_cache = FileCache(
        cache_dir=cache_dir, expiration_in_seconds=cache_timeout_in_seconds)

  def GetSourceRoot(self):
    return self.source_root

  def GetLogger(self):
    return self.logger

  def GetFileSpec(self, path=None):
    if not path:
      return FileSpec(name='.', package_name=self.package_name)

    if isinstance(path, FileSpec):
      return FileSpec(name=path.name, package_name=path.package_name)

    relpath = os.path.relpath(os.path.abspath(path), self.source_root).replace(
        '\\', '/')
    return FileSpec(name=relpath, package_name=self.package_name)

  def GetFileSpecFromSignature(self, signature):
    KYTHE_PREFIX = "kythe://"
    assert signature.startswith(KYTHE_PREFIX)
    signature = signature[len(KYTHE_PREFIX):].split('#')[0]
    args = signature.split('?')
    assert len(args) > 1
    path = ""
    for a in args:
      if a.startswith("path="):
        path = unquote(a[5:])
    assert path != ""
    return FileSpec(name=path, package_name=args[0])

  def TeardownCache(self):
    if self.file_cache:
      self.file_cache.close()

    self.file_cache = None

  def _Retrieve(self, url):
    """Retrieve the URL, optionally using the cache.

    If a cache is in use, checks if the URL is cached, otherwise sends it to the
    network. Note that the cache in question may not obey usual HTTP caching
    semantics.

    The response is a str object containing the response body on success. Will
    throw on failure.
    """
    self.logger.debug('Fetching %s', url)

    if self.file_cache:
      cached_response = self.file_cache.get(url)
      self.logger.debug('Found cached response')
      if (cached_response):
        self.stats.cache_hits += 1
        return cached_response.decode('utf8')
    self.stats.cache_misses += 1

    # Long URLs cause the request to fail. If it's too long, snip off the query
    # and send it as a POST body. Yes, this is how it works.
    if len(url) > 1500:
      parsed = urlparse(url)
      short_url = '{}://{}{}'.format(parsed.scheme, parsed.netloc, parsed.path)
      data = parsed.query.encode('utf-8')
      request = Request(url=short_url, headers=self.extra_headers, data=data)
    else:
      request = Request(url=url, headers=self.extra_headers)

    response = urlopen(request, timeout=self.request_timeout_in_seconds)
    result = response.read()
    response.close()
    if self.file_cache:
      self.file_cache.put(url, result)
    return StringFromBytes(result)

  def SendRequestToServer(self, compound_request):
    if not isinstance(compound_request, CompoundRequest):
      raise ValueError(
          '|compound_request| should be an instance of CompoundRequest')

    qs = urlencode(compound_request.AsQueryString(), doseq=True)
    url = '{host}/codesearch/json?{qs}'.format(host=self.codesearch_host, qs=qs)
    result = self._Retrieve(url)
    return CompoundResponse.FromJsonString(result)

  def GetCallGraph(self, signature, max_num_results=500):
    """Retrieves a list of call graph nodes corresponding to all call sites of
    |signature|.

    """
    return self.SendRequestToServer(
        CompoundRequest(call_graph_request=[
            CallGraphRequest(
                file_spec=self.GetFileSpec(),
                max_num_results=max_num_results,
                signature=signature)
        ]))

  def GetAnnotationsForFile(
      self,
      filename,
      annotation_types=[
          AnnotationType(id=AnnotationTypeValue.XREF_SIGNATURE), AnnotationType(
              id=AnnotationTypeValue.LINK_TO_DEFINITION)
      ]):
    """Retrieves a list of annotations for a file.

    Note that it is much more efficient in your scripts to use
    CsFile.GetAnnotations() instead of calling this function directly. CsFile
    caches the annotations per file, and CodeSearch.GetFileInfo() ensures that
    there's only one CsFile per file. This minimizes the number of requests
    sent over the network.
    """
    return self.SendRequestToServer(
        CompoundRequest(annotation_request=[
            AnnotationRequest(
                file_spec=self.GetFileSpec(filename), type=annotation_types)
        ]))

  def GetSignatureForLocation(self, filename, line, column):
    """Get the signature of the symbol at a specific location in a source file.

    All locations are 1-based."""

    annotations = self.GetFileInfo(filename).GetAnnotations()
    enclosing_annotation_found = False
    for annotation in annotations:
      if not annotation.range.Contains(line, column):
        continue

      enclosing_annotation_found = True

      if hasattr(annotation, 'xref_signature'):
        return annotation.xref_signature.signature

      if hasattr(annotation, 'internal_link'):
        return annotation.internal_link.signature

    if not enclosing_annotation_found:
      raise NotFoundError("no annotations found at (%d:%d) for %s" %
                          (line, column, filename))

    raise NotFoundError("can't determine signature for %s at %d:%d" %
                        (filename, line, column))

  def GetFileInfo(self,
                  filename,
                  fetch_html_content=False,
                  fetch_outline=True,
                  fetch_folding=False,
                  fetch_generated_from=False):
    """Return a CsFile object corresponding to the file named by |filename|.

    If |filename| is a FileSpec object, then that FileSpec object is used as-is
    to locate the file. Otherwise |filename| is resolved as a local path which
    should then map to a file known to CodeSearch."""

    print('GetFileInfo=', filename)
    file_spec = self.GetFileSpec(filename)
    cacheable = (not fetch_html_content) and fetch_outline and (
        not fetch_folding) and (not fetch_generated_from)
    if cacheable and (file_spec.name in self.file_info_cache):
      return self.file_info_cache[file_spec.name]

    result = self.SendRequestToServer(
        CompoundRequest(file_info_request=[
            FileInfoRequest(
                file_spec=self.GetFileSpec(filename),
                fetch_html_content=fetch_html_content,
                fetch_outline=fetch_outline,
                fetch_folding=fetch_folding,
                fetch_generated_from=fetch_generated_from)
        ]))

    if hasattr(result.file_info_response[0], 'error_message'):
      raise ServerError('server reported error while fetching FileInfo: {}'.
                        format(result.file_info_response[0].error_message))

    if hasattr(result.file_info_response[0], 'file_info'):
      file_info = CsFile(self, file_info=result.file_info_response[0].file_info)
      if cacheable:
        self.file_info_cache[file_spec.name] = file_info

      return file_info

    raise ServerError(
        'unexpected message format while fetching file info for %s' %
        (filename))

  def GetSignatureForSymbol(self, filename, symbol):
    """Return a signature matching |symbol| in |filename|.
    """

    fileinfo = self.GetFileInfo(filename)
    for annotation in fileinfo.GetAnnotations():
      if symbol not in fileinfo.Text(annotation.range):
        continue

      if annotation.type.id == AnnotationTypeValue.LINK_TO_DEFINITION:
        return annotation.internal_link.GetSignature()

      if annotation.type.id == AnnotationTypeValue.XREF_SIGNATURE:
        return annotation.xref_signature.GetSignature()

    raise NotFoundError("Can't determine signature for %s:%s" % (filename,
                                                                 symbol))

  def GetSignaturesForSymbol(self, filename, symbol, node_kind=None):
    """Get all matching signatures given a symbol.

    Returns all the signatures matching the given symbol in |filename|. If
    |node_kind| is not None, it is assumed to be one of the KytheNodeKind
    values and is used to filter matches to those of the |node_kind|.

    Symbols are matched using SymbolSuffixMatcher. See documentation for that
    class for more information about how fuzzy symbol matches are performed.

    Returns a list of strings containing signatures. The list will be empty if
    the search failed.
    """

    assert node_kind is None or KytheNodeKind.ToSymbol(node_kind) != node_kind

    signatures = set()
    fileinfo = self.GetFileInfo(filename)
    matcher = SymbolSuffixMatcher(symbol)

    for annotation in fileinfo.GetAnnotations():
      text = fileinfo.Text(annotation.range)
      if not matcher.Match(text):
        continue

      if node_kind is not None and node_kind != annotation.kythe_xref_kind:
        continue

      if annotation.type.id == AnnotationTypeValue.LINK_TO_DEFINITION:
        signatures.update(annotation.internal_link.GetSignatures())
        continue

      if annotation.type.id == AnnotationTypeValue.XREF_SIGNATURE:
        signatures.update(annotation.xref_signature.GetSignatures())
        continue

    return list(signatures)

  def SearchForSymbol(self,
                      symbol,
                      xref_kind=None,
                      max_results_to_analyze=5,
                      return_all_results=False):
    """Search for a specified symbol.

    Use this when you know the symbol name and its type, and don't mind a few
    extra requests on the wire. It is possible that you'll end up with more
    than one result.

    Returns a list of XrefNode objects. The list will be empty if the search
    failed.

    |xref_kind| should be one of NodeEnumKind.

    Set |max_results_to_analyze| to change the number of search results to look
    at. Note that each search result corresponds to a file.

    If |return_all_results| is True, then all signatures from all the search
    results will be returned. Note that this can be very slow. By default, the
    search will end as soon as at least one signature has been found.
    """
    XREF_KIND_TO_SEARCH_PREFIX = {
        NodeEnumKind.CLASS: "class:",
        NodeEnumKind.FUNCTION: "function:",
        NodeEnumKind.METHOD: "function:"
    }

    search_prefix = XREF_KIND_TO_SEARCH_PREFIX.get(xref_kind, "symbol:")
    search_query = "{}{}".format(search_prefix, symbol)

    search_response = self.SendRequestToServer(
        CompoundRequest(search_request=[
            SearchRequest(
                query=search_query,
                exhaustive=True,
                max_num_results=max_results_to_analyze,
                return_all_duplicates=False,
                return_all_snippets=True,
                return_decorated_snippets=False,
                return_directories=False,
                return_line_matches=True,
                return_snippets=True)
        ])).search_response[0]

    if not hasattr(search_response, 'search_result'):
      return []

    signatures = set()
    result = None
    for r in search_response.search_result:
      result = r
      assert isinstance(result, SearchResult)

      if not hasattr(result, 'top_file'):
        continue

      if not hasattr(result, 'snippet'):
        continue

      for snippet in result.snippet:
        if not hasattr(snippet, 'text') or not hasattr(snippet.text, 'range'):
          continue
        for r in snippet.text.range:
          try:
            s = self.GetSignatureForLocation(
                result.top_file.file.name,
                snippet.first_line_number + r.range.end_line - 1,
                r.range.end_column - 1)
            signatures.add(s)
          except:
            # This usually means that there were no matches at the location. CS
            # being CS, sometimes we need to resort to doing terrible things,
            # like searching by symbol name.
            sa = self.GetSignaturesForSymbol(result.top_file.file.name, symbol)
            signatures.update(set(sa))

            if len(sa) == 0:
              # Well, that didn't work either. Let's try one more time and
              # this time look up signatures that look like they are in the
              # right ballpark. This is the least accurate of the methods
              # available to us.
              try:
                s = self.GetSignatureForSymbol(result.top_file.file.name,
                                               symbol)
                signatures.add(s)
              except:
                pass
            pass

      if not return_all_results and len(signatures) > 0:
        break

    return [XrefNode.FromSignature(self, signature=s) for s in signatures]

  def GetXrefsFor(self, signature, max_num_results=500):
    refs = self.SendRequestToServer(
        CompoundRequest(xref_search_request=[
            XrefSearchRequest(
                file_spec=self.GetFileSpec(),
                query=signature,
                max_num_results=max_num_results)
        ]))
    if not refs or not hasattr(refs.xref_search_response[0], 'search_result'):
      return []
    return refs.xref_search_response[0].search_result

  def GetOverridingDefinitions(self, signature):
    """GetOverridingDefinitions returns a list of XrefSearchResult objects
    representing all the overrides of the symbol corresponding to |signature|.

    Returns an empty list if there are no overrides.
    """

    refs = self.GetXrefsFor(signature)
    for result in refs:
      result.match = filter(
          lambda match: getattr(match, 'type_id', -1) == KytheXrefKind.OVERRIDDEN_BY,
          result.match)
    return filter(lambda result: len(result.match) > 0, refs)

  def GetCallTargets(self, signature):
    # First look up the declaration(s) for the callsite.
    decl_signatures = []
    for decl in self.GetXrefsFor(signature):
      assert isinstance(decl, XrefSearchResult)
      for match in decl.match:
        assert isinstance(match, XrefSingleMatch)
        if match.type_id not in (KytheXrefKind.DECLARATION,
                                 KytheXrefKind.DEFINITION,
                                 KytheXrefKind.OVERRIDDEN_BY):
          continue
        decl_signatures.append(match.signature)

    # These are signatures of declarations or definions of symbols that could
    # possibly override the call site.
    candidates = []
    for sig in decl_signatures:
      candidates.extend(self.GetOverridingDefinitions(sig))
    return candidates

  def IsContentStale(self, filename, buffer_lines, check_prefix=False):
    """Returns true if the file known to codesearch has different contents from
    what's expected.

    |filename| specifies a file on disk (or just relative to the
    |source_root|).  |buffer_lines| is a list of strings containing the
    expected contents of the file.

    If |check_prefix| is true, then only the first len(buffer_lines) lines of
    |filename| are compared.
    """
    fileinfo = self.GetFileInfo(filename)
    content_lines = fileinfo.lines

    if check_prefix:
      content_lines = content_lines[:len(buffer_lines)]
      if len(content_lines) != len(buffer_lines):
        return True

    for left, right in zip(content_lines, buffer_lines):
      if left != right:
        return True

    return False
