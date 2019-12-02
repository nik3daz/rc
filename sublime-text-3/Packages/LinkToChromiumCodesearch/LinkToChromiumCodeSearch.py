import sublime
import sublime_plugin
import re


class LinkToCodesearchCommand(sublime_plugin.TextCommand):
  def run(self, edit):
    view = self.view
    line_num = view.rowcol(view.sel()[0].begin())[0] + 1
    (file_name, subs) = re.subn('.*?/src/', '', view.file_name(), 1)
    if subs == 0:
      return

    sublime.set_clipboard('https://cs.chromium.org/chromium/src/%s?l=%s' % (file_name, line_num))
