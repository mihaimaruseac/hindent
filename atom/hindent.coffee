{CompositeDisposable} = require 'atom'
{BufferedProcess} = require 'atom'
{dirname} = require 'path'
{statSync} = require 'fs'

prettify = (style, text, workingDirectory, {onComplete, onFailure}) ->
  lines = []
  proc = new BufferedProcess
    command: 'hindent'
    args: ['--style', style]
    options:
      cwd: workingDirectory
    stdout: (line) -> lines.push(line)
    exit: -> onComplete?(lines.join(''))
  proc.onWillThrowError ({error, handle}) ->
    atom.notifications.addError "Hindent could not spawn",
      detail: "#{error}"
    onFailure?()
    handle()
  proc.process.stdin.write(text)
  proc.process.stdin.end()

prettifyFile = (style, editor, format = 'haskell') ->
  [firstCursor, cursors...] = editor.getCursors().map (cursor) ->
    cursor.getBufferPosition()
  try
    workDir = dirname(editor.getPath())
    if not statSync(workDir).isDirectory()
      workDir = '.'
  catch
    workDir = '.'
  prettify style, editor.getText(), workDir,
    onComplete: (text) ->
      editor.setText(text)
      if editor.getLastCursor()?
        editor.getLastCursor().setBufferPosition firstCursor,
          autoscroll: false
        cursors.forEach (cursor) ->
          editor.addCursorAtBufferPosition cursor,
            autoscroll: false

module.exports = Hindent =
  disposables: null
  menu: null

  activate: (state) ->
    @disposables = new CompositeDisposable
    @menu = new CompositeDisposable

    @disposables.add \
      atom.commands.add 'atom-text-editor[data-grammar~="haskell"]',
        'hindent:prettify-fundamental': ({target}) =>
          prettifyFile 'fundamental', target.getModel()
        'hindent:prettify-chris-done': ({target}) =>
          prettifyFile 'chris-done', target.getModel()
        'hindent:prettify-johan-tibell': ({target}) =>
          prettifyFile 'johan-tibell', target.getModel()
        'hindent:prettify-gibiansky': ({target}) =>
          prettifyFile 'gibiansky', target.getModel()

    @menu.add atom.menu.add [
      label: 'hindent'
      submenu : [
        {label: 'Fundamental', command: 'hindent:prettify-fundamental'}
        {label: 'Chris Done', command: 'hindent:prettify-chris-done'}
        {label: 'Johan Tibell', command: 'hindent:prettify-johan-tibell'}
        {label: 'Gibiansky', command: 'hindent:prettify-gibiansky'}
      ]
    ]

  deactivate: ->
    @disposables.dispose()
    @disposables = null

    @clearMenu()

  clearMenu: ->
    @menu.dispose()
    @menu = null
    atom.menu.update()
