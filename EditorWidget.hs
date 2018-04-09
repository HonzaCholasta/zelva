-- Copyright (c) 2018 Jan Cholasta <jan@cholasta.net>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

module EditorWidget where

import Qtc.Classes.Qccs
import Qtc.Classes.Core
import Qtc.Enums.Classes.Core
import Qtc.Classes.Gui
import Qtc.Classes.Object
import Qtc.ClassTypes.Core
import Qtc.ClassTypes.Gui
import Qtc.Core.Base
import Qtc.Core.QFile
import Qtc.Core.QFileInfo
import Qtc.Core.QIODevice
import Qtc.Enums.Core.QIODevice
import Qtc.Core.QVariant
import Qtc.Gui.QFont
import Qtc.Gui.QTextDocument
import Qtc.Gui.QTextEdit
import Qtc.Enums.Gui.QTextEdit

type EditorWidget = QTextEditSc (CEditorWidget)
data CEditorWidget = CEditorWidget

editorWidget :: QWidget t1 -> String -> IO (EditorWidget)
editorWidget parent filename = do
    this <- qSubClass $ qTextEdit (parent)
    setAcceptRichText this False
    setLineWrapMode this eNoWrap
    connectSlot this "textChanged()" this "on_textChanged()" $ editorWidget_textChanged

    doc <- document this ()
    setDefaultFont doc =<< qFont ("Courier", 12 :: Int)

    if (filename /= "")
        then do
            editorWidget_loadDocument this filename
            return ()
        else do
            --qObjectSetProperty this "filename" =<< qVariant [""]
            qObjectSetProperty this "modified" =<< qVariant True
            return ()

    return this

editorWidget_textChanged :: EditorWidget -> IO ()
editorWidget_textChanged this = do
    modified <- editorWidget_isModified this
    if modified
        then return ()
        else do
            qObjectSetProperty this "modified" =<< qVariant True
            emitSignal this "titleChanged()" ()
            return ()
    return ()

editorWidget_fileName :: EditorWidget -> IO (String)
editorWidget_fileName this = do
    --filename <- qVariantValue_QStringList =<< qObjectProperty this "filename"
    --return (head filename)
    return "File"

editorWidget_baseName :: EditorWidget -> IO (String)
editorWidget_baseName this = do
    pathname <- editorWidget_fileName this
    info <- qFileInfo pathname
    return =<< fileName info ()

editorWidget_isModified :: EditorWidget -> IO (Bool)
editorWidget_isModified this = do
    return =<< qVariantValue_Bool =<< qObjectProperty this "modified"

editorWidget_title :: EditorWidget -> IO (String)
editorWidget_title this = do
    filename <- editorWidget_baseName this
    modified <- editorWidget_isModified this
    return ((if (filename /= "") then filename else "New") ++ (if modified then "*" else ""))

editorWidget_text :: EditorWidget -> IO (String)
editorWidget_text this = do
    doc <- document this ()
    return =<< toPlainText doc ()

editorWidget_loadDocument :: EditorWidget -> String -> IO ()
editorWidget_loadDocument this filename = do
    file <- qFile filename
    open file fReadOnly

    doc <- document this ()
    setPlainText doc =<< readAll file ()

    --qObjectSetProperty this "filename" =<< qVariant [filename]
    qObjectSetProperty this "modified" =<< qVariant False
    emitSignal this "titleChanged()" () -- FIXME: signal not received

    close file ()
    return ()

editorWidget_saveDocument :: EditorWidget -> String -> IO ()
editorWidget_saveDocument this filename = do
    file <- qFile filename
    open file fWriteOnly

    text <- editorWidget_text this
    write file text -- FIXME: does not write anything

    --qObjectSetProperty this "filename" =<< qVariant [filename]
    qObjectSetProperty this "modified" =<< qVariant False
    emitSignal this "titleChanged()" () -- FIXME: signal not received

    close file ()
    return ()

qCast_EditorWidget :: Object a -> IO (EditorWidget)
qCast_EditorWidget obj = return (objectCast obj)
