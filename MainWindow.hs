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

module MainWindow where

import Qtc.Classes.Qccs
import Qtc.Classes.Qccs_h
import Qtc.Classes.Core
import Qtc.Enums.Classes.Core
import Qtc.Classes.Gui
import Qtc.ClassTypes.Core
import Qtc.ClassTypes.Gui
import Qtc.Core.Base
import Qtc.Core.QFile
import Qtc.Core.QCoreApplication
import Qtc.Core.QEvent
import Qtc.Enums.Core.QIODevice
import Qtc.Gui.QDockWidget
import Qtc.Gui.QFileDialog
import Qtc.Gui.QMainWindow_h
import Qtc.Gui.QMenu
import Qtc.Gui.QMessageBox
import Qtc.Enums.Gui.QMessageBox
import Qtc.Gui.QTabWidget
import Qtc.Gui.QTextCursor
import Qtc.Gui.QTextEdit
import Qtc.Tools.QUiLoader

import EditorWidget
import RenderWidget
import Turtle

type MainWindow = QMainWindowSc (CMainWindow)
data CMainWindow = CMainWindow

mainWindow :: IO (MainWindow)
mainWindow = do
    loader <- qUiLoader ()
    file <- qFile ":/forms/MainWindow.ui"
    open file fReadOnly
    ui <- load loader file
    close file ()

    this <- qSubClass $ qCast_QMainWindow ui
    --setHandler this "closeEvent(QCloseEvent*)" $ mainWindow_closeEvent -- FIXME: pada

    viewDock <- qCast_QDockWidget =<< findChild this ("<QDockWidget*>", "viewDock")
    actionToggleViewDock <- toggleViewAction viewDock ()

    messagesDock <- qCast_QDockWidget =<< findChild this ("<QDockWidget*>", "messagesDock")
    actionToggleMessagesDock <- toggleViewAction messagesDock ()

    menuZobrazit <- qCast_QMenu =<< findChild this ("<QMenu*>", "menu_Zobrazit")
    addAction menuZobrazit actionToggleViewDock
    addAction menuZobrazit actionToggleMessagesDock

    view <- renderWidget viewDock Nothing
    setWidget viewDock (view)

    actionFileNew <- findChild this ("<QAction*>", "action_file_new")
    connectSlot actionFileNew "triggered()" this "on_action_file_new()" $ mainWindow_onFileNew

    actionFileOpen <- findChild this ("<QAction*>", "action_file_open")
    connectSlot actionFileOpen "triggered()" this "on_action_file_open()" $ mainWindow_onFileOpen

    actionFileSave <- findChild this ("<QAction*>", "action_file_save")
    connectSlot actionFileSave "triggered()" this "on_action_file_sage()" $ mainWindow_onFileSave

    actionFileSaveAs <- findChild this ("<QAction*>", "action_file_save_as")
    connectSlot actionFileSaveAs "triggered()" this "on_action_file_save_as()" $ mainWindow_onFileSaveAs

    actionFileSaveAll <- findChild this ("<QAction*>", "action_file_save_all")
    connectSlot actionFileSaveAll "triggered()" this "on_action_file_save_all()" $ mainWindow_onFileSaveAll

    actionFileClose <- findChild this ("<QAction*>", "action_file_close")
    connectSlot actionFileClose "triggered()" this "on_action_file_close()" $ mainWindow_onFileClose

    actionFileQuit <- findChild this ("<QAction*>", "action_file_quit")
    connectSlot actionFileQuit "triggered()" this "on_action_file_quit()" $ mainWindow_onFileQuit

    actionEditUndo <- findChild this ("<QAction*>", "action_edit_undo")
    connectSlot actionEditUndo "triggered()" this "on_action_edit_undo()" $ mainWindow_onEditUndo

    actionEditRedo <- findChild this ("<QAction*>", "action_edit_redo")
    connectSlot actionEditRedo "triggered()" this "on_action_edit_redo()" $ mainWindow_onEditRedo

    actionEditCut <- findChild this ("<QAction*>", "action_edit_cut")
    connectSlot actionEditCut "triggered()" this "on_action_edit_cut()" $ mainWindow_onEditCut

    actionEditCopy <- findChild this ("<QAction*>", "action_edit_copy")
    connectSlot actionEditCopy "triggered()" this "on_action_edit_copy()" $ mainWindow_onEditCopy

    actionEditPaste <- findChild this ("<QAction*>", "action_edit_paste")
    connectSlot actionEditPaste "triggered()" this "on_action_edit_paste()" $ mainWindow_onEditPaste

    actionEditDelete <- findChild this ("<QAction*>", "action_edit_delete")
    connectSlot actionEditDelete "triggered()" this "on_action_edit_delete()" $ mainWindow_onEditDelete

    actionEditSelectAll <- findChild this ("<QAction*>", "action_edit_select_all")
    connectSlot actionEditSelectAll "triggered()" this "on_action_edit_select_all()" $ mainWindow_onEditSelectAll

    actionRun <- findChild this ("<QAction*>", "action_run")
    connectSlot actionRun "triggered()" this "on_action_run()" $ mainWindow_onRun

    tabs <- findChild this ("<QTabWidget*>", "tabWidget")
    connectSlot tabs "currentChanged(int)" this "tabChanged(int)" $ mainWindow_tabChanged
    connectSlot tabs "tabCloseRequested(int)" this "closeTab(int)" $ mainWindow_closeTab

    mainWindow_message this "Želva 1.0\n© 2018 Jan Cholasta"

    return this

mainWindow_closeEvent :: MainWindow -> QCloseEvent () -> IO ()
mainWindow_closeEvent this event = do
    mainWindow_closeAllTabs this
    accept event ()

mainWindow_onFileNew :: MainWindow -> IO ()
mainWindow_onFileNew this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    editor <- editorWidget this ""
    title <- editorWidget_title editor
    num <- addTab tabs (editor, title)
    setCurrentIndex tabs num
    connectSlot editor "titleChanged()" this "tabTitleChanged()" $ mainWindow_tabTitleChanged num
    return ()

mainWindow_onFileOpen :: MainWindow -> IO ()
mainWindow_onFileOpen this = do
    filename <- qFileDialogGetOpenFileName (this, "Otevřít soubor", ".", "Soubory LParseru (*.ls);;Všechny soubory (*)")
    if (length filename) > 0
        then do
            tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
            editor <- editorWidget this filename
            title <- editorWidget_title editor
            num <- addTab tabs (editor, title)
            setCurrentIndex tabs num
            connectSlot editor "titleChanged()" this "tabTitleChanged()" $ mainWindow_tabTitleChanged num
            return ()
        else return ()
    return ()

mainWindow_onFileSave :: MainWindow -> IO ()
mainWindow_onFileSave this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    num <- currentIndex tabs ()
    mainWindow_saveTab this num False
    return ()

mainWindow_onFileSaveAs :: MainWindow -> IO ()
mainWindow_onFileSaveAs this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    num <- currentIndex tabs ()
    mainWindow_saveTab this num True
    return ()

mainWindow_onFileSaveAll :: MainWindow -> IO ()
mainWindow_onFileSaveAll this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    count <- count tabs ()
    foldr (\ i r -> mainWindow_saveTab this i False >> r) (return ()) [0..(count-1)]
    return ()

mainWindow_onFileClose :: MainWindow -> IO ()
mainWindow_onFileClose this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    num <- currentIndex tabs ()
    mainWindow_closeTab this num
    return ()

mainWindow_onFileQuit :: MainWindow -> IO ()
mainWindow_onFileQuit this = do
    mainWindow_closeAllTabs this
    qCoreApplicationExit ()
    return ()

mainWindow_onEditUndo :: MainWindow -> IO ()
mainWindow_onEditUndo this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    editor <- qCast_EditorWidget =<< currentWidget tabs ()
    undo editor ()
    return ()

mainWindow_onEditRedo :: MainWindow -> IO ()
mainWindow_onEditRedo this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    editor <- qCast_EditorWidget =<< currentWidget tabs ()
    redo editor ()
    return ()

mainWindow_onEditCut :: MainWindow -> IO ()
mainWindow_onEditCut this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    editor <- qCast_EditorWidget =<< currentWidget tabs ()
    cut editor ()
    return ()

mainWindow_onEditCopy :: MainWindow -> IO ()
mainWindow_onEditCopy this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    editor <- qCast_EditorWidget =<< currentWidget tabs ()
    copy editor ()
    return ()

mainWindow_onEditPaste :: MainWindow -> IO ()
mainWindow_onEditPaste this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    editor <- qCast_EditorWidget =<< currentWidget tabs ()
    paste editor ()
    return ()

mainWindow_onEditDelete :: MainWindow -> IO ()
mainWindow_onEditDelete this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    editor <- qCast_EditorWidget =<< currentWidget tabs ()
    cursor <- textCursor editor ()
    removeSelectedText cursor ()
    return ()

mainWindow_onEditSelectAll :: MainWindow -> IO ()
mainWindow_onEditSelectAll this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    editor <- qCast_EditorWidget =<< currentWidget tabs ()
    selectAll editor ()
    return ()

mainWindow_onRun :: MainWindow -> IO ()
mainWindow_onRun this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    count <- count tabs ()

    if count > 0
        then do
            editor <- qCast_EditorWidget =<< currentWidget tabs ()
            text <- editorWidget_text editor

            viewDock <- qCast_QDockWidget =<< findChild this ("<QDockWidget*>", "viewDock")
            view <- qCast_RenderWidget =<< widget viewDock ()

            let result = parseLSystem text
            if result /= Nothing
                then renderWidget_update view result
                else return ()

            mainWindow_message this $ "Výstup: " ++ (show result)
            return ()
        else return ()

    return ()

mainWindow_tabChanged :: MainWindow -> Int -> IO ()
mainWindow_tabChanged this num = do
    return ()

mainWindow_tabTitleChanged :: Int -> MainWindow -> IO ()
mainWindow_tabTitleChanged num this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    editor <- qCast_EditorWidget =<< currentWidget tabs ()
    title <- editorWidget_title editor
    setTabText tabs (num, title)
    return ()

mainWindow_saveTab :: MainWindow -> Int -> Bool -> IO ()
mainWindow_saveTab this num ask = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    editor <- qCast_EditorWidget =<< widget tabs num
    oldname <- editorWidget_fileName editor

    if (oldname == "" || ask)
        then do
            filename <- qFileDialogGetSaveFileName (this, "Uložit soubor", ".", "Soubory LParseru (*.ls);;Všechny soubory (*)")
            if (length filename) > 0
                then editorWidget_saveDocument editor filename
                else return ()
            return ()
        else do
            editorWidget_saveDocument editor oldname
            return ()

    return ()

mainWindow_closeTab :: MainWindow -> Int -> IO ()
mainWindow_closeTab this num = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")

    editor <- qCast_EditorWidget =<< widget tabs num
    modified <- editorWidget_isModified editor
    if modified
        then do
            filename <- editorWidget_baseName editor
            answer <- qMessageBoxQuestion (this, "Uložit soubor?", "Soubor " ++ filename ++ " byl změněn. Přejete si jej uložit?", (fYes+fNo+fCancel) :: QMessageBoxStandardButtons) :: IO QMessageBoxStandardButton
            if (answer == eYes)
                then mainWindow_saveTab this num True
                else return ()
            if (answer /= eCancel)
                then removeTab tabs num
                else return ()
            return ()
        else do
            removeTab tabs num
            return ()

    return ()

mainWindow_closeAllTabs :: MainWindow -> IO ()
mainWindow_closeAllTabs this = do
    tabs <- qCast_QTabWidget =<< findChild this ("<QTabWidget*>", "tabWidget")
    count <- count tabs ()
    foldr (\ i r -> mainWindow_closeTab this 0 >> r) (return ()) [0..(count-1)]
    return ()

mainWindow_message :: MainWindow -> String -> IO ()
mainWindow_message this text = do
    messages <- qCast_QTextEdit =<< findChild this ("<QTextEdit*>", "messages")
    append messages (text ++ "\n")
    ensureCursorVisible messages ()
    return ()
