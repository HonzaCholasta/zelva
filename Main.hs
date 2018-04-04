module Main where

import Qtc.Classes.Qccs
import Qtc.Classes.Gui
import Qtc.Core.Base
import Qtc.Gui.Base
import Qtc.Gui.QApplication
import MainWindow

main :: IO Int
main = do
    qApplication ()

    foo <- registerResource "turtle.rcc"

    window <- mainWindow
    qshow window ()

    qApplicationExec ()
