all: zelva zelva.rcc

clean:
	rm -f zelva zelva.rcc *.hi *.o *~

zelva: qt-installed EditorWidget.hs Main.hs MainWindow.hs RenderWidget.hs Turtle.hs
	ghc --make Main -dynamic -package qt -fglasgow-exts -O2 -i. -o zelva

zelva.rcc: zelva.qrc forms/MainWindow.ui
	rcc-qt4 -binary -o zelva.rcc zelva.qrc

qt-installed:
	./install-qt.sh

.PHONY: all clean qt-installed
