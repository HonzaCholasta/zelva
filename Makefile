all: zelva zelva.rcc

clean:
	rm -f zelva zelva.rcc *.hi *.o *~

zelva: EditorWidget.hs Main.hs MainWindow.hs RenderWidget.hs Turtle.hs
	env PATH=/usr/local/share/Qt/bin:$(PATH):/usr/local/share/ghc/bin ghc --make Main -package qt -fglasgow-exts -O2 -i. -o zelva

zelva.rcc: zelva.qrc forms/MainWindow.ui
	env PATH=/usr/local/share/Qt/bin:$(PATH) rcc -binary -o zelva.rcc zelva.qrc

.PHONY: all clean
