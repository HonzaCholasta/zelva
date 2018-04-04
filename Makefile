all: turtle turtle.rcc

clean:
	rm -f turtle turtle.rcc *.hi *.o *~

turtle: EditorWidget.hs Main.hs MainWindow.hs RenderWidget.hs Turtle.hs
	env PATH=/usr/local/share/Qt/bin:$(PATH):/usr/local/share/ghc/bin ghc --make Main -package qt -fglasgow-exts -O2 -i. -o turtle

turtle.rcc: turtle.qrc forms/MainWindow.ui
	env PATH=/usr/local/share/Qt/bin:$(PATH) rcc -binary -o turtle.rcc turtle.qrc

.PHONY: all clean
