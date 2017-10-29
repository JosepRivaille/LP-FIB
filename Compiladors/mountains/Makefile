RM = rm -f
OBJS = *.exe *.o *.c *.h *.dlg out*

mountains:
	antlr -gt mountains.g
	dlg parser.dlg scan.c
	g++ -std=c++11 -w -o mountains.exe mountains.c scan.c err.c -I ../pccts/h/

clean:
	$(RM) $(OBJS)
