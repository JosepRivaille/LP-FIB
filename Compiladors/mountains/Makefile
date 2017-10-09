RM = rm -f
OBJS = *.exe *.o *.c *.h *.dlg

mountains:
	antlr -gt mountains.g
	dlg parser.dlg scan.c
	g++ -w -o mountains.exe mountains.c scan.c err.c -I ../pccts/h/

clean:
	$(RM) $(OBJS)
