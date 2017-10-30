RM = rm -f
OBJS = *.exe *.o *.c *.h *.dlg

karel:
	antlr -gt lego.g
	dlg parser.dlg scan.c
	g++ -std=c++11 -w -o lego.exe lego.c scan.c err.c -I ../pccts/h/

clean:
	$(RM) $(OBJS)
