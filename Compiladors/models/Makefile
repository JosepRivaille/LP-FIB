RM = rm -f
OBJS = *.exe *.o *.c *.h *.dlg

models:
	antlr -gt models.g
	dlg parser.dlg scan.c
	g++ -std=c++11 -w -o models.exe models.c scan.c err.c -I ../pccts/h/

clean:
	$(RM) $(OBJS)
