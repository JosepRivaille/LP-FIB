RM = rm -f
OBJS = *.exe *.o *.c *.h *.dlg

roomba:
	antlr -gt roomba.g
	dlg parser.dlg scan.c
	g++ -std=c++11 -w -o roomba.exe roomba.c scan.c err.c -I ../pccts/h/

clean:
	$(RM) $(OBJS)
