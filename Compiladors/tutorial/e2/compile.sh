antlr -gt main.g
dlg parser.dlg scan.c
g++ -o main main.c scan.c err.c
