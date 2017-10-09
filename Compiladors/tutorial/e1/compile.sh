antlr -gs main.g
dlg parser.dlg scan.c
gcc -o main main.c scan.c err.c
