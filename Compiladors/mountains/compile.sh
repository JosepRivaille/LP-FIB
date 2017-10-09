antlr -gt mountains.g
dlg parser.dlg scan.c
g++ -w -o mountains mountains.c scan.c err.c
