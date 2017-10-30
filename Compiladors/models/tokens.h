#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: models.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define START 2
#define PAR 3
#define EXC 4
#define INC 5
#define SEQ 6
#define LPAR 7
#define RPAR 8
#define END 9
#define CONNECTION 10
#define FILEP 11
#define FILEDIR 12
#define QUERIES 13
#define CRITICAL 14
#define DIFFERENCE 15
#define CORRECTFILE 16
#define ID 17
#define SPACE 18

#ifdef __USE_PROTOS
void model(AST**_root);
#else
extern void model();
#endif

#ifdef __USE_PROTOS
void defs(AST**_root);
#else
extern void defs();
#endif

#ifdef __USE_PROTOS
void role(AST**_root);
#else
extern void role();
#endif

#ifdef __USE_PROTOS
void connection(AST**_root);
#else
extern void connection();
#endif

#ifdef __USE_PROTOS
void file(AST**_root);
#else
extern void file();
#endif

#ifdef __USE_PROTOS
void filep(AST**_root);
#else
extern void filep();
#endif

#ifdef __USE_PROTOS
void queries(AST**_root);
#else
extern void queries();
#endif

#ifdef __USE_PROTOS
void critical(AST**_root);
#else
extern void critical();
#endif

#ifdef __USE_PROTOS
void difference(AST**_root);
#else
extern void difference();
#endif

#ifdef __USE_PROTOS
void correctfile(AST**_root);
#else
extern void correctfile();
#endif

#ifdef __USE_PROTOS
void processP0(AST**_root);
#else
extern void processP0();
#endif

#ifdef __USE_PROTOS
void processP1(AST**_root);
#else
extern void processP1();
#endif

#ifdef __USE_PROTOS
void processP2(AST**_root);
#else
extern void processP2();
#endif

#ifdef __USE_PROTOS
void processP3(AST**_root);
#else
extern void processP3();
#endif

#ifdef __USE_PROTOS
void processP4(AST**_root);
#else
extern void processP4();
#endif

#endif
extern SetWordType setwd1[];
extern SetWordType zzerr1[];
extern SetWordType setwd2[];
