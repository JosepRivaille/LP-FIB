#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: lego.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define GRID 2
#define PLACE 3
#define AT 4
#define ASSIGN 5
#define NUM 6
#define MOVE 7
#define DIRECTION 8
#define WHILE 9
#define FITS 10
#define HEIGHT 11
#define AND 12
#define GT 13
#define LT 14
#define LPAR 15
#define RPAR 16
#define LBRA 17
#define RBRA 18
#define COMMA 19
#define DEF 20
#define ENDEF 21
#define ID 22
#define SPACE 23

#ifdef __USE_PROTOS
void lego(AST**_root);
#else
extern void lego();
#endif

#ifdef __USE_PROTOS
void map(AST**_root);
#else
extern void map();
#endif

#ifdef __USE_PROTOS
void linstr(AST**_root);
#else
extern void linstr();
#endif

#ifdef __USE_PROTOS
void instr(AST**_root);
#else
extern void instr();
#endif

#ifdef __USE_PROTOS
void assign(AST**_root);
#else
extern void assign();
#endif

#ifdef __USE_PROTOS
void pairNum(AST**_root);
#else
extern void pairNum();
#endif

#ifdef __USE_PROTOS
void move(AST**_root);
#else
extern void move();
#endif

#ifdef __USE_PROTOS
void loop(AST**_root);
#else
extern void loop();
#endif

#ifdef __USE_PROTOS
void defs(AST**_root);
#else
extern void defs();
#endif

#ifdef __USE_PROTOS
void def(AST**_root);
#else
extern void def();
#endif

#ifdef __USE_PROTOS
void boolexpr(AST**_root);
#else
extern void boolexpr();
#endif

#ifdef __USE_PROTOS
void height(AST**_root);
#else
extern void height();
#endif

#ifdef __USE_PROTOS
void numexp(AST**_root);
#else
extern void numexp();
#endif

#ifdef __USE_PROTOS
void fits(AST**_root);
#else
extern void fits();
#endif

#endif
extern SetWordType zzerr1[];
extern SetWordType zzerr2[];
extern SetWordType zzerr3[];
extern SetWordType setwd1[];
extern SetWordType zzerr4[];
extern SetWordType zzerr5[];
extern SetWordType zzerr6[];
extern SetWordType zzerr7[];
extern SetWordType setwd2[];
extern SetWordType setwd3[];
