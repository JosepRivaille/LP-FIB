#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: main.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define NUM 2
#define PLUS 3
#define MINUS 4
#define MULT 5
#define DIV 6
#define SPACE 7

#ifdef __USE_PROTOS
void input(AST**_root);
#else
extern void input();
#endif

#ifdef __USE_PROTOS
void expr(AST**_root);
#else
extern void expr();
#endif

#ifdef __USE_PROTOS
void term(AST**_root);
#else
extern void term();
#endif

#endif
extern SetWordType zzerr1[];
extern SetWordType setwd1[];
