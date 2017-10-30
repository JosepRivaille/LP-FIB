/*
 * A n t l r  S e t s / E r r o r  F i l e  H e a d e r
 *
 * Generated from: models.g
 *
 * Terence Parr, Russell Quong, Will Cohen, and Hank Dietz: 1989-2001
 * Parr Research Corporation
 * with Purdue University Electrical Engineering
 * With AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR33
 */

#define ANTLR_VERSION	13333
#include "pcctscfg.h"
#include "pccts_stdio.h"

#include <string>
#include <iostream>
#include <exception>
using namespace std;

// struct to store information about tokens
struct Attrib {
  string kind;
  string text;
};

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr, int ttype, char *textt);
#define zzSET_SIZE 4
#include "antlr.h"
#include "ast.h"
#include "tokens.h"
#include "dlgdef.h"
#include "err.h"

ANTLRChar *zztokens[19]={
	/* 00 */	"Invalid",
	/* 01 */	"@",
	/* 02 */	"START",
	/* 03 */	"PAR",
	/* 04 */	"EXC",
	/* 05 */	"INC",
	/* 06 */	"SEQ",
	/* 07 */	"LPAR",
	/* 08 */	"RPAR",
	/* 09 */	"END",
	/* 10 */	"CONNECTION",
	/* 11 */	"FILEP",
	/* 12 */	"FILEDIR",
	/* 13 */	"QUERIES",
	/* 14 */	"CRITICAL",
	/* 15 */	"DIFFERENCE",
	/* 16 */	"CORRECTFILE",
	/* 17 */	"ID",
	/* 18 */	"SPACE"
};
SetWordType setwd1[19] = {0x0,0xb9,0x4,0x0,0x0,0x0,0x0,
	0x0,0x0,0x0,0x0,0x0,0x0,0x6,0x40,
	0x40,0x40,0x0,0x0};
SetWordType zzerr1[4] = {0x80,0x0,0x2,0x0};
SetWordType setwd2[19] = {0x0,0x7,0x0,0xf0,0xe0,0xc0,0x80,
	0x0,0xf8,0xf8,0x0,0x0,0x0,0x0,0x7,
	0x7,0x7,0x0,0x0};
