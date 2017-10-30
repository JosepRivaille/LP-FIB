/*
 * A n t l r  S e t s / E r r o r  F i l e  H e a d e r
 *
 * Generated from: lego.g
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

class KarelCompilerException : public exception {
  protected:
  string errorMsg = "ERROR: ";
  public:
  virtual ~KarelCompilerException() throw() {}
  virtual const char* what() const throw() {
    return errorMsg.c_str();
  }
};

class StopProgramException : public KarelCompilerException {
  public:
  StopProgramException() {
    errorMsg = "STOP";
  }
};

class InvalidDefinitionException : public KarelCompilerException {
  public:
  InvalidDefinitionException(string id) {
    errorMsg += "Definition " + id + " doesn't exist.";
  }
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

ANTLRChar *zztokens[24]={
	/* 00 */	"Invalid",
	/* 01 */	"@",
	/* 02 */	"GRID",
	/* 03 */	"PLACE",
	/* 04 */	"AT",
	/* 05 */	"ASSIGN",
	/* 06 */	"NUM",
	/* 07 */	"MOVE",
	/* 08 */	"DIRECTION",
	/* 09 */	"WHILE",
	/* 10 */	"FITS",
	/* 11 */	"HEIGHT",
	/* 12 */	"AND",
	/* 13 */	"GT",
	/* 14 */	"LT",
	/* 15 */	"LPAR",
	/* 16 */	"RPAR",
	/* 17 */	"LBRA",
	/* 18 */	"RBRA",
	/* 19 */	"COMMA",
	/* 20 */	"DEF",
	/* 21 */	"ENDEF",
	/* 22 */	"ID",
	/* 23 */	"SPACE"
};
SetWordType zzerr1[4] = {0x80,0x2,0x40,0x0};
SetWordType zzerr2[4] = {0x0,0x80,0x40,0x0};
SetWordType zzerr3[4] = {0xa2,0x2,0x74,0x0};
SetWordType setwd1[24] = {0x0,0xfb,0x0,0x0,0x80,0x0,0x0,
	0xf6,0x0,0xf6,0x0,0x0,0x0,0x0,0x0,
	0x0,0x0,0x0,0xf8,0x0,0xfa,0xf8,0xf6,
	0x0};
SetWordType zzerr4[4] = {0x40,0xc,0x0,0x0};
SetWordType zzerr5[4] = {0x40,0xc,0x0,0x0};
SetWordType zzerr6[4] = {0x0,0x60,0x0,0x0};
SetWordType zzerr7[4] = {0x40,0x8,0x0,0x0};
SetWordType setwd2[24] = {0x0,0xf,0x0,0x0,0x0,0x0,0x30,
	0x3,0x0,0x3,0x0,0x30,0x80,0x0,0x0,
	0x0,0xc0,0x0,0x3,0x0,0xb,0x3,0x3,
	0x0};
SetWordType setwd3[24] = {0x0,0x0,0x0,0x0,0x0,0x0,0x0,
	0x0,0x0,0x0,0x0,0x0,0x3,0x1,0x1,
	0x0,0x3,0x0,0x0,0x0,0x0,0x0,0x0,
	0x0};
