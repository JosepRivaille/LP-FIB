/*
 * A n t l r  T r a n s l a t i o n  H e a d e r
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * With AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR33
 *
 *   antlr -gt lego.g
 *
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
#define GENAST

#include "ast.h"

#define zzSET_SIZE 4
#include "antlr.h"
#include "tokens.h"
#include "dlgdef.h"
#include "mode.h"

/* MR23 In order to remove calls to PURIFY use the antlr -nopurify option */

#ifndef PCCTS_PURIFY
#define PCCTS_PURIFY(r,s) memset((char *) &(r),'\0',(s));
#endif

#include "ast.c"
zzASTgvars

ANTLR_INFO

#include <cstdlib>
#include <cmath>
#include <vector>
#include <unordered_map>

//global structures
AST *root;

// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
  attr->kind = text;
  attr->text = "";
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind;
  as->text = attr->text;
  as->right = NULL;
  as->down = NULL;
  return as;
}

/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
  AST *as = new AST;
  as->kind = "list";
  as->right = NULL;
  as->down = child;
  return as;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a, int n) {
  AST *c = a->down;
  for (int i=0; c!=NULL and i<n; i++) c = c->right;
  return c;
}


/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a, string s) {
  if (a == NULL) return;
  
    cout << a->kind;
  if (a->text != "") cout << "(" << a->text << ")";
  cout << endl;
  
    AST *i = a->down;
  while (i != NULL and i->right != NULL) {
    cout << s+"  \\__";
    ASTPrintIndent(i, s+"  |"+string(i->kind.size()+i->text.size(), ' '));
    i = i->right;
  }
  
    if (i != NULL) {
    cout << s+"  \\__";
    ASTPrintIndent(i, s+"   "+string(i->kind.size()+i->text.size(), ' '));
    i = i->right;
  }
}

/// print AST
void ASTPrint(AST *a) {
  while (a != NULL) {
    cout << " ";
    ASTPrintIndent(a, "");
    a = a->right;
  }
}

int main(int argc, char** argv) {
  root = NULL;
  ANTLR(lego(&root), stdin);
  ASTPrint(root);
}

void
#ifdef __USE_PROTOS
lego(AST**_root)
#else
lego(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  map(zzSTR); zzlink(_root, &_sibling, &_tail);
  linstr(zzSTR); zzlink(_root, &_sibling, &_tail);
  defs(zzSTR); zzlink(_root, &_sibling, &_tail);
  (*_root) = createASTlist(_sibling);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x1);
  }
}

void
#ifdef __USE_PROTOS
map(AST**_root)
#else
map(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(GRID); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x2);
  }
}

void
#ifdef __USE_PROTOS
linstr(AST**_root)
#else
linstr(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (setwd1[LA(1)]&0x4) ) {
      instr(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  (*_root) = createASTlist(_sibling);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x8);
  }
}

void
#ifdef __USE_PROTOS
instr(AST**_root)
#else
instr(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==MOVE) ) {
    move(zzSTR); zzlink(_root, &_sibling, &_tail);
  }
  else {
    if ( (LA(1)==WHILE) ) {
      loop(zzSTR); zzlink(_root, &_sibling, &_tail);
    }
    else {
      if ( (LA(1)==ID) ) {
        assign(zzSTR); zzlink(_root, &_sibling, &_tail);
      }
      else {zzFAIL(1,zzerr1,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x10);
  }
}

void
#ifdef __USE_PROTOS
assign(AST**_root)
#else
assign(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    if ( (LA(1)==ASSIGN) ) {
      zzmatch(ASSIGN);  zzCONSUME;
      zzmatch(PLACE); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      pairNum(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzmatch(AT);  zzCONSUME;
      {
        zzBLOCK(zztasp3);
        zzMake0;
        {
        if ( (LA(1)==LPAR) ) {
          pairNum(zzSTR); zzlink(_root, &_sibling, &_tail);
        }
        else {
          if ( (LA(1)==ID) ) {
            zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
          }
          else {zzFAIL(1,zzerr2,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
        }
        zzEXIT(zztasp3);
        }
      }
    }
    else {
      if ( (setwd1[LA(1)]&0x20) ) {
      }
      else {zzFAIL(1,zzerr3,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x40);
  }
}

void
#ifdef __USE_PROTOS
pairNum(AST**_root)
#else
pairNum(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(LPAR);  zzCONSUME;
  zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(COMMA);  zzCONSUME;
  zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(RPAR); 
  (*_root) = createASTlist(_sibling);
 zzCONSUME;

  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x80);
  }
}

void
#ifdef __USE_PROTOS
move(AST**_root)
#else
move(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(MOVE); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(DIRECTION); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x1);
  }
}

void
#ifdef __USE_PROTOS
loop(AST**_root)
#else
loop(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(WHILE); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(LPAR);  zzCONSUME;
  boolexpr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(RPAR);  zzCONSUME;
  zzmatch(LBRA);  zzCONSUME;
  linstr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(RBRA);  zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x2);
  }
}

void
#ifdef __USE_PROTOS
defs(AST**_root)
#else
defs(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (LA(1)==DEF) ) {
      def(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  (*_root) = createASTlist(_sibling);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x4);
  }
}

void
#ifdef __USE_PROTOS
def(AST**_root)
#else
def(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(DEF); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  linstr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(ENDEF);  zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x8);
  }
}

void
#ifdef __USE_PROTOS
boolexpr(AST**_root)
#else
boolexpr(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    if ( (setwd2[LA(1)]&0x10) ) {
      height(zzSTR); zzlink(_root, &_sibling, &_tail);
    }
    else {
      if ( (LA(1)==FITS) ) {
        fits(zzSTR); zzlink(_root, &_sibling, &_tail);
      }
      else {zzFAIL(1,zzerr4,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
    zzEXIT(zztasp2);
    }
  }
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (LA(1)==AND) ) {
      zzmatch(AND); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      {
        zzBLOCK(zztasp3);
        zzMake0;
        {
        if ( (setwd2[LA(1)]&0x20) ) {
          height(zzSTR); zzlink(_root, &_sibling, &_tail);
        }
        else {
          if ( (LA(1)==FITS) ) {
            fits(zzSTR); zzlink(_root, &_sibling, &_tail);
          }
          else {zzFAIL(1,zzerr5,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
        }
        zzEXIT(zztasp3);
        }
      }
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x40);
  }
}

void
#ifdef __USE_PROTOS
height(AST**_root)
#else
height(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  numexp(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    if ( (LA(1)==GT) ) {
      zzmatch(GT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {
      if ( (LA(1)==LT) ) {
        zzmatch(LT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      }
      else {zzFAIL(1,zzerr6,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
    zzEXIT(zztasp2);
    }
  }
  numexp(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x80);
  }
}

void
#ifdef __USE_PROTOS
numexp(AST**_root)
#else
numexp(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==HEIGHT) ) {
    zzmatch(HEIGHT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
    zzmatch(LPAR);  zzCONSUME;
    zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    zzmatch(RPAR);  zzCONSUME;
  }
  else {
    if ( (LA(1)==NUM) ) {
      zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {zzFAIL(1,zzerr7,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x1);
  }
}

void
#ifdef __USE_PROTOS
fits(AST**_root)
#else
fits(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(FITS); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(LPAR);  zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(COMMA);  zzCONSUME;
  zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(COMMA);  zzCONSUME;
  zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(COMMA);  zzCONSUME;
  zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(RPAR);  zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x2);
  }
}
