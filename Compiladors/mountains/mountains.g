#header
<<
#include <string>
#include <iostream>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr, int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>

//global structures
AST *root;


// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
  if (type == ID) {
     attr->kind = "id";
     attr->text = text;
  }
  else if (type == NUM) {
    attr->kind = "intconst";
    attr->text = text;
  }
  else {
    attr->kind = text;
    attr->text = "";
  }
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
  for (int i=0; c!=NULL && i<n; i++) c = c->right;
  return c;
}


/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a, string s) {
  if (a == NULL) return;

  cout << a->kind;
  if (a->text != "") cout << "(" << a->text << ")";
  cout << endl;

  AST *i = a->down;
  while (i != NULL && i->right != NULL) {
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


int main() {
  root = NULL;
  ANTLR(program(&root), stdin);
  ASTPrint(root);
}
>>


#lexclass START
// Keywords
#token AND "AND"
#token OR "OR"
#token NOT "NOT"
#token IF "if"
#token ENDIF "endif"
#token WHILE "while"
#token ENDWHILE "endwhile"
#token CONCAT ";"
#token COMPARE "\< | \<= | \>= | \> | =="
// Definitions
#token NUM "[0-9]+"
#token MULT "\*"
#token DIRECTION "\/ | \- | \\"
// Functions
#token SHAPE "Peak | Valley"
#token MATCH "Match"
#token HEIGHT "Height"
#token WELLFORMED "Wellformed"
#token COMPLETE "Complete"
#token DRAW "Draw"
// Operators
#token ASSIGN "is"
#token ID "[A-Za-z][A-Za-z0-9_]*"
#token PLUS "\+"
#token MINUS "\$"
#token DIV  "\/"
// WhiteSpaces
#token SPACE "[\ \t\n\s]" << zzskip(); >>

program: (instruction)* << #0 = createASTlist(_sibling); >>;
instruction: assign | condition | loop | draw | complete;

assign: ID ASSIGN^ (mountain | height);
condition: IF^ "\("! boolexprP0 "\)"! program ENDIF!;
loop: WHILE^ "\("! boolexprP0 "\)"! program ENDWHILE!;
draw: DRAW^ "\("! mountain "\)"!;
complete: COMPLETE^ "\("! ID "\)"!;

mountain: part (CONCAT^ part)*;
part: shape | section | idref;

shape: SHAPE^ "\("! operationP0 ","! operationP0 ","! operationP0 "\)"!;
section: NUM (MULT^ DIRECTION |);
idref: "#"! ID;

height: HEIGHT^ "\("! idref "\)"!;
match: MATCH^ "\("! idref ","! idref "\)"!;
wellformed: WELLFORMED^ "\("! ID "\)"!;
comparation: operationP0 COMPARE^ operationP0;

numericexpr: NUM | height | ID;

boolexprP0: boolexprP1 (OR^ boolexprP1)*;
boolexprP1: boolexprP2 (AND^ boolexprP2)*;
boolexprP2: (NOT^ |) boolexprP3;
boolexprP3: match | wellformed | comparation;

operationP0: operationP1 ((PLUS^ | MINUS^) operationP1)*;
operationP1: numericexpr ((MULT^ | DIV^) numericexpr)*;
