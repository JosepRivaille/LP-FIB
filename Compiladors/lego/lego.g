#header
<<
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
>>

<<
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
>>


#lexclass START
// Keywords
#token GRID "Grid"
#token PLACE "PLACE"
#token AT "AT"
#token ASSIGN "="
#token NUM "[0-9]+"
#token MOVE "MOVE"
#token DIRECTION "NORTH | SOUTH | EAST | WEST"
#token WHILE "WHILE"
#token FITS "FITS"
#token HEIGHT "HEIGHT"
#token AND "AND"
#token GT "\>"
#token LT "\<"
#token LPAR "\("
#token RPAR "\)"
#token LBRA "\["
#token RBRA "\]"
#token COMMA ","
#token DEF "DEF"
#token ENDEF "ENDEF"
#token ID "[a-zA-Z][a-zA-Z0-9_\-]*"
// WhiteSpaces
#token SPACE "[\ \t\n\s]" << zzskip(); >>


lego: map linstr defs << #0 = createASTlist(_sibling); >>;

map: GRID^ NUM NUM;
linstr: (instr)* << #0 = createASTlist(_sibling); >>;
instr: move | loop | assign;
assign: ID (ASSIGN! PLACE^ pairNum AT! (pairNum | ID) |);
pairNum: LPAR! NUM COMMA! NUM RPAR! << #0 = createASTlist(_sibling); >>;
move: MOVE^ ID DIRECTION NUM;
loop: WHILE^ LPAR! boolexpr RPAR! LBRA! linstr RBRA!;
defs: (def)* << #0 = createASTlist(_sibling); >>;
def: DEF^ ID linstr ENDEF!;
boolexpr: (height | fits) (AND^ (height | fits))*;
height: numexp (GT^ | LT^) numexp;
numexp: HEIGHT^ LPAR! ID RPAR! | NUM;
fits: FITS^ LPAR! ID COMMA! NUM COMMA! NUM COMMA! NUM RPAR!;
