#header
<<
#include <string>
#include <iostream>
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
>>

<<
#include <cstdlib>
#include <cmath>
#include <unordered_map>
#include <vector>
#include <string>
#include <exception>

class MountainCompilerException : public exception {
protected:
    string errorMsg = "ERROR: ";
public:
    virtual ~MountainCompilerException() throw() {}
    virtual const char* what() const throw() {
        return errorMsg.c_str();
    }
};

class InvalidTypeException : public MountainCompilerException {
public:
    InvalidTypeException(const string& id, const string & type) {
        errorMsg += id + " isn't a valid " + type + ".";
    }
};

class InvalidNumParameters : public MountainCompilerException {
public:
    InvalidNumParameters(const string& fun, const unsigned int& expected,
        const unsigned int& current) {
            errorMsg += "Function " + fun + " expected " + to_string(expected) +
                " parameters and it has " + to_string(current) + ".";
        }
};

class VariableNotDeclaredException : public MountainCompilerException {
public:
    VariableNotDeclaredException(const string& varName, const string& expectedType) {
            errorMsg += "Variable " + varName + " is not declared or is not a valid "
                + expectedType + ".";
        }
};

// Interpretation structures

typedef pair<unsigned int, char> MountainPart;
typedef vector<MountainPart> MountainStruct;

typedef unordered_map<string, int> NumericEnvironment;
typedef unordered_map<string, MountainStruct> DataEnvironment;

DataEnvironment DE;
DataEnvironment::iterator DEit;

NumericEnvironment NE;
NumericEnvironment::iterator NEit;


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

MountainStruct evalAssignShape(AST* a) {
    unsigned int expectedCount = 3;
    unsigned int childCount = 0;

    while (child(a, childCount) != NULL) ++childCount;
    if (childCount != expectedCount)
        throw InvalidNumParameters(a->kind, expectedCount, childCount);

    for (unsigned int i = 0; i < expectedCount; ++i)
        if (child(a, i)->kind != "intconst")
            throw InvalidTypeException(child(a, i)->text, "number");

    MountainStruct M(3);
    M[0] = {stoi(child(a, 0)->text), a->kind == "Peak" ? '/' : '\\'};
    M[1] = {stoi(child(a, 1)->text), '-'};
    M[2] = {stoi(child(a, 2)->text), a->kind == "Peak" ? '\\' : '/'};

    return M;
}

void evalAssignExpr(AST* a) {
    AST *childID = child(a, 0);
    if (childID->kind != "id") {
        throw InvalidTypeException(childID->text, "ID");
    }

    AST *valueID = child(a, 1);
    string kindValue = valueID->kind;
    if (kindValue == ";") { // Mountain section (3*/2*-1*\)
        // TODO
    } else if (kindValue == "Peak" || kindValue == "Valley") {
        DE.insert({childID->text, evalAssignShape(valueID)});
    } else if (kindValue == "id") { // Another variable
        // TODO
    } else { // Numeric value
        if (valueID->kind != "intconst")
            throw InvalidTypeException(valueID->kind, "mountain or numeric value");
        NE.insert({childID->text, stoi(valueID->text)});
        DEit = DE.find(childID->text);
        if (DEit != DE.end())
            DE.erase(DEit);
    }
}

void evalDrawMountain(AST* a) {
    AST *childID = child(a, 0);
    if (childID->kind == "id") {
        DEit = DE.find(childID->text);
        if (DEit != DE.end()) {
            for (unsigned int i = 0; i < DEit->second.size(); ++i)
                cout << string(DEit->second[i].first, DEit->second[i].second);
            cout << endl;
        } else throw VariableNotDeclaredException(childID->text, "mountain");
    }
}

void executeMountains(AST* a) {
    int ithChild = 0;
    while(child(a, ithChild)) {
        string kindChild = child(a, ithChild)->kind;
        try {
            if (kindChild == "is") {
                evalAssignExpr(child(a, ithChild));
            } else if (kindChild == "Draw") {
                evalDrawMountain(child(a, ithChild));
            }
        } catch(exception &e) {
            cerr << e.what() << endl;
        }
        ++ithChild;
    }
}


int main() {
    root = NULL;
    ANTLR(program(&root), stdin);
    //ASTPrint(root);
    executeMountains(root);
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
#token TIMES "\*"
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
#token MULT "\·"
#token DIV  "\/"
// WhiteSpaces
#token SPACE "[\ \t\n\s]" << zzskip(); >>

program: (instruction)* << #0 = createASTlist(_sibling); >>;
instruction: assign | condition | loop | draw | complete;

assign: ID ASSIGN^ (mountain);
condition: IF^ "\("! boolexprP0 "\)"! program ENDIF!;
loop: WHILE^ "\("! boolexprP0 "\)"! program ENDWHILE!;
draw: DRAW^ "\("! mountain "\)"!;
complete: COMPLETE^ "\("! ID "\)"!;

mountain: part (CONCAT^ part)*;
part: shape | section | idref;

shape: SHAPE^ "\("! operationP0 ","! operationP0 ","! operationP0 "\)"!;
section: operationP0 (TIMES^ DIRECTION |);
idref: "#"! ID;

operationP0: operationP1 ((PLUS^ | MINUS^) operationP1)*;
operationP1: numericexpr ((MULT^ | DIV^) numericexpr)*;

height: HEIGHT^ "\("! idref "\)"!;
match: MATCH^ "\("! idref ","! idref "\)"!;
wellformed: WELLFORMED^ "\("! ID "\)"!;
comparation: operationP0 COMPARE^ operationP0;

boolexprP0: boolexprP1 (OR^ boolexprP1)*;
boolexprP1: boolexprP2 (AND^ boolexprP2)*;
boolexprP2: (NOT^ |) boolexprP3;
boolexprP3: match | wellformed | comparation;

numericexpr: NUM | height | ID;
