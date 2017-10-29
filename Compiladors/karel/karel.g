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
    if (type == DEFINEID) {
        attr->kind = "id";
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

typedef vector<pair<int, int> > Row; // Direction wall - sensor
typedef vector<Row> Matrix;

struct Robot {
    int x;
    int y;
    int sensors;
    int orientation;
};

struct World {
    int w;
    int h;
    Matrix m;
};

Robot R;
World W;
unordered_map<string, AST*> Definitions;
unordered_map<string, AST*>::iterator DefinitionsIT;


bool dinsDominis(int x, int y) {
    return (x > 0 and x <= W.w) and (y > 0 and y <= W.h);
}

bool isClear(int x, int y, int o) {
    return W.m[x][y].first == o;
}

bool evaluateCondition(AST* a) {
    string k = a->kind;
    if (k == "and") {
        bool first = evaluateCondition(child(a, 0));
        return first and evaluateCondition(child(a, 1));
    } else if (k == "or") {
        bool first = evaluateCondition(child(a, 0));
        return first or evaluateCondition(child(a, 1));
    } else if (k == "not") {
        return !evaluateCondition(child(a, 0));
    } else if (k == "foundBeeper") {
        return W.m[R.x][R.y].second > 0;
    } else if (k == "isClear") {
        return isClear(R.x, R.y, R.orientation);
    } else if (k == "anyBeepersInBag") {
        return R.sensors > 0;
    }
    return true;
}

AST* findDefinition(string id) {
    DefinitionsIT = Definitions.find(id);
    if (DefinitionsIT != Definitions.end())
        return DefinitionsIT->second;
    throw InvalidDefinitionException(id);
}

void evaluateMain(AST* a) {
    int ithChild = 0;
    while (AST* instr = child(a, ithChild++)) {
        string k = instr->kind;
        if (k == "iterate") {
            int iterations = stoi(child(instr, 0)->kind);
            for (unsigned int i = 0; i < iterations; ++i)
                evaluateMain(child(instr, 1));
        } else if (k == "if") {
            if (evaluateCondition(child(instr, 0)))
                evaluateMain(child(instr, 1));
        } else if (k == "move") {
            bool willMove;
            int x, y;
            switch (R.orientation) {
                case 0:
                    x = R.x; y = R.y + 1; break;
                case 1:
                    x = R.x + 1; y = R.y; break;
                case 2:
                    x = R.x; y = R.y - 1; break;
                default:
                    x = R.x - 1; y = R.y;
            }
            if (dinsDominis(x, y) && isClear(x, y, R.orientation))
                R.x = x; R.y = y;
        } else if (k == "turnoff") {
            throw StopProgramException();
        } else if (k == "turnleft") {
            R.orientation = (R.orientation + 1) % 4;
        } else if (k == "id") {
            evaluateMain(findDefinition(instr->text));
        }
    }
}

void novaPosicio(AST* a) {
    AST* world = child(a, 0);
    int w = stoi(child(world, 0)->kind);
    int h = stoi(child(world, 1)->kind);
    W = {w, h, Matrix(w, Row(h, {0, 4}))};

    AST* robot = child(a, 1);
    int x = stoi(child(robot, 0)->kind);
    int y = stoi(child(robot, 1)->kind);
    int s = stoi(child(robot, 2)->kind);
    string orientation = child(robot, 3)->kind;
    int o = 3;
    if (orientation == "up") o = 0;
    else if (orientation == "right") o = 1;
    else if (orientation == "down") o = 2;
    R = {x, y, s, o};

    AST* elems = child(a, 2);
    int ithChild = 0;
    while (AST* elem = child(elems, ithChild++)) {
        if (elem->kind == "define")
            Definitions.insert({child(elem, 0)->text, child(elem, 1)});
        else if (elem->kind == "walls") {
            int posX = stoi(child(elem, 0)->kind);
            int posY = stoi(child(elem, 1)->kind);
            orientation = child(elem, 2)->kind;
            int o = 3;
            if (orientation == "up") o = 0;
            else if (orientation == "right") o = 1;
            else if (orientation == "down") o = 2;
            W.m[posX][posY].first = o;
        } else {
            int posX = stoi(child(elem, 0)->kind);
            int posY = stoi(child(elem, 1)->kind);
            W.m[posX][posY].second = stoi(child(elem, 2)->kind);
        }
    }

    AST* main = child(a, 3);
    try {
        evaluateMain(main);
    } catch (exception &e) {
        if (e.what() != "STOP")
            cerr << e.what() << endl;
    } cout << "(" << R.x << ", " << R.y << ")" << endl;
}

int main(int argc, char** argv) {
    root = NULL;
    ANTLR(karel(&root), stdin);
    ASTPrint(root);
    novaPosicio(root);
}
>>


#lexclass START
// Keywords
#token WORLD "world"
#token ROBOT "robot"
#token ORIENTATION "(left | right | up | down)"
#token WALLS "walls"
#token BEEPERS "beepers"
#token NUM "[0-9]+"
#token IF "if"
#token AND "and"
#token OR "or"
#token NOT "not"
#token ISCLEAR "isClear"
#token ANYBEEPERS "anyBeepersInBag"
#token TURNLEFT "turnleft"
#token MOVE "move"
#token PUTBEEPER "putbeeper"
#token BEGIN "begin"
#token END "end"
#token ITERATE "iterate"
#token FOUNDBEEPER "foundBeeper"
#token PICKBEEPER "pickBeeper"
#token TURNOFF "turnoff"
#token C "\,"
#token SC "\;"
#token LC "\{"
#token RC "\}"
#token LB "\["
#token RB "\]"
#token DEFINE "define"
#token DEFINEID "[a-zA-Z][a-zA-Z0-9_\-]*"
// WhiteSpaces
#token SPACE "[\ \t\n\s]" << zzskip(); >>

karel: world robot elems BEGIN! linstr END! << #0 = createASTlist(_sibling); >>;

world: WORLD^ NUM NUM;
robot: ROBOT^ NUM NUM NUM ORIENTATION;
elems: (wall | beeper | definition)* << #0 = createASTlist(_sibling); >>;

wall: WALLS^ LB! NUM NUM ORIENTATION (C! NUM NUM ORIENTATION)* RB!;
beeper: BEEPERS^ NUM NUM NUM;
definition: DEFINE^ DEFINEID LC! linstr RC!;

linstr: (instr)+ << #0 = createASTlist(_sibling); >>;
instr: (exeFunc | cond | loop);

exeFunc: (MOVE | PUTBEEPER | PICKBEEPER | TURNLEFT | TURNOFF | DEFINEID) SC!;
cond: IF^ booleanExpr LC! linstr RC!;
loop: ITERATE^ NUM LC! linstr RC!;

booleanExpr: booleanExprP ((OR^ | AND^) booleanExprP)*;
booleanExprP: (NOT^ |) (ISCLEAR | ANYBEEPERS | FOUNDBEEPER);
