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

class RoombaCompilerException : public exception {
protected:
    string errorMsg = "ERROR: ";
public:
    virtual ~RoombaCompilerException() throw() {}
    virtual const char* what() const throw() {
        return errorMsg.c_str();
    }
};

class MustBeException : public RoombaCompilerException {
public:
    MustBeException(const string& id, const string& type) {
        errorMsg += id + " must be " + type + ".";
    }
};

class ComparationTypeException : public RoombaCompilerException {
public:
    ComparationTypeException(const string& id, const string& type) {
            errorMsg += id + " only accepts " + type + ".";
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

//global structures
AST *root;

// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
    if (type == TASKID) {
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

int SensorLight() {
    return 42;
}

string SensorProx() {
    return "OFF";
}

AST* findTask(string id) {
    AST* tasks = child(root, 2);
    int ithChild = 0;
    while (AST* task = child(tasks, ithChild++))
        if (child(task, 0)->text == id) {
            return child(task, 1);
        }
    throw MustBeException(id, "a existing task");
}

bool evalCompExpressions(AST* a, string k) {
    string firstK = child(a, 0)->kind;
    string secondK = child(a, 1)->kind;

    if (firstK == "sensorlight") {
        try {
            int f = SensorLight();
            int s = stoi(secondK);
            if (k == ">") return f > s;
            else return f == s;
        } catch(exception &e) {
            throw MustBeException(secondK, "a numeric expression");
        }
    } else if (firstK == "sensoraprox") {
        if (k == ">")
            throw ComparationTypeException(firstK, "== operator");
        if (secondK != "OFF" or secondK != "ON")
            throw MustBeException(secondK, "ON or OFF");
        return SensorProx() == secondK;
    }
}

bool evalBooleanExpression(AST *a) {
    string k = a->kind;
    if (k == "AND") {
        bool leftPart = evalBooleanExpression(child(a, 0));
        return leftPart and evalBooleanExpression(child(a, 1));
    } else if (k == "OR") {
        bool leftPart = evalBooleanExpression(child(a, 0));
        return leftPart or evalBooleanExpression(child(a, 1));
    } else if (k == ">" or k == "=") {
        return evalCompExpressions(a, k);
    }
}

pair<int, int> evalMovement(AST* a) {
    int ithChild = 0, x = 0, y = 0;
    while (AST* instruction = child(a, ithChild++)) {
        string k = instruction->kind;
        if (k == "move") {
            string direction = child(instruction, 0)->kind;
            int offset = stoi(child(instruction, 1)->kind);
            if (direction == "up") y += offset;
            else if (direction == "down") y -= offset;
            else if (direction == "left") x -= offset;
            else x += offset;
        } else if (k == "exec") {
            string taskID = child(instruction, 0)->text;
            AST* task = findTask(taskID);
            pair<int, int> i = evalMovement(task);
            x += i.first;
            y += i.second;
        } else if (k == "ops") {
            pair<int, int> i = evalMovement(instruction);
            x += i.first;
            y += i.second;
        } else if (k == "if") {
            bool exec = evalBooleanExpression(child(instruction, 0));
            if (exec) {
                pair<int, int> i = evalMovement(child(instruction, 1));
                x += i.first;
                y += i.second;
            }
        }
    }
    return {x, y};
}

void novaposicio(AST* a) {
    AST* position = child(a, 0);
    int x = stoi(child(position, 0)->kind);
    int y = stoi(child(position, 1)->kind);

    AST* instructions = child(a, 1);
    pair<int, int> movement = evalMovement(instructions);
    x += movement.first;
    y += movement.second;

    cout << "(" << x << ", " << y << ")" << endl;
}

int main(int argc, char** argv) {
    root = NULL;
    ANTLR(roomba(&root), stdin);
    ASTPrint(root);
    novaposicio(root);
}
>>


#lexclass START
// Keywords
#token AND "AND"
#token OR "OR"
#token ON "ON"
#token OFF "OFF"
#token NUM "[0-9]*"
#token POSITION "position"
#token STARTC "startcleaning"
#token ENDC "endcleaning"
#token MOVE "move"
#token DIRECTION "(up | down | left | right)"
#token SENSORLIGHT "sensorlight"
#token SENSORPROX "sensorprox"
#token FLUSH "flush"
#token EXEC "exec"
#token OPS "ops"
#token IF "if"
#token THEN "then"
#token EQ "=="
#token GT "\>"
#token TASK "TASK"
#token ENDTASK "ENDTASK"
#token TASKID "[a-zA-Z][a-zA-Z0-9]*"
// WhiteSpaces
#token SPACE "[\ \t\n\s]" << zzskip(); >>

roomba: position STARTC! linst ENDC! tasks << #0 = createASTlist(_sibling); >>;

position: POSITION^ NUM NUM;
linst: (inst)* << #0 = createASTlist(_sibling); >>;
inst: (move | flush | exec | ops | cond);

move: MOVE^ DIRECTION NUM;
flush: FLUSH^ NUM;
exec: EXEC^ TASKID;
ops: OPS^ "\["! inst ("\,"! inst)* "\]"!;

cond: IF^ boolexprP0 THEN! inst;

boolexprP0: boolexprP1 ((OR^ | AND^) boolexprP1)*;
boolexprP1: (SENSORLIGHT (EQ^ | GT^) NUM | SENSORPROX EQ^ (ON | OFF));

tasks: (task)* << #0 = createASTlist(_sibling); >>;
task: TASK^ TASKID linst ENDTASK!;
