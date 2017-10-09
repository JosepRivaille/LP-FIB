#header 
<< #include "charptr.h" >>

<<
#include "charptr.c"

int main() {
   ANTLR(input(), stdin);
}
>>

#lexclass START
#token NUM "[0-9]+"
#token PLUS "\+"
#token MINUS "\-"
#token SPACE "[\ \n]" << zzskip(); >>

input: expr "@";
expr: NUM (PLUS expr | MINUS expr |);

//expr: NUM (PLUS NUM)* "@";
//expr1: expr PLUS expr | NUM; //Primer terme és recursiu -> StackOverflow
//expr2: NUM PLUS expr | NUM; //Genera expressió ambigua
//expr2: NUM (PLUS expr |);
//expr3: expr PLUS NUM | NUM; //Primer terme és recursiu -> StackOverflow
//expr4: NUM | NUM PLUS expr; // Genera expressió ambigua
//expr4: NUM (PLUS expr |);
