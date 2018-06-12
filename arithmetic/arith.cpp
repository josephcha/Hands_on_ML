#include <stdio.h>
#include <bits/stdc++.h>
using namespace std;


stack<int> numst;
stack<int> opst;
int answer ; 
char inbuf[1024];
int mex[1024]; bool opterm[1024]; // if an entry(mex[i]) of mex array is an op term, opterm[i] = true

typedef enum {
    NOP,
    ADD,
    DEL,
    MUL,
    DIV,
    LPA,
    RPA,
} OP;

int prec[7] = { 0, 1, 1, 2, 2, 0, 0 };

inline bool not_lower(OP a, OP b) {
    return prec[a] >= prec[b];
}

inline bool is_num(char ch) {
    return (ch >= '0' && ch <='9'); 
}

inline int ex_op(OP op, int v1, int v2) {
    if (op == ADD) {
        return (v1 + v2);
    } else 
    if (op == DEL) {
        return (v1 - v2);
    } else
    if (op == MUL) {
        return (v1 * v2);
    } else
    if (op == DIV) {
        return (v1 / v2);
    } 
}


inline void skip_space(char **ptr)
{
    while((**ptr==' ')||(**ptr=='\t')) { *ptr += sizeof(**ptr); }
}

void calc()
{

    int term = 0;
    char ch ; 
    cin.getline(inbuf, sizeof inbuf);
    char *ptr = inbuf;
    
    for ( ; ; term++ ) {
        skip_space(&ptr);
        printf("after skip_space.. *ptr = %c\n", *ptr);
        if (*ptr=='\0') break;
        ch = *ptr;
        if (is_num(ch)) {
            int n = 0; 
            for (; is_num(ch); ptr++, ch = *ptr ) {
                n = n *10 + ch-'0';
 
            }
            mex[term] = n;
            opterm[term] = false;
        } else {
            if (ch == '+') {
                mex[term] = ADD;
            } else 
            if (ch == '-') {
                mex[term] = DEL;
            } else 
            if (ch == '*') {
                mex[term] = MUL;
            } else 
            if (ch == '/') {
                mex[term] = DIV;
            } else 
            if (ch == '(') {
                mex[term] = LPA;
            } else 
            if (ch == ')') {
                mex[term] = RPA;
            }
            opterm[term] = true;
            ptr++;
        }
    }
    printf("term %d\n", term);
    
    for (int i=0; i< term; i++) {
        if (opterm[i]) {
            int v1, v2;
            OP cop = (OP)mex[i];
            OP lop;
            if (cop == ADD || cop == DEL || cop == MUL || cop == DIV) {
                while (!opst.empty()) {
                    lop=(OP)opst.top();
                    if (not_lower(lop, cop)) {
                        v2 = numst.top();
                        numst.pop();
                        v1 = numst.top();
                        numst.pop();
                        numst.push(ex_op(lop, v1, v2));
                        opst.pop();
                    } else {
                        break;
                    }
                }
                opst.push(cop);
            } 
            else 
            if (cop == LPA) {
                opst.push(cop);
            } else 
            if (cop == RPA) {
                while ((!opst.empty()) && ((lop=(OP)opst.top())!=LPA) ) {
                    v2 = numst.top();
                    numst.pop();
                    v1 = numst.top();
                    numst.pop();
                    numst.push(ex_op(lop, v1, v2));
                    opst.pop();
                }
                if (!opst.empty()) {
                    opst.pop();
                }
            }
        } else {
            numst.push(mex[i]);
        }
    }
    while (!opst.empty()) {
        int v1, v2;
        OP op = (OP)opst.top();
        v2 = numst.top();
        numst.pop();
        v1 = numst.top();
        numst.pop();
        numst.push(ex_op(op, v1, v2));
        opst.pop();
    }
    answer = numst.top();

  
}


int main(int argc, char **argv)
{
    freopen("input.txt", "r", stdin);
    
  
    calc();

    cout << answer << endl;
	return 0;
}
