#include <bits/stdc++.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

using namespace std;

/* Program Description 
 *   Reverse mode audodiff calculation which is applicable to machine learning calculation.
 *   Key concept is 
 *      Given an arbitrary polynomial where supporte operator set is { +, -, * }, 
 *      partial derivatives wrt each variable can be calculated recursively using below equations.
 *      1) f(x) = g(x) + h(x)
 *         df/dx = dg/dx + dh/dx
 *      2) f(x) = g(x)*h(x)
 *         df/dx = dg/dx*h(x) + dh/dx * g(x) 
 *      3) f(x) = g(x) - h(x) = g(x) + h(x)*(-1)
 *         To get df/dx, apply equation 1 and 2. 
 *      Here we can see
 *         - for 2nd equation, sub-exressions should be evaluated with given instance values of variables. 
 *           We do this at the 1st tree traversal. 
 *         - partial derivative for a variable can be optained by summing up derivatives at leaf nodes for same variable. 
 *           derivative at a leaf node = multiplication of all added coefficients while traversing expression tree by DFS manner.
 *           ex) f = g*h, g= i*j, i=x, ...
 *               derivative for node i = h(x0)*j(x0)*1 at x=x0
 *         - So, partial derivatives for all variables can be optained at 2nd traversal of expression tree. 
 *             
 */  
   
typedef unsigned int uint;
typedef unsigned short ushort;
typedef unsigned char uchar;
typedef pair<int, int> DN; 

#define INV_NUM (uint)(0xffffffff)

typedef enum {
    NT_OP_PLU = 0x1, //NT_VAR << 1,
    NT_OP_MIN = NT_OP_PLU << 1,
    NT_OP_MUL = NT_OP_MIN << 1,
    NT_OP_DIV = NT_OP_MUL << 1,
    NT_OP_POW = NT_OP_DIV << 1,  // 
    
    NT_BR_OPEN = 0x100, // NT_OP_POW << 1, // open only 
    NT_BR_CLOSE = NT_BR_OPEN << 1,  // close only
    NT_BR = NT_BR_OPEN | NT_BR_CLOSE, // open + close
    NT_NUM = 0x10000,
    NT_VAR = NT_NUM << 1,
    NT_CONST = NT_VAR << 1  // natural constant such as 'e' 
} NODE_TYPE;    



typedef struct _link{
    struct _link *p; // parent
    struct _link *l;
    struct _link *r;
} LINK;
 
typedef struct {
    NODE_TYPE type; 
    LINK link;
    union {
        uint num; // numeric data
        char *str;
        uint op; 
        uchar constant;  
    } data;
    int64_t eval;
    uint varIdx;
} NODE;

#define pptr link.p
#define lptr link.l
#define rptr link.r




NODE *iter;
NODE *mexRoot = NULL; // math expression tree root node
NODE *curNode = NULL;
NODE *dexRoot = NULL; // diff tree root node

//#define CONTAINER(link) /
//    (NODE *)((char *)&link - &((NODE *)0)->link)
    

inline NODE *node_of(LINK *l)
{
    if (l) 
        return (NODE *)((char *)l - offsetof(NODE, link));    
    else 
        return NULL;
}

inline bool is_op_node(NODE *node)
{
    if (node->type & 0xff) return true;
    return false;
} 

inline bool is_num_var_node(NODE *node)
{
    if (node->type & 0xff0000) return true;
    return false;
} 

inline bool is_num_node(NODE *node)
{
    if (node->type & NT_NUM) return true;
    return false;
} 

inline bool is_var_node(NODE *node)
{
    if (node->type & NT_VAR) return true;
    return false;
} 

inline bool is_same_var(NODE *node, char *var)
{
    if (node->type & NT_VAR && 
        !strcmp(node->data.str, var)) return true;
    return false;
} 

inline bool is_const_node(NODE *node)
{
    if (node->type & NT_CONST) return true;
    return false;
} 

inline bool is_bracket_node(NODE *node)
{
    if (node->type & NT_BR) return true;
    return false;
} 

inline bool is_bracket_complete_node(NODE *node)
{
    if (node->type == NT_BR) return true;
    return false;
} 

inline bool is_bracket_open_node(NODE *node)
{
    if (node->type == NT_BR_OPEN) return true;
    return false;
} 

inline bool is_bracket_close_node(NODE *node)
{
    if (node->type == NT_BR_CLOSE) return true;
    return false;
} 

inline LINK *link_of(NODE *node)
{
    return &node->link;
}     

inline char op_of(NODE *node)
{
    return (char) (node->data.op & 0xff);
}     

inline NODE *parent_of(NODE *node)
{
    if (node->link.p)
        return node_of(node->link.p);
    else return NULL;
}     

void show_node(NODE *v)
{
    if (is_op_node(v) || is_bracket_node(v)) {
        printf("op_node : %c ", op_of(v));
    } else
    if (is_num_node(v)) {
        printf("num %u ", v->data.num);
    } else 
   // if (is_var_node(v)) 
    {
        printf("var %s ", v->data.str);
    }
    printf("\n");
}


inline void set_rchild(NODE *parent, NODE *node)
{
    if (parent != NULL) {
        LINK *pl = link_of(parent); 
        LINK *lc = parent->link.r;
        LINK *nl = link_of(node); 
        node->link.p = pl;
        parent->link.r = nl;
        node->link.l = lc;
        if (lc) {
            lc->p = nl;
        }
        //printf("set_rchild : parent ");
        //show_node(parent);
        //printf("set_rchild : child ");
        //show_node(node);
        
    }
}

inline void set_lchild(NODE *parent, NODE *node)
{
    if (parent && node) {
        parent->lptr = link_of(node);
        node->pptr = link_of(parent);    
    }
}

inline NODE *lchild_of(NODE *node) 
{
    return node_of(node->link.l);
}

inline NODE *rchild_of(NODE *node) 
{
    return node_of(node->link.r);
}

inline int cmp_prec(NODE *v1, NODE *v2)
{
    return v1->type - v2->type;
}

vector<pair<char *, int>> varMap;
vector<int64_t> diffResult;  

inline uint varmap_add_var(char *var)
{
    strdup(var);

    for (uint i=0; i<varMap.size(); i++) {
        if (strcmp(varMap[i].first, var)==0) {
            return i;
        }
    }
    //printf("varmap_add_var for %s\n", var);
    varMap.push_back(make_pair(strdup(var), 0));
    diffResult.push_back(0);
    return varMap.size()-1;
}

inline void varmap_set_var(char *var, int val)
{
    //printf("varmap_set_var for %s val %d\n", var, val);
    strdup(var);

    DN dn;
    for (uint i=0; i<varMap.size(); i++) {
        if (strcmp(varMap[i].first, var)==0) {
            varMap[i].second = val; return;
        }
    }
    printf("Input Error! - no such var %s\n", var);
}

inline int varmap_get_val(char *var)
{
    strdup(var);

    for (uint i=0; i<varMap.size(); i++) {
        if (strcmp(varMap[i].first, var)==0) {
            return varMap[i].second;
        }
    }
    printf("Input Error! - no such var %s\n", var);
    return 0;
}


// 
// simple DFA for parsing inbuf
// 
typedef enum {
    ISM_INIT,
    ISM_NUM,
    ISM_VAR,
    ISM_OP,
    ISM_STATE_CNT,
} ISM_STATE;

/*
typedef enum {
    _NUM,
    TOK_VAR,
    TOK_OP,
    TOK_,
}*/




int strToNum(char *s, int slen) 
{
    int n=0;
    for (int i=0; i< slen;i++) {
        n *=10 ; n+=s[i]-'0';
    }
    return n;
}

//ISM ismMat[ISM_STATE_CNT][];
NODE * new_node_from_token(ISM_STATE state, char *pTok, int tokLen)
{
    NODE *pNode = (NODE *) calloc(1, sizeof(NODE));
    if (!pNode) { return NULL; }
    
    switch(state) {
        case ISM_NUM:
            pNode->type = NT_NUM;
            pNode->data.num = strToNum(pTok, tokLen);
            break;
        case ISM_VAR:
            if (tokLen==1) {
                if (*pTok == 'e') { 
                    pNode->type = NT_CONST;
                    pNode->data.constant = *pTok;
                    break;
                }
            }
            pNode->type = NT_VAR;
            pNode->data.str = strndup(pTok, tokLen);

            pNode->varIdx = varmap_add_var(pNode->data.str);

            break;
        case ISM_OP:
            switch (*pTok) {
                case '+':
                    pNode->type = NT_OP_PLU;
                    break;
                case '-':
                    pNode->type = NT_OP_MIN;
                    break;
                case '*':
                    pNode->type = NT_OP_MUL;
                    break;
                case '/':
                    pNode->type = NT_OP_DIV;
                    break;
                case '^':
                    pNode->type = NT_OP_POW;
                    break;                    
                case '(':
                    pNode->type = NT_BR_OPEN;
                    break;
                case ')':
                    pNode->type = NT_BR_CLOSE;
                    break;
            }
            pNode->data.op = (uchar)(*pTok);
            break;
        default:
            break;
    }
    return pNode;
}


NODE * new_node()
{
    NODE *pNode = (NODE *) calloc(1, sizeof(NODE));
    if (!pNode) { return NULL; }
/*
	pNode->type = t; 
	if (is_op_node(pNode)) {
    	pNode->data.op = (uchar)op;
	}
	if (str) {
    	pNode->data.str = strdup(str);
	}
     */
	return pNode;
}

void free_node(NODE *v)
{
    if (is_var_node(v)) {
        free(v->data.str);
    }
    free(v);    
}

int free_tree(NODE *v)
{
    int rv = 0;
    
    NODE *lc, *rc;
    
    if ((lc = lchild_of(v)) && (parent_of(lc) == v))
        free_tree(lc);
    
    if ((rc = rchild_of(v)) && (parent_of(rc) == v))  
        free_tree(rc);
    
    free_node(v);
    return rv;
    
}

void add_node(NODE *v)
{
    //printf("%s:%d new v %p..\n", __FUNCTION__, __LINE__, v);
    
    if (mexRoot==NULL) {
        curNode = mexRoot = v;
        return;
    }
    
    
    if (is_num_var_node(v)) {
        if (is_op_node(curNode) || is_bracket_open_node(curNode)) {
            // v should be number or var node
            // r link should be NULL
            set_rchild(curNode, v);
        } else  {
            printf("%s:%d error case..\n", __FUNCTION__, __LINE__);
            return;  
        }
    } else 
    //if (is_op_node(v)) 
    {
        NODE *cn, *prn; 
        cn = curNode;
        if (is_num_var_node(cn)||is_bracket_complete_node(cn)) { 
            prn = cn; 
            cn = parent_of(cn);
        //    printf("%s:%d parent %p\n", __FUNCTION__, __LINE__, cn);
        //    if (cn) show_node(cn); 
        }

        for (; cn ; prn = cn, cn = parent_of(cn) ) {
        //    printf("%s:%d cn %p..\n", __FUNCTION__, __LINE__, cn);
        //    show_node(cn); 
            if ( is_bracket_open_node(cn) || 
                (!is_bracket_close_node(v) && (cmp_prec(cn, v)<0))) { 
                break; 
            }
        }
        if (is_bracket_close_node(v)) { 
            if (is_bracket_open_node(cn)) {
                cn->type = NT_BR;
                curNode = cn;
                free_node(v);
                return ;
            } else {
                printf("%s:%d error case..\n", __FUNCTION__, __LINE__);
                return;      
            }
        }
            
        if (cn) {
            set_rchild(cn, v);
        } else {
            set_lchild(v, prn);
            if (mexRoot == prn) { 
                mexRoot = v;
            //    printf("%s:%d mexRoot -> %p..\n", __FUNCTION__, __LINE__, v);
            } else  {
                printf("%s:%d error case..\n", __FUNCTION__, __LINE__);
                return;  
            }
        }
    }

    curNode = v;
}



inline bool isDigit(char c)
{
    return c>='0' && c<='9';
}
 
inline bool isAlpha(char c)
{
    return (c>='a' && c<='z') ||
        ( c>='A' && c<='Z' );
} 

char inbuf[1024] = { 0 };
const size_t inbufSize = 1024;
 
int build_math_exp_tree()
{
    ISM_STATE state = ISM_INIT ;
    int bufLen = strlen(inbuf);

    char *pToken = NULL;
    NODE *node;
    mexRoot = NULL;
    
    for (int i=0; i<bufLen; i++) {
        ISM_STATE nextState = ISM_INIT;
        char in = inbuf[i];
        if (in == ' ') {
            if (state != ISM_INIT ) {
                if ((node = new_node_from_token(state, pToken, &inbuf[i]-pToken))) {
                    add_node(node);
                }
            }
            
            state = nextState;
            printf("input %c : state -> %d\n", in, state );
            continue;
        }
        if (isDigit(in)) {
            if (state == ISM_NUM) {
                continue;
            } 
            nextState = ISM_NUM;
        } else 
        if (isAlpha(in)) {
            if (state == ISM_VAR) {
                continue;
            } 
            nextState = ISM_VAR;
        } else 
            nextState = ISM_OP;
            
        if (state != ISM_INIT ) {
            if ((node = new_node_from_token(state, pToken, &inbuf[i]-pToken))) {
                add_node(node);
            }
        }
    
        pToken = &inbuf[i];
        state = nextState;
    //    printf("input %c : state -> %d\n", in, state );
    }

    if (state != ISM_INIT ) {
        if ((node = new_node_from_token(state, pToken, &inbuf[bufLen]-pToken))) {
            add_node(node);
        }
    }
    return 0;
} 


int traverse_tree(NODE *v)
{
    int rv = 0;
    
    if (is_op_node(v)|| is_bracket_complete_node(v)) {
        NODE *lc, *rc; 
        if (!is_bracket_node(v))   
            printf("[ ");
        else 
            printf("( ");
        if ((lc = lchild_of(v)))
            traverse_tree(lc);
            
        if (!is_bracket_node(v))   
            printf("%c ", op_of(v));
        
        if ((rc = rchild_of(v)))
            traverse_tree(rc);
        if (!is_bracket_node(v))   
            printf("] ");
        else 
            printf(") ");
    } else {
        if (is_num_node(v)) {
            printf("%u ", v->data.num);
        } else 
        if (is_const_node(v)) {
            printf("%c ", v->data.constant);   
        } else
        {
            printf("%s ", v->data.str);
        }
    }
    
    return rv;
    
}
 
void verify_math_exp_tree()
{
    NODE *v = mexRoot;
    if (v) {
        traverse_tree(v);
        printf("\n");
    }
} 



int eval_math_exp_tree(NODE *v)
{
    int rv = 0;
    
    if (is_op_node(v)|| is_bracket_complete_node(v)) {
        NODE *lc, *rc; 

        if ((lc = lchild_of(v)))
            eval_math_exp_tree(lc);
        if ((rc = rchild_of(v)))
            eval_math_exp_tree(rc);
        
        switch (op_of(v)) {
            case '+':
                v->eval = lc->eval + rc->eval;
                break;
            case '-':
                v->eval = lc->eval - rc->eval;
                break;
             
            case '*':  // (f * g)' = f'*g + f*g'
                v->eval = lc->eval * rc->eval;
                break;
            case '^' : // (x^c)' = c*(x^(c-1))
                if (!lc || !rc ) {
                        printf("syntax error!");  rv = -1 ;
                }

                if (is_var_node(lc) && is_num_node(rc)) {
                    v->eval = pow(lc->eval, rc->eval);
                //    printf("v->eval = %ld\n", v->eval);
                } else {
                    printf("syntax error!"); rv = -1 ;
                }
                break;
            case '(':  
                v->eval = rc->eval;
                break;
        }            
    } else {
        if (is_var_node(v)) {
            v->eval = varMap[v->varIdx].second;
         //   printf("v->eval = %ld\n", v->eval);
        } else {
            v->eval = v->data.num;
          //  printf("v->eval = %ld\n", v->eval);
        } 
    }
    
    return rv;
    
}
 
void autodiff_rvs(NODE *node, int64_t coeff)
{
    //printf("[autodiff_rvs] coeff = %ld\n", coeff);
    if (is_op_node(node)) {

        NODE *lc = lchild_of(node);
        NODE *rc = rchild_of(node);

        //printf("op_of(node) %c\n", op_of(node));
        switch (op_of(node)) {
            case '+':
                autodiff_rvs(lc, coeff);
                autodiff_rvs(rc, coeff);
                break;
            case '-':
                autodiff_rvs(lc, coeff);
                autodiff_rvs(rc, coeff*(-1));
                break;
             
            case '*':  // (f * g)' = f'*g + f*g'
                autodiff_rvs(lc, coeff*rc->eval);
                autodiff_rvs(rc, coeff*lc->eval);
                break;
            case '^' : // (x^c)' = c*(x^(c-1))
                if (!lc || !rc ) {
                        printf("syntax error!"); return ;
                }

                if (is_var_node(lc) && is_num_node(rc)) {
                    diffResult[lc->varIdx] += coeff * pow(varmap_get_val(lc->data.str), rc->data.num-1) * rc->data.num;
                }
                break;
        }
    } else 
    if (is_var_node(node)) {
        diffResult[node->varIdx] += coeff;
    } else
    if (is_bracket_complete_node(node)) {
        NODE *rc = rchild_of(node);
        if (rc) 
            autodiff_rvs(rc, coeff);
    } else 
    if (!is_num_node(node)) {
        printf("%s:%d error case..\n", __FUNCTION__, __LINE__);
    }
}

int input_var()
{
    for (uint i=0; i<varMap.size(); i++)
    {
        char buf[256]={ 0 };
        cin.getline(buf, sizeof buf);
        int off=0, varoff;
        while (buf[off] && !isAlpha(buf[off])) { off++; }
        if (buf[off]=='\0') {
            printf("Input Error!\n"); return -1;
        }
        varoff = off;
        while (isAlpha(buf[off])) { off++; }
        if (buf[off]=='\0') {
            printf("Input Error!\n"); return -1;
        }
        // ' ' or '='
        buf[off++] = '\0';

        while (buf[off] && !isDigit(buf[off])) { off++; }
        if (buf[off]=='\0') {
            printf("Input Error!\n"); return -1;
        }
        // init value
        int val = 0;
        while (buf[off] && isDigit(buf[off])) { 
            val = val * 10 + buf[off]-'0';
            off++; 
        }
        varmap_set_var(&buf[varoff], val);
    }
    return 0;
} 


int main(int argc, char **argv)
{
    freopen("input.txt", "r", stdin);
    for (uint i=0; i<sizeof(inbuf); i++)
    {
        int c = getc(stdin);
        if (c == '\n') {
            inbuf[i] = '\0'; break;
        } 
        inbuf[i] = (char )c ;    
    }
    cout << inbuf << endl;

    

    // 1. build math-exp tree (MEX-TREE)
    build_math_exp_tree();

    // 2. veritify MEX-TREE
    verify_math_exp_tree();

    // 3. do differentiation
    //    Now, single var derivatives

    if (!mexRoot) return 0;

    input_var();
   
    eval_math_exp_tree(mexRoot);
    
    autodiff_rvs(mexRoot, 1);
    
    
    printf("Derivative at ( ");
    for (uint i=0; i< varMap.size(); i++) {
        if (i) printf(", ");
        printf("%s=%d", varMap[i].first, varMap[i].second);
    }
    printf(" ) ==> \n");
    for (uint i=0; i< varMap.size(); i++) {
        printf("[%s] %ld\n", varMap[i].first, diffResult[i]);
    }

    free_tree(mexRoot);
    return 0;
}
