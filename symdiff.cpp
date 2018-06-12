#include <bits/stdc++.h>
#include <stdio.h>
#include <stdlib.h>

using namespace std;

/* Program Description 
 *   1. binary tree is built by parsing mathematical expression 
 *   2. Symbolic Differentication (SD) is performed on the built tree from step 1. 
 */     
typedef unsigned int uint;
typedef unsigned short ushort;
typedef unsigned char uchar;

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
        return (NODE *)((void *)l - offsetof(NODE, link));    
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
        printf("set_rchild : parent ");
        show_node(parent);
        printf("set_rchild : child ");
        show_node(node);
        
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


void node_free(NODE *v)
{
    
  
    
}



void add_node(NODE *v)
{
    printf("%s:%d new v %p..\n", __FUNCTION__, __LINE__, v);
    
    if (mexRoot==NULL) {
        curNode = mexRoot = v;
        return;
    }
    
    LINK *vl = link_of(v);
    
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
            printf("%s:%d parent %p ", __FUNCTION__, __LINE__, cn);
            if (cn) show_node(cn); 
        }

        for (; cn ; prn = cn, cn = parent_of(cn) ) {
            printf("%s:%d cn %p..\n", __FUNCTION__, __LINE__, cn);
            show_node(cn); 
            if ( is_bracket_open_node(cn) || 
                (!is_bracket_close_node(v) && (cmp_prec(cn, v)<0))) { 
                break; 
            }
        }
        if (is_bracket_close_node(v)) { 
            if (is_bracket_open_node(cn)) {
                cn->type = NT_BR;
                curNode = cn;
                node_free(v);
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
                printf("%s:%d mexRoot -> %p..\n", __FUNCTION__, __LINE__, v);
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
    uint tokenNum = INV_NUM;
    char *pToken = NULL;
    int tokenLen; NODE *node;
    mexRoot = NULL;
    
    for (int i=0; i<bufLen; i++) {
        ISM_STATE nextState = ISM_INIT;
        bool token = false;
        char in = inbuf[i];
        if (in == ' ') {
            if (state != ISM_INIT ) {
                if (node = new_node_from_token(state, pToken, &inbuf[i]-pToken)) {
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
            if (node = new_node_from_token(state, pToken, &inbuf[i]-pToken)) {
                add_node(node);
            }
        }
    
        pToken = &inbuf[i];
        state = nextState;
        printf("input %c : state -> %d\n", in, state );
    }
  
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
        if (lc = lchild_of(v))
            traverse_tree(lc);
            
        if (!is_bracket_node(v))   
            printf("%c ", op_of(v));
        
        if (rc = rchild_of(v))
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


void verify_diff_exp_tree()
{
    NODE *v = dexRoot;
    if (v) {
        traverse_tree(v);
        printf("\n");
    }
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


void sym_diff(NODE *node, NODE *dnode, char *dvar)
{
    
    if (is_op_node(node)) {
        NODE *lc = lchild_of(node);
        NODE *rc = rchild_of(node);
        switch (op_of(node)) {
            case '+':
            case '-':
                {
                    dnode->type = node->type;
                    dnode->data.op = node->data.op;
                    
                    NODE *ld =new_node();
                    dnode->lptr = link_of(ld);
                    sym_diff(lc, ld, dvar);
                    
                    NODE *rd =new_node();
                    dnode->rptr = link_of(rd);
                    sym_diff(rc, rd, dvar);
                }
                
                break;
             
            case '*':  // (f * g)' = f'*g + f*g'
                {
                    dnode->type = NT_OP_PLU;
                    
                    dnode->data.op = '+';
                    
                    NODE *ld =new_node();
                    ld->type = NT_OP_MUL;
                    ld->data.op = '*';
                    dnode->lptr = link_of(ld);
                    
                    NODE *rd =new_node();
                    rd->type = NT_OP_MUL;
                    rd->data.op = '*';
                    dnode->rptr = link_of(rd);
                    
                    NODE *gld =new_node();
                    ld->lptr = link_of(gld);
                    sym_diff(lc, gld, dvar);
                    
                    ld->rptr = link_of(rc);
                    NODE *grd =new_node();
                    rd->lptr = link_of(lc);
                    rd->rptr = link_of(grd);
                    sym_diff(rc, grd, dvar);
                }
            
                break;
            case '^' : // (x^c)' = c*(x^(c-1))
                if (!lc || !rc ) {
                        printf("syntax error!"); return ;
                }

                if (is_var_node(lc) && is_num_node(rc)) {
                    if (is_same_var(lc, dvar)) {
                
                        dnode->type = NT_OP_MUL;
                        
                        dnode->data.op = '*';
                        
                        dnode->lptr = link_of(rc);
                
                        
                        NODE *rd =new_node();
                        rd->type = NT_OP_POW;
                        rd->data.op = '^';
                        dnode->rptr = link_of(rd);
                    // rd->lptr = link_of(lc);
                        
                        NODE *gld =new_node();
                        rd->lptr = link_of(lc);
                        
                        NODE *grd =new_node();
                        grd->type = NT_NUM;
                        grd->data.num = rc->data.num-1;
                        rd->rptr = link_of(grd);
                    } else {
                        dnode->type = NT_NUM;
                        dnode->data.num = 0;                    
                    }
                }
                else 
                if (is_const_node(lc) && is_var_node(rc)){
                    // ( e^x )' = e^x
                    if (is_same_var(rc, dvar)) {
                        dnode->type = NT_OP_POW;
                        dnode->data.op = '^';
                        dnode->lptr = link_of(lc);
                        dnode->rptr = link_of(rc);
                    } else {
                        dnode->type = NT_NUM;
                        dnode->data.num = 0;                    
                    }
                }
                else
                if (!is_var_node(lc) && !is_var_node(rc)) {
                    dnode->type = NT_NUM;
                    dnode->data.num = 0;                    
                } 
                else {
                    dnode->type = NT_NUM;
                    dnode->data.num = 0;         
                }
                
                break;
        }
    } else 
    if (is_var_node(node)) {
        if (is_same_var(node, dvar)) {
            dnode->type = NT_NUM;
            dnode->data.num = 1;
        } else {
            dnode->type = NT_NUM;
            dnode->data.num = 0;                    
        }
    } else
    if (is_num_node(node) || is_const_node(node)) {
        dnode->type = NT_NUM;
        dnode->data.num = 0;    
    } else 
    if (is_bracket_complete_node(node)) {
       
        NODE *rc = rchild_of(node);
        dnode->type = NT_BR;
        NODE *rd =new_node();
        dnode->rptr = link_of(rd);
        sym_diff(rc, rd, dvar);
    } else {
        printf("%s:%d error case..\n", __FUNCTION__, __LINE__);
    }
    
}

 
int main(int argc, char **argv)
{
    freopen("input.txt", "r", stdin);
    for (int i=0; i<sizeof(inbuf); i++)
    {
        int c = getc(stdin);
        if (c == EOF) {
            inbuf[i] = '\0'; break;
        } 
        inbuf[i] = (char )c ;    
    }

    cin >> inbuf;
    
    cout << inbuf << endl;
        cin >> inbuf;
    cout << inbuf << endl;

    // 1. build math-exp tree (MEX-TREE)
    build_math_exp_tree();
    // 2. veritify MEX-TREE
    verify_math_exp_tree();

    // 3. do differentiation
    //    Now, single var derivatives

    char *dvar = "x";
    if (!mexRoot) return 0;
    
    dexRoot = new_node();
    sym_diff(mexRoot, dexRoot, dvar);
    verify_diff_exp_tree();

    free_tree(dexRoot);
    free_tree(mexRoot);
    
	return 0;
}
