#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#define MAX_LENGTH 200
typedef enum {
	ASSIGN, ADD, SUB, MUL, DIV, REM, PREINC, PREDEC, POSTINC, POSTDEC, IDENTIFIER, CONSTANT, LPAR, RPAR, PLUS, MINUS, END
} Kind;
typedef enum {
	STMT, EXPR, ASSIGN_EXPR, ADD_EXPR, MUL_EXPR, UNARY_EXPR, POSTFIX_EXPR, PRI_EXPR
} GrammarState;
typedef struct TokenUnit {
	Kind kind;
	int val; // record the integer value or variable name
	struct TokenUnit *next;
} Token;
typedef struct ASTUnit {
	Kind kind;
	int val; // record the integer value or variable name
	struct ASTUnit *lhs, *mid, *rhs;
} AST;
int err_num=0;
const int alpha[3]={0,4,8};
/// utility interfaces

// err marco should be used when a expression error occurs.
#define err(x) {\
	puts("Compile Error!\n");\
	if(DEBUG) {\
		fprintf(stderr, "Error at line: %d\n", __LINE__);\
		fprintf(stderr, "Error message: %s\n", x);\
	}\
	exit(0);\
}
// You may set DEBUG=1 to debug. Remember setting back to 0 before submit.
#define DEBUG 0
// Split the input char array into token linked list.
Token *lexer(const char *in);
// Create a new token.
Token *new_token(Kind kind, int val);
// Translate a token linked list into array, return its length.
size_t token_list_to_arr(Token **head);
// Parse the token array. Return the constructed AST.
AST *parser(Token *arr, size_t len);
// Parse the token array. Return the constructed AST.
AST *parse(Token *arr, int l, int r, GrammarState S);
// Create a new AST node.
AST *new_AST(Kind kind, int val);
// Find the location of next token that fits the condition(cond). Return -1 if not found. Search direction from start to end.
int findNextSection(Token *arr, int start, int end, int (*cond)(Kind));
// Return 1 if kind is ASSIGN.
int condASSIGN(Kind kind);
// Return 1 if kind is ADD or SUB.
int condADD(Kind kind);
// Return 1 if kind is MUL, DIV, or REM.
int condMUL(Kind kind);
// Return 1 if kind is RPAR.
int condRPAR(Kind kind);
// Check if the AST is semantically right. This function will call err() automatically if check failed.
void semantic_check(AST *now);

void change(AST** root);

void check(AST** root);
// Generate ASM code.
void codegen(AST *root);
// Free the whole AST.
void freeAST(AST *now);

/// debug interfaces

// Print token array.
//void token_print(Token *in, size_t len);
void token_print(Token *in);
// Print AST tree.
void AST_print(AST *head);

char input[MAX_LENGTH];

int num=0;

int f=0;

int main() {
	AST *ast_root[15];
	int count=0;
	while (fgets(input, MAX_LENGTH, stdin) != NULL) {
		num=0;
		Token *content = lexer(input);
		size_t len = token_list_to_arr(&content);
		if (len == 0) continue;
		ast_root[count] = parser(content, len);
		if(ast_root[count] == NULL)
        {
            count++;
            continue;
        }
		semantic_check(ast_root[count]);
		//count number directly, delete LPAR
        if(ast_root[count]->kind == ASSIGN) change(&(ast_root[count]->rhs));
        else if(ast_root[count]->kind == LPAR) change(&(ast_root[count]));
		free(content);
		count++;
	}
	for(int i=0;i<count;i++)
    {
        if(ast_root[i]==NULL){}
        else if(ast_root[i]->kind != ASSIGN) //see pre/dec do something
            check(&(ast_root[i]));
        else
            codegen(ast_root[i]);
        freeAST(ast_root[i]);
    }
	return 0;
}

Token *lexer(const char *in) {
	Token *head = NULL;
	Token **now = &head;
	for (int i = 0; in[i]; i++) {
		if (isspace(in[i])) // ignore space characters
			continue;
		else if (isdigit(in[i])) { // number
			(*now) = new_token(CONSTANT, atoi(in + i));
			while (in[i+1] && isdigit(in[i+1])) i++;
		}
		else if ('x' <= in[i] && in[i] <= 'z') // variable
			(*now) = new_token(IDENTIFIER, in[i]);
		else switch (in[i]) {
			case '=':
				(*now) = new_token(ASSIGN, 0);
				break;
			case '+':
				if (in[i+1] && in[i+1] == '+') {
					i++;
					// In lexer scope, all "++" will be labeled as PREINC.
					(*now) = new_token(PREINC, 0);
				}
				// In lexer scope, all single "+" will be labeled as PLUS.
				else (*now) = new_token(PLUS, 0);
				break;
			case '-':
				if (in[i+1] && in[i+1] == '-') {
					i++;
					// In lexer scope, all "--" will be labeled as PREDEC.
					(*now) = new_token(PREDEC, 0);
				}
				// In lexer scope, all single "-" will be labeled as MINUS.
				else (*now) = new_token(MINUS, 0);
				break;
			case '*':
				(*now) = new_token(MUL, 0);
				break;
			case '/':
				(*now) = new_token(DIV, 0);
				break;
			case '%':
				(*now) = new_token(REM, 0);
				break;
			case '(':
				(*now) = new_token(LPAR, 0);
				break;
			case ')':
				(*now) = new_token(RPAR, 0);
				break;
			case ';':
				(*now) = new_token(END, 0);
				break;
			default:
				err("Unexpected character.");
		}
		now = &((*now)->next);
	}
	return head;
}

Token *new_token(Kind kind, int val) {
	Token *res = (Token*)malloc(sizeof(Token));
	res->kind = kind;
	res->val = val;
	res->next = NULL;
	return res;
}

size_t token_list_to_arr(Token **head) {
	size_t res;
	Token *now = (*head), *del;
	for (res = 0; now != NULL; res++) now = now->next;
	now = (*head);
	if (res != 0) (*head) = (Token*)malloc(sizeof(Token) * res);
	for (int i = 0; i < res; i++) {
		(*head)[i] = (*now);
		del = now;
		now = now->next;
		free(del);
	}
	return res;
}

void change(AST** root)
{
    switch((*root)->kind)
    {
        AST *t;
        AST *tmp1,*tmp2;
        int tmp;
        case ASSIGN:
            change(&((*root)->lhs));
            change(&((*root)->rhs));
            break;
        case ADD:
            if((*root)->lhs->kind != CONSTANT) change(&((*root)->lhs));
            if((*root)->rhs->kind != CONSTANT) change(&((*root)->rhs));
            if((*root)->lhs->kind!=CONSTANT && (*root)->lhs->kind!=LPAR) return;
            if((*root)->rhs->kind!=CONSTANT && (*root)->rhs->kind!=LPAR) return;
            if((*root)->lhs->kind == CONSTANT&&(*root)->rhs->kind == CONSTANT)
            {
                tmp1 = (*root)->lhs;
                tmp2 = (*root)->rhs;
                tmp = tmp1->val + tmp2->val;
                (*root)->kind = CONSTANT;
                (*root)->val = tmp;
                free(tmp1),free(tmp2),(*root)->lhs=NULL,(*root)->rhs=NULL;
            }

            break;
        case SUB:
            if((*root)->lhs->kind != CONSTANT) change(&((*root)->lhs));
            if((*root)->rhs->kind != CONSTANT) change(&((*root)->rhs));
            if((*root)->lhs->kind == CONSTANT&&(*root)->rhs->kind == CONSTANT)
            {
                tmp1 = (*root)->lhs;
                tmp2 = (*root)->rhs;
                tmp = tmp1->val - tmp2->val;
                (*root)->kind = CONSTANT;
                (*root)->val = tmp;
                free(tmp1),free(tmp2),(*root)->lhs=NULL,(*root)->rhs=NULL;
            }
            break;
        case MUL:
            if((*root)->lhs->kind != CONSTANT) change(&((*root)->lhs));
            if((*root)->rhs->kind != CONSTANT) change(&((*root)->rhs));
            if((*root)->lhs->kind!=CONSTANT && (*root)->lhs->kind!=LPAR) return;
            if((*root)->rhs->kind!=CONSTANT && (*root)->rhs->kind!=LPAR) return;
            if((*root)->lhs->kind == CONSTANT&&(*root)->rhs->kind == CONSTANT)
            {
                tmp1 = (*root)->lhs;
                tmp2 = (*root)->rhs;
                tmp = tmp1->val * tmp2->val;
                (*root)->kind = CONSTANT;
                (*root)->val = tmp;
                free(tmp1),free(tmp2),(*root)->lhs=NULL,(*root)->rhs=NULL;
            }

            break;
        case DIV:
            if((*root)->lhs->kind != CONSTANT) change(&((*root)->lhs));
            if((*root)->rhs->kind != CONSTANT) change(&((*root)->rhs));
            if((*root)->lhs->kind!=CONSTANT && (*root)->lhs->kind!=LPAR) return;
            if((*root)->rhs->kind!=CONSTANT && (*root)->rhs->kind!=LPAR) return;
            if((*root)->lhs->kind == CONSTANT&&(*root)->rhs->kind == CONSTANT)
            {
                tmp1 = (*root)->lhs;
                tmp2 = (*root)->rhs;
                tmp = tmp1->val / tmp2->val;
                (*root)->kind = CONSTANT;
                (*root)->val = tmp;
                free(tmp1),free(tmp2),(*root)->lhs=NULL,(*root)->rhs=NULL;
            }

            break;
        case REM:
            if((*root)->lhs->kind != CONSTANT) change(&((*root)->lhs));
            if((*root)->rhs->kind != CONSTANT) change(&((*root)->rhs));
            if((*root)->lhs->kind!=CONSTANT && (*root)->lhs->kind!=LPAR) return;
            if((*root)->rhs->kind!=CONSTANT && (*root)->rhs->kind!=LPAR) return;
            if((*root)->lhs->kind == CONSTANT&&(*root)->rhs->kind == CONSTANT)
            {
                tmp1 = (*root)->lhs;
                tmp2 = (*root)->rhs;
                tmp = tmp1->val % tmp2->val;
                (*root)->kind = CONSTANT;
                (*root)->val = tmp;
                free(tmp1),free(tmp2),(*root)->lhs=NULL,(*root)->rhs=NULL;
            }

            break;
        case LPAR:
            t = *root;
            while(t->mid!=NULL&&t->kind!=POSTDEC&&t->kind!=POSTINC&&
                  t->kind!=PREDEC&&t->kind!=PREINC&&
                  t->kind!=CONSTANT&&t->kind!=IDENTIFIER)
            {
                AST* tt = (*root)->mid;
                t->kind = tt->kind;
                t->lhs = tt->lhs;
                t->rhs = tt->rhs;
                t->mid = tt->mid;
                t->val = tt->val;
                free(tt);
            }
            change(&t);
            break;
        case PLUS:
            t = *root;
            if(t->mid->kind == PLUS)
            {
                AST* tt = (*root)->mid;
                t->kind = PLUS;
                t->lhs = tt->lhs;
                t->rhs = tt->rhs;
                t->mid = tt->mid;
                t->val = tt->val;
                free(tt);
            }
            else if(t->mid->kind == MINUS)
            {
                AST* tt = (*root)->mid;
                t->kind = MINUS;
                t->lhs = tt->lhs;
                t->rhs = tt->rhs;
                t->mid = tt->mid;
                t->val = tt->val;
                free(tt);
            }
            else if(t->mid->kind == CONSTANT || t->mid->kind == IDENTIFIER ||
                    t->mid->kind == PREINC || t->mid->kind == PREDEC ||
                    t->mid->kind == POSTDEC || t->mid->kind == POSTINC || t->mid->kind == ASSIGN)
            {
                AST* tt = (*root)->mid;
                t->kind = tt->kind;
                t->lhs = tt->lhs;
                t->rhs = tt->rhs;
                t->mid = tt->mid;
                t->val = tt->val;
                free(tt);
            }
            change(&t);
            break;
        case MINUS:
            t = *root;
            if(t->mid->kind == PLUS)
            {
                AST* tt = (*root)->mid;
                t->kind = MINUS;
                t->lhs = tt->lhs;
                t->rhs = tt->rhs;
                t->mid = tt->mid;
                t->val = tt->val;
                free(tt);
            }
            else if(t->mid->kind == MINUS)
            {
                AST* tt = (*root)->mid;
                t->kind = PLUS;
                t->lhs = tt->lhs;
                t->rhs = tt->rhs;
                t->mid = tt->mid;
                t->val = tt->val;
                free(tt);
            }
            else if(t->mid->kind == PREINC || t->mid->kind == PREDEC ||
                    t->mid->kind == POSTDEC || t->mid->kind == POSTINC||
                    t->mid->kind == CONSTANT || t->mid->kind == IDENTIFIER)
                    break;
            else if(t->mid->kind == LPAR)
            {
                change(&(t->mid));
                return;
            }
            change(&t);
            break;
        default: break;
    }
}

void check(AST** root)
{
    switch((*root)->kind)
    {
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case REM:
            check(&((*root)->lhs));
            check(&((*root)->rhs));
            break;
        case PLUS:
        case MINUS:
            check(&((*root)->mid));
            break;

        case LPAR:
            check(&((*root)->mid));
            break;
        case PREINC:
            printf("load r0 [%d]\n",alpha[(*root)->mid->val-'x']);
            printf("add r0 r0 1\n");
            printf("store [%d] r0\n",alpha[(*root)->mid->val-'x']);
            break;
        case POSTINC:
            printf("load r0 [%d]\n",alpha[(*root)->mid->val-'x']);
            printf("add r0 r0 1\n");
            printf("store [%d] r0\n",alpha[(*root)->mid->val-'x']);
            break;
        case PREDEC:
            printf("load r0 [%d]\n",alpha[(*root)->mid->val-'x']);
            printf("sub r0 r0 1\n");
            printf("store [%d] r0\n",alpha[(*root)->mid->val-'x']);
            break;
        case POSTDEC:
            printf("load r0 [%d]\n",alpha[(*root)->mid->kind-'x']);
            printf("sub r0 r0 1\n");
            printf("store [%d] r0\n",alpha[(*root)->mid->kind-'x']);
            break;
        default: break;
    }
}


AST *parser(Token *arr, size_t len) {
    //original they are all plus and minus
	for (int i = 1; i < len; i++) {
		// correctly identify "ADD" and "SUB"
		if (arr[i].kind == PLUS || arr[i].kind == MINUS) {
			switch (arr[i - 1].kind) {

				case PREINC:
				case PREDEC:
				case IDENTIFIER:
				case CONSTANT:
				case RPAR:
					arr[i].kind = arr[i].kind - PLUS + ADD; //plus->add
				default: break;
			}
		}
	}
	return parse(arr, 0, len - 1, STMT);
}

//(((((x))))=((y%z)));

AST *parse(Token *arr, int l, int r, GrammarState S) {
	if(err_num == 1) return NULL;
	AST *now = NULL;
	if (l > r)
        err("Unexpected parsing range.");
	int nxt;
	switch (S) {
		case STMT:
			if (l == r && arr[l].kind == END)
				return NULL;
			else if (arr[r].kind == END)
				return parse(arr, l, r - 1, EXPR);
			else err("Expected \';\' at the end of line.");
		case EXPR:
			return parse(arr, l, r, ASSIGN_EXPR);
		case ASSIGN_EXPR:
			if ((nxt = findNextSection(arr, l, r, condASSIGN)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, UNARY_EXPR);
				now->rhs = parse(arr, nxt + 1, r, ASSIGN_EXPR);
				return now;
			}
			return parse(arr, l, r, ADD_EXPR);
		case ADD_EXPR:
			if((nxt = findNextSection(arr, r, l, condADD)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, ADD_EXPR);
				now->rhs = parse(arr, nxt + 1, r, MUL_EXPR);
				return now;
			}
			return parse(arr, l, r, MUL_EXPR);
		case MUL_EXPR:
            if((nxt = findNextSection(arr, r, l, condMUL)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, MUL_EXPR);
				now->rhs = parse(arr, nxt + 1, r, UNARY_EXPR);
				return now;
			}
			return parse(arr, l, r, UNARY_EXPR);
		case UNARY_EXPR:
			if(arr[l].kind == PREINC || arr[l].kind == PREDEC)
            {
                now = new_AST(arr[l].kind, 0);
				now->mid = parse(arr, l+1, r, UNARY_EXPR);
				return now;
            }
            if(arr[l].kind == PLUS || arr[l].kind == MINUS)
            {
                now = new_AST(arr[l].kind, 0);
				now->mid = parse(arr, l+1, r, UNARY_EXPR);
				return now;
            }
            return parse(arr, l, r, POSTFIX_EXPR);
		case POSTFIX_EXPR:
			if (arr[r].kind == PREINC || arr[r].kind == PREDEC) {
				// translate "PREINC", "PREDEC" into "POSTINC", "POSTDEC"
				now = new_AST(arr[r].kind - PREINC + POSTINC, 0);
				now->mid = parse(arr, l, r - 1, POSTFIX_EXPR);
				return now;
			}
			return parse(arr, l, r, PRI_EXPR);
		case PRI_EXPR:
			if (findNextSection(arr, l, r, condRPAR) == r) {
				now = new_AST(LPAR, 0);
				now->mid = parse(arr, l + 1, r - 1, EXPR);
				return now;
			}
			if (l == r) {
				if (arr[l].kind == IDENTIFIER || arr[l].kind == CONSTANT)
					return new_AST(arr[l].kind, arr[l].val);
				err("Unexpected token during parsing.");
			}
			err("No token left for parsing.");
		default:
			err("Unexpected grammar state.");
	}
}

AST *new_AST(Kind kind, int val) {
	AST *res = (AST*)malloc(sizeof(AST));
	res->kind = kind;
	res->val = val;
	res->lhs = res->mid = res->rhs = NULL;
	return res;
}

int findNextSection(Token *arr, int start, int end, int (*cond)(Kind)) {
	int par = 0;
	int d = (start < end) ? 1 : -1;
	for (int i = start; (start < end) ? (i <= end) : (i >= end); i += d) {
		if (arr[i].kind == LPAR) par++;
		if (arr[i].kind == RPAR) par--;
		if (par == 0 && cond(arr[i].kind) == 1) return i;
	}
	return -1;
}

int condASSIGN(Kind kind) {
	return kind == ASSIGN;
}

int condADD(Kind kind) {
	return kind == ADD || kind == SUB;
}

int condMUL(Kind kind) {
	return kind == MUL || kind == DIV || kind == REM;
}

int condRPAR(Kind kind) {
	return kind == RPAR;
}

void semantic_check(AST *now) {
	if (now == NULL) return;
	if (now->kind == ASSIGN) {
		AST *tmp = now->lhs;
		while (tmp->kind == LPAR) tmp = tmp->mid;
		if (tmp->kind != IDENTIFIER)
			err("Lvalue is required as left operand of assignment.");
        semantic_check(now->rhs);//****
	}
	if(now->kind!=PREINC||now->kind!=PREDEC||now->kind!=POSTINC||now->kind!=POSTDEC)
    {
        semantic_check(now->lhs);
        semantic_check(now->mid);
        semantic_check(now->rhs);
    }
    if(now->kind==PREINC||now->kind==PREDEC||now->kind==POSTINC||now->kind==POSTDEC)
    {
        AST *tmp = now->mid;
        while (tmp->kind == LPAR) tmp = tmp->mid;
        if (tmp->kind != IDENTIFIER) err("qqq");
    }
}

void codegen(AST *root) {
    if(!root) return;
    switch(root->kind)
    {
        case ASSIGN:
            codegen(root->rhs);
            AST* tmp = root->lhs;
            while(tmp->kind == LPAR) tmp = tmp->mid;
            printf("store [%d] r%d\n",alpha[tmp->val - 'x'],num-1);
            break;
        case ADD:
            codegen(root->lhs);
            codegen(root->rhs);
            printf("add r%d r%d r%d\n",num-2,num-2,num-1);
            num--;
            break;
        case SUB:
            codegen(root->lhs);
            codegen(root->rhs);
            printf("sub r%d r%d r%d\n",num-2,num-2,num-1);
            num--;
            break;
        case MUL:
            codegen(root->lhs);
            codegen(root->rhs);
            printf("mul r%d r%d r%d\n",num-2,num-2,num-1);
            num--;
            break;
        case DIV:
            codegen(root->lhs);
            codegen(root->rhs);
            printf("div r%d r%d r%d\n",num-2,num-2,num-1);
            num--;
            break;
        case REM:
            codegen(root->lhs);
            codegen(root->rhs);
            printf("rem r%d r%d r%d\n",num-2,num-2,num-1);
            num--;
            break;
        case LPAR:
            return codegen(root->mid);
            break;
        case IDENTIFIER:
            printf("load r%d [%d]\n",num,alpha[root->val - 'x']);
            num++;
            break;
        case CONSTANT:
            if(root->val < 0)
                printf("sub r%d 0 %d\n",num,abs(root->val));
            else
            printf("add r%d 0 %d\n",num,root->val);
            num++;
            break;
        case PLUS:
            printf("add r%d 0 %d\n",num,root->mid->val);
            num++;
            break;
        case MINUS:
            if(root->mid->kind == CONSTANT)
               printf("sub r%d 0 %d\n",num,root->mid->val);
            else if(root->mid->kind == IDENTIFIER)
            {
                printf("load r%d [%d]\n",num,alpha[root->mid->val - 'x']);
                printf("mul r%d r%d 2\n",num+1,num);
                printf("sub r%d r%d r%d\n",num,num,num+1);
            }
            else if(root->mid->kind == PREDEC || root->mid->kind == PREINC ||
                    root->mid->kind == POSTDEC|| root->mid->kind == POSTINC)
            {
 //****
                f=1;
                codegen(root->mid);
                printf("load r%d [%d]\n",num,alpha[root->mid->mid->val - 'x']);
                printf("mul r%d r%d 2\n",num+1,num);
                printf("sub r%d r%d r%d\n",num,num,num+1);
            }
            else if(root->mid->kind == ASSIGN)
            {
                codegen(root->mid);
                printf("load r%d [%d]\n",num,alpha[root->mid->lhs->val - 'x']);
                printf("sub r%d 0 r%d\n",num,num);
            }
            //x=-(y=8+y);
            num++;
            break;
        case PREINC:
            printf("load r%d [%d]\n",num,alpha[root->mid->val - 'x']);
            printf("add r%d 1 r%d\n",num,num);
            printf("store [%d] r%d\n",alpha[root->mid->val - 'x'],num);
            if(f) f=0,num = num;
            else num++;
            break;
        case PREDEC:
            printf("load r%d [%d]\n",num,alpha[root->mid->val - 'x']);
            printf("sub r%d r%d 1\n",num,num);
            printf("store [%d] r%d\n",alpha[root->mid->val - 'x'],num);
            if(f) f=0,num = num;
            else num++;
            break;
        case POSTINC:
            codegen(root->mid);
            printf("add r%d r%d 1\n",num,num-1);
            printf("store [%d] r%d\n",alpha[root->mid->val - 'x'],num);
            break;
        case POSTDEC:
            codegen(root->mid);
            printf("sub r%d r%d 1\n",num,num-1);
            printf("store [%d] r%d\n",alpha[root->mid->val - 'x'],num);
            break;
        default: return;
    }
}
void freeAST(AST *now) {
	if (now == NULL) return;
	freeAST(now->lhs);
	freeAST(now->mid);
	freeAST(now->rhs);
	free(now);
}

void token_print(Token *in/*, size_t len*/) {
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "PreInc", "PreDec", "PosInc", "PosDec", "Identifier", "Constant", "LPar", "RPar", "Plus", "Minus", "End"
	};
	while(in)
    {
        printf("(%s,%d) ",KindName[in->kind],in->val);
        in = in->next;
    }
}

void AST_print(AST *head) {
	static char indent_str[MAX_LENGTH] = "";
	static int indent = 0;
	char *indent_now = indent_str + indent;
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "PreInc", "PreDec", "PostInc", "PostDec", "Identifier", "Constant", "Parentheses", "Parentheses", "Plus", "Minus"
	};
	if(head)
    {
        AST_print(head->lhs);
        AST_print(head->mid);
        AST_print(head->rhs);
        printf("(%s,%d)",KindName[head->kind],head->val);
    }
}
