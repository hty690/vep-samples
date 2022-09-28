#include<iostream>
#include<cstring>
#include<cstdlib>
#include<cstdio>
#include<cstring>
using namespace std;

#define BPF_ALU64 0x07
#define BPF_JMP32 0x06
#define BPF_JMP 0x05
#define BPF_ALU 0x04
#define BPF_STX 0x03
#define BPF_ST 0x02
#define BPF_LDX 0x01
#define BPF_LD 0x00
#define BPF_CLASS(code) (code & (0x07))
#define BPF_SRC(code) (code & (0x08))
#define BPF_OP(code) (code & (0xf0))
#define BPF_SZ(code) (code & (0x18))
#define BPF_MZ(code) (code & (0xe0))

int code;
int dest,src,off,n;
long long imm;
int x,y;
char trash[200];
int a[20],Code[20];
int len;
int opcode;
int SZ;
int MZ;
char BPF_OP_C[][20] = {
 "BPF_ADD", "BPF_SUB", "BPF_MUL", "BPF_DIV", "BPF_OR", "BPF_AND", "BPF_LSH",
 "BPF_RSH", "BPF_NEG", "BPF_MOD", "BPF_XOR", "BPF_MOV", "BPF_ARSH", "BPF_END"  
};
char BPF_JMP_OP[][20] = {
  "JMP BPF_GOTO", "JMP BPF_JEQ", "JMP BPF_JGT", "JMP BPF_JGE", "JMP BPF_JSET", "JMP BPF_JNE", "JMP BPF_JSGT", "JMP BPF_JSGE",
  "BPF_CALL", "BPF_EXIT", "JMP BPF_JLT", "JMP BPF_JLE", "JMP BPF_JSLT", "JMP BPF_JSLE"
};
char Size_M[][20] = {
  "BPF_W", "BPF_HW", "BPF_B", "BPF_DW"
};

char Mode_M[][20] = {
  " BPF_IMM", " BPF_ABS", " BPF_IND", " BPF_MEM", " BPF_LEN", " BPF_MSH", " BPF_ATOMIC"
};

long long offset[] = {
  0x0, 0xf, 0xff, 0xfff, 0xffff, 0xfffff, 0xffffff, 0xfffffff, 0xffffffff, 0xfffffffff, 0xffffffffff,0xfffffffffff, 0xffffffffffff
};

char op[100];

long long compute_imm(bool flag,int len)
{
  static long long num;
  num = 0;
  if (flag) 
  {
    for (int i = 0 ; i < len; ++i) num = num << 8 | a[i];
    if (a[0] & 0x80) num = num - offset[len * 2] - 1;
  }
  else 
  {
    for (int i = len - 1; i>=0 ;--i) num = num << 8 | a[i];
    if (a[len - 1] & 0x80) num = num - offset[len * 2] - 1;
  }
  return num;
}

void Do_scanf(){
  n --;
  for (int i = 0;i<4;++i)
  {
    scanf("%x",&code);
    Code[i << 1 | 1] = code >> 8;
    Code[i << 1] = code & (0xff); 
  }
}

void Get_op()
{

  SZ = BPF_SZ(code) >> 3;
  MZ = BPF_MZ(code) >> 5;
  opcode = BPF_OP(code) >> 4;
  len = 4;
  switch (BPF_CLASS(code)){
    case BPF_ALU : strcpy(op,BPF_OP_C[opcode]);
                    break;
     case BPF_ALU64 : strcpy(op,BPF_OP_C[opcode]);
                    break; 
     case BPF_JMP : strcpy(op,BPF_JMP_OP[opcode]);
                    break;
     case BPF_JMP32 : strcpy(op,BPF_JMP_OP[opcode]);
                    break;
     case BPF_ST :  if (SZ == 3 && MZ != 3) len = 12;
                    strcpy(op, Size_M[SZ]);
                    strcat(op, Mode_M[MZ]);
                    break;
     case BPF_STX : if (SZ == 3 && MZ != 3) len = 12;
                    strcpy(op, Size_M[SZ]);
                    strcat(op, Mode_M[MZ]);
                    break;
     case BPF_LD :  if (SZ == 3 && MZ != 3) len = 12;
                    strcpy(op, Size_M[SZ]);
                    strcat(op, Mode_M[MZ]);
                    break;
     case BPF_LDX : if (SZ == 3 && MZ != 3) len = 12;
                    strcpy(op, Size_M[SZ]);
                    strcat(op, Mode_M[MZ]);
                    break;
  }
  for (int i = 0;i<4;++i) a[i] = Code[i + 4];
  if (len == 12)
  {
     Do_scanf();
     for (int i = 4;i<12;++i) a[i] = Code[i - 4];
     len = 8;
     for (int i = 4;i< 8;++i) a[i] = a[i + 4];
  }
  imm = compute_imm(0, len);
}

void Print_off_imm()
{
  if (off < 0) printf("(%d) ", off); else printf("%d " ,off);
  if (imm < 0) printf("(%lld)", imm); else printf("%lld" ,imm);
}
int main()
{
  freopen("in.txt","r",stdin);
  freopen("out.txt","w",stdout);
  puts("Definition STATEMENT : list Singleton_statement := ");
  n = 1;
  Do_scanf();
  n = (Code[6] << 8 | Code[7]) / 8;
  while (n)
  {
    putchar('\t');
    Do_scanf(); 
    code  = Code[0];
    src = (Code[1] & (0xf0)) >> 4; dest = Code[1] & (0x0f);
    off = (Code[3] & 0x80) ? (Code[3] << 8 | Code[2] - 0xffff - 1) : Code[3] << 8 | Code[2];
    if (BPF_CLASS(code) < 0x08) printf("Op%d ", BPF_CLASS(code));
    Get_op();
    switch (BPF_CLASS(code)){
     case BPF_ALU : printf("(ALU %s) %s %d %d ", op, BPF_SRC(code) ? "BPF_X" : "BPF_K",dest, src);
                    Print_off_imm();
                    break;
     case BPF_ALU64 : printf("(ALU64 %s) %s %d %d ", op, BPF_SRC(code) ? "BPF_X" : "BPF_K",dest, src);
                      Print_off_imm();
                    break; 
     case BPF_JMP :  printf("(%s) %s %d %d ", op, BPF_SRC(code) ? "BPF_X" : "BPF_K",dest, src);
                    Print_off_imm();
                    break;
     case BPF_JMP32 : printf("(JMP32 %s) %s %d %d ", op, BPF_SRC(code) ? "BPF_X" : "BPF_K",dest, src);
                    Print_off_imm();
                    break;
     case BPF_ST :  printf("(ST %s) %d %d ", op,dest, src);
                    Print_off_imm();
                    break;
     case BPF_STX : printf("(STX %s) %d %d ", op,dest, src);
                    Print_off_imm();
                    break;
     case BPF_LD : printf("(LD %s) %d %d ", op,dest, src);
                   Print_off_imm();
                    break;
     case BPF_LDX : printf("(LDX %s) %d %d ", op,dest, src);
                    Print_off_imm();
                    break;
     default : printf("Wrong operation.");
   }
   if (len == 8) {puts(" ::"); printf("\tEmpty_line ");} 
    if (n) puts(" ::");
    else puts(" :: nil.");
  }
  Do_scanf();
  int m = Code[3];
  for (int i = 0;i<m;++i)
  {
    printf("Ebpf_map (Ez_val %d) ", Code[6] << 8 | Code[7]);
    Do_scanf();
    printf("(Ez_val %d) (Ez_val %d) ", Code[2] << 8 | Code[3], Code[6] << 8 | Code[7]);
    Do_scanf();
    printf("(Ez_val %d) (Ez_val %d)\n", Code[2] << 8 | Code[3],Code[6] << 8 | Code[7]);
    Do_scanf();
  }
  fclose(stdin);
  fclose(stdout);
  return 0;
}