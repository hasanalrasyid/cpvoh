#include <stdio.h>
#include <A_C.h>

extern void dbgstamp_ (int*,char*,int*);
extern double add_ (int*,double*,int*,double*);

double ar1[4]={1.0, 2.0, 3.0, 4.0};
double ar2[4]={5.0, 6.0, 7.0, 8.0};

int main (int argc, char** argv){
  int ifile = 0;
  char* nFile = "sa";
  int fline = 25;
  int x, y;
  double z;
  x = 4;
  y = 4;
  z = add_(&x, ar1, &y, ar2); 
  dbgstamp_(&ifile,nFile, &fline);
  A_static_function();
  _A a = A_new();
  A_member_function(a);
  printf("@call_a.c=========%f",z);
  return 0;
}
