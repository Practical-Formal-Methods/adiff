extern void __VERIFIER_error() __attribute__ ((__noreturn__));
void __VERIFIER_assert(int cond) { if(!(cond)) { ERROR: __VERIFIER_error(); } }

int main() {
  int i = 0;
  while(i > 0 ) {
    i++;
  }
  __VERIFIER_assert(1);

}

