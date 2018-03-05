extern void __VERIFIER_error() __attribute__ ((__noreturn__));
void __VERIFIER_assert(int cond) { if(!(cond)) { ERROR: __VERIFIER_error(); } }


int main() {

  int x, y;

  x = 2;

  if (x == 3) {
    __VERIFIER_error();
  } else {
    5 + 3;
  }
}
