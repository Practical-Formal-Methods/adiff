
int main() {

  int x;
  if (x) {
    goto ERROR;
  }
  return 0;

 ERROR:
  return 1;
}
