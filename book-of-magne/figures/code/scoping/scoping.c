// A program to demonstrate static scoping.

int x = 10;
 
// Called by g()
int f()
{
   return x;
}
 
// g() has its own variable
// named as x and calls f()
int g()
{
   int x = 20;
   return f();
}
 
int main()
{
  print(g());
  return 0;
}