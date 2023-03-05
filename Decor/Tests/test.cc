#include <iostream>
#include "bff/bff-mpfr.h"
#include <string>

/*
float extract_var_flt(float var, const std::string& var_name) {
  std::cout << var_name << "[f]: " << var << std::endl;
  return var;
}
*/

// double extract_var_dlb(double var, const std::string& var_name) {
//   std::cout << var_name << "[d]: " << var << std::endl;
//   return var;
// }

// bff::Mpfr& extract_var_mpfr(bff::Mpfr& var, const std::string& var_name) {
//   std::cout << var_name << "[m]: " << var << std::endl;
//   return var;
// }

/*
char ab = default;

t v *= fancy_id + 1;
namespace foo 
= bar;
class foo 
= { };

using ns = std;
class
= 
{};
*/

float x = /* affectation! */ { 1.0 };

/*
typename t = foo;
typename = bar;
virtual void f (int x = (x=0)) = 0;
virtual void g 
(int x
) = 
0;
a.operator+=(b);
*/

long double p2 = [=](float x, float y) {
  std::cout << "pwet" << std::endl; return x+y; } (f1=3, f2=22);

void pos(const int line = 0, const int col) { }

void flts() {

  std::string s = "abc\"\n";

  typedef float flt_t;

  flt_t a;
  //  flt_t b;
  //  flt_t c;

  //  a = b = c = 3.5;
  //  a = 3 + b + (c = (pos(29,15),35)) + 4;
  //  a += 3;
  /*
  a += 3.5;
  a = i++ + 2;
  b = ++i + 3;
  c = i-- + 4;
  d = --i + 5;
  */
}

// void mpfrs() {
//   typedef bff::Mpfr flt_t;
//   float x = 0;

//   flt_t a;
//   flt_t b;
//   flt_t c;

//   a = b = c = 3.5;
// }

int main() {
  flts();
  //  mpfrs();
  a = 13.2 * std::sqrt(b) / d[(int)a+1] + - + - - - + c;
  if (a >= 0) {}

  return 0;
}

