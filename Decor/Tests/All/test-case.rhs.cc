void pos (int line, int col) {}

#include <iostream>
#include <limits>

class some_class
{
 public:
 some_class() {}
 some_class(int i) {}
 some_class(float y) {}
 some_class(int i, float y) {}
};

void test_initialization()
{
 const char* c1   = (pos(15,18), "test" "-" "case");
#if 0
 const char  c2[] = (pos(17,18), { "hello world" });
 const char  c3[] = (pos(18,18), { 'h', 'e', 'l', 'l', 'o', 0x20, 'w', 'o', 'r', 'l', 'd', '\n', '\0' });
 if (c1 || c2[0] || c3[0]) {} // avoir unused variables
#endif
 int i1;
 int i2 = (pos(22,8), 3);
 int i3 = (pos(23,8), i2++);
 int i4 = (pos(24,8), ++i2);

 float       f1 = (pos(26,16), 0.1f);
 double      f2 = (pos(27,16), 0.2);
 float       f3 = (pos(28,16), 0.3l);
 long double f4 = (pos(29,16), std::numeric_limits<long double>::quiet_NaN());
 float       f5 /* comment in the middle */ = (pos(30,44), /* another comment in the middle */ 5.5);

 float       f6 /* = 99.99; */ = (pos(32,31), /* 88.88; */ 74.77 // ;
 /* oh, another comment ! */ + /* right there */ 3); // /* this last one is a little tricky

 /* hello world
    //
    float f7 = "haha";
 */
 // if (f1 || f2 || f3 || f4 || f5 || f6) {}

 // Float Arrays:
#if 0
 double d2[ ] = (pos(43,14), {0.5, 0.125, 0.0625});
 double d3[5][5] = (pos(44,17), { { 1.0, 0.0, 0.1, 0.2, 0.3 },
                     { 0.4, 1.0, 0.5, 0.6, 0.7 },
       { 0.8, 0.9, 1.0, 1.1, 1.2 },
       { 1.3, 1.4, 1.5, 1.0, 1.6 },
       { 1.7, 1.8, 1.9, 2.0, 1.0 } });
#endif
 double* d4 = (pos(50,12), new double[5]);   // Not initialized
 double* d5 = (pos(51,12), new double[5]()); // All initialized to 0.0
 double* d6 = (pos(52,12), new double(3.0));

 // if (d2[0] || d3[0][0]) {}
 delete [] d4;
 delete [] d5;

 #if __cplusplus >= 201103L
#if 0
 double p1 = (pos(60,11), { 0.125 });
#endif
 // A vicious one:
 long double p2 = (pos(63,16), [=](float x, float y) { return x+y; } (f1= (pos(63,59),3), f2= (pos(63,65),2222)));
 long double p3 = (pos(64,16), [&](float x, float y) { f3 = (pos(64,45), 33); return x+y; } (f1= (pos(64,68),4), f2= (pos(64,74),21)));
#if 0
 if (p1 || p2 || p3) {}
#endif
 #endif

 // Multi:
 float m1, m2= (pos(71,13),33.5), m3= (pos(71,22),55.55); double m4= (pos(71,39),11.22);
 if (m1 || m2 || m3 || m4) {}

 // Objects:
 some_class o1;
 some_class o2(int x = (pos(76,21), 3)); // Not an object call: function declaration with default value for argument x
 some_class o3(2);
 some_class o4 = 2; // call to constructor
 some_class o5(6.5f);
 some_class o6 = 7.5f;

 #if __cplusplus >= 201103L
 some_class o7{};
 some_class o8{1};
 some_class o9{5.5f};
 some_class o10{1, 6.6f};
 #endif
}


void test_expressions()
{
}

int main()
{
 test_initialization();
 test_expressions();

}

