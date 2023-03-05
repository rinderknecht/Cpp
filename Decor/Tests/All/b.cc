float a ={ 1.0 };
float b = a = 3.0;
b = f(c=4.0,d=e=5.0), a=b;
// b = (pos(3,2), f(c= (pos(3,7),4.0),d= (pos(3,13),e= (pos(3,15),5.0)))), a= (pos(3,23),b);
