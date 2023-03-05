void pos (int line, int col) {}

float a = (pos(1,8),{ 1.0 });
float b = (pos(2,8), a = (pos(2,12), 3.0));
b = (pos(3,2), f(c= (pos(3,7),4.0),d= (pos(3,13),e= (pos(3,15),5.0)))), a= (pos(3,23),b);
// b = (pos(3,2), f(c= (pos(3,7),4.0),d= (pos(3,13),e= (pos(3,15),5.0)))), a= (pos(3,23),b);
