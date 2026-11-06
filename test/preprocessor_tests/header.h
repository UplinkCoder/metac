#define HEADER_VERSION 1
#define HEADER_MACRO(x) (x + 1)
#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define COMPLEX_MACRO(a, b) \
  ((a) + (b))

#ifndef TEST_FLAG
#define TEST_FLAG 100
#endif
