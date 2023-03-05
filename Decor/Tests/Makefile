.RECIPEPREFIX = > # Use '>' instead of TAB for recipes

CXX=g++
CXXFLAGS=#-std=c++11
LIBS=#
CLANG=/usr/bin/clang
BIN=test-case.rhs

.PHONY: all clean distclean

all: ${BIN} ${BIN}.ll ${BIN}.E.cc

${BIN}: ${BIN}.o
> $(CXX) $^ $(LIBS) -o $@

${BIN}.E.cc: ${BIN}.cc
> ${CXX} -E -c $^ -o $@

${BIN}.bc: ${BIN}.cc
> ${CLANG} -emit-llvm -c $^ $(CXXFLAGS) -o $@

${BIN}.ll: ${BIN}.bc
> llvm-dis $^

clean:
> rm -f ${BIN}.o ${BIN}.bc ${BIN}.E.cc

distclean: clean
> rm -f ${BIN} ${BIN}.ll
