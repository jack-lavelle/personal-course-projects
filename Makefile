# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
all : seepp.native printbig draw drawcircle


# "make seepp.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

seepp.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind seepp.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
	rm -f *.err *.ll *.s *.log
	rm -f *.o

# Testing the "printbig" example

printbig : printbig.c
#	cc -c -DBUILD_TEST printbig.c
	gcc -c -fPIC printbig.c
#	llvm-as-6.0 printbig.ll -o printbig.bc

draw : draw.c svg.c
	gcc -c -fPIC draw.c
	gcc -c -fPIC svg.c

drawcircle : drawcircle.c svg.c
	gcc -c -fPIC drawcircle.c
	gcc -c -fPIC svg.c

# Building the tarball
TESTS = canvascircle

#TESTS = \
#  add1 arith1 arith2 arith3 fib float1 float2 float3 for1 for2 func1 \
#  func2 func3 func4 func5 func6 func7 func8 func9 gcd2 gcd global1 \
#  global2 global3 hello if1 if2 if3 if4 if5 if6 local1 local2 ops1 \
#  ops2 printbig var1 var2 while1 while2 call canvasassign canvasconstruct\
#	pixelassign pixelconstruct draw fib prints ptassign ptconstruct shoehorn\
#  array-access-change array-access array-decl field

FAILS = canvascircle

#FAILS = \
  assign1 assign2 assign3 dead1 dead2 expr1 expr2 expr3 float1 float2 \
  for1 for2 for3 for4 for5 func1 func2 func3 func4 func5 func6 func7 \
  func8 global1 global2 if1 if2 if3 nomain printbig printb print \
  return1 return2 while1 while2 canvasassign canvasconstruct pixelconstruct-numargs\
	pixelconstruct-typeargs ptassign ptconstruct shoehorn field-assign field-invexpr field-prim\


TESTFILES = $(TESTS:%=test-%.seepp) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.seepp) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags seepp.ml parserseepp.mly \
	README scanner.mll semant.ml testall.sh \
	printbig.c draw.c drawcircle.c svg.c svg.h \
	Dockerfile \
	$(TESTFILES:%=tests/%) 

seepp.tar.gz : $(TARFILES)
	cd .. && tar czf compiler/seepp.tar.gz \
		$(TARFILES:%=compiler/%)
