#lang typed/racket

(require typed/rackunit)
(require "compiler.rkt")
(require "definitions.rkt")

;; TYPED/RACKET TEST CASES

;; TEST CASES for front-end

#| 1.mini
{
   int i;
   int j1;
   bool b;
   struct A a;
};
struct B
{
   struct A a;
};

int i, j, k;
struct B b, bb, bbb;
bool bob;

fun f(int i, struct B j) struct A
{
   int f, l, k;
   return b.a.a.a.a;
}

fun foo(int n) int
{
   if (n <= 0)
   {
      return 1;
   }
   else
   {
      return n * foo(n - 1);
   }
}

fun g(int i, struct B j) int
{
   int g, m, k;
   return 3;
}

fun main() int
{
   struct A a;
   int i, j, k;
   bool b;
      int h;
   {
      i = --2;
      if (i < g(1,null))
      {
         print 1;
      }

      if (i > g(1,null))
      {
         print 1;
      }
      else
      {
         print 3 endl;
      }
      while (true)
      {
         print 7;
      }
      f(g(1,new B),new B);
      print f(g(1,new B),new B).i endl;
   }
   return 0;
} |#
(check-equal? (front-end "../mile1/test_cases/1.json")
              (Program
               (list (Struct 'A (list (Declaration 'int 'i) (Declaration 'int 'j1) (Declaration 'bool 'b) (Declaration 'A 'a))) (Struct 'B (list (Declaration 'A 'a))))
               (list (Declaration 'int 'i) (Declaration 'int 'j) (Declaration 'int 'k) (Declaration 'B 'b) (Declaration 'B 'bb) (Declaration 'B 'bbb) (Declaration 'bool 'bob))
               (list
                (Function
                 'f
                 (list (Declaration 'int 'i) (Declaration 'B 'j))
                 'A
                 (list (Declaration 'int 'f) (Declaration 'int 'l) (Declaration 'int 'k))
                 (list (Return (Selector (Selector (Selector (Selector 'b 'a) 'a) 'a) 'a))))
                (Function
                 'foo
                 (list (Declaration 'int 'n))
                 'int
                 '()
                 (list (If-Else (Binary '<= 'n 0) (Block (list (Return 1))) (Block (list (Return (Binary '* 'n (Invocation 'foo (list (Binary '- 'n 1))))))))))
                (Function 'g (list (Declaration 'int 'i) (Declaration 'B 'j)) 'int (list (Declaration 'int 'g) (Declaration 'int 'm) (Declaration 'int 'k)) (list (Return 3)))
                (Function
                 'main
                 '()
                 'int
                 (list (Declaration 'A 'a) (Declaration 'int 'i) (Declaration 'int 'j) (Declaration 'int 'k) (Declaration 'bool 'b) (Declaration 'int 'h))
                 (list
                  (Block
                   (list
                    (Assignment 'i 2)
                    (If (Binary '< 'i (Invocation 'g '(1 null))) (Block (list (Print 1 #f))))
                    (If-Else (Binary '> 'i (Invocation 'g '(1 null))) (Block (list (Print 1 #f))) (Block (list (Print 3 #t))))
                    (Loop #t (Block (list (Print 7 #f))))
                    (Invocation 'f (list (Invocation 'g (list 1 (Allocation 'B))) (Allocation 'B)))
                    (Print (Selector (Invocation 'f (list (Invocation 'g (list 1 (Allocation 'B))) (Allocation 'B))) 'i) #t)))
                  (Return 0))))))

#| 2.mini (return integer, expects struct A)
struct A
{
   int i;
   int j1;
   bool b;
   struct A a;
};
struct B
{
   struct A a;
};

int i, j, k;
struct B b, bb, bbb;
bool bob;

fun f(int i, struct B j) struct A
{
   int l, k;
   return b.a.a.a.a.i;              # remove i
}

fun foo(int n) int
{
   if (n + 0)                       # change operator
   {
      return 1;
   }
   else
   {
      return n * foo(n - 1);
   }
}

fun g(int i, struct B j) int
{
   int g, m, k;
   return;                          # add an integer return value
}

fun main() int
{
   struct A a;
   int i, j, k;
   bool b;
      int h;
   {
      i = --2;
      if (i < g(1,null))
      {
         print 1;
      }

      if (i > g(1,null))
      {
         print 1;
      }
      else
      {
         print 3 endl;
      }
      while (true < false)          # remove comparison
      {
         print 7;
      }
      f(g(1,new B),2,new B);        # f(g(1,new B),new B);
      print f(2,new B).bob endl;    # change .bob to .i
      print foo(2).a endl;          # remove .a
   }
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "Return expression (Selector (Selector (Selector (Selector (Selector 'b 'a) 'a) 'a) 'a) 'i) failed type check; given type 'int, expected type 'A"))
 (lambda () (front-end "../mile1/test_cases/2_a.json")))

#|2.mini (if statement has integer guard)
struct A
{
   int i;
   int j1;
   bool b;
   struct A a;
};
struct B
{
   struct A a;
};

int i, j, k;
struct B b, bb, bbb;
bool bob;

fun f(int i, struct B j) struct A
{
   int l, k;
   return b.a.a.a.a;
}

fun foo(int n) int
{
   if (n + 0)                       # change operator
   {
      return 1;
   }
   else
   {
      return n * foo(n - 1);
   }
}

fun g(int i, struct B j) int
{
   int g, m, k;
   return;                          # add an integer return value
}

fun main() int
{
   struct A a;
   int i, j, k;
   bool b;
      int h;
   {
      i = --2;
      if (i < g(1,null))
      {
         print 1;
      }

      if (i > g(1,null))
      {
         print 1;
      }
      else
      {
         print 3 endl;
      }
      while (true < false)          # remove comparison
      {
         print 7;
      }
      f(g(1,new B),2,new B);        # f(g(1,new B),new B);
      print f(2,new B).bob endl;    # change .bob to .i
      print foo(2).a endl;          # remove .a
   }
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "If statements require boolean guards; given expression (Binary '+ 'n 0) of type 'int"))
 (lambda () (front-end "../mile1/test_cases/2_b.json")))

#| 2.mini (returns void, expects integer)
struct A
{
   int i;
   int j1;
   bool b;
   struct A a;
};
struct B
{
   struct A a;
};

int i, j, k;
struct B b, bb, bbb;
bool bob;

fun f(int i, struct B j) struct A
{
   int l, k;
   return b.a.a.a.a;
}

fun foo(int n) int
{
   if (n > 0)                       
   {
      return 1;
   }
   else
   {
      return n * foo(n - 1);
   }
}

fun g(int i, struct B j) int
{
   int g, m, k;
   return;                          # add an integer return value
}

fun main() int
{
   struct A a;
   int i, j, k;
   bool b;
      int h;
   {
      i = --2;
      if (i < g(1,null))
      {
         print 1;
      }

      if (i > g(1,null))
      {
         print 1;
      }
      else
      {
         print 3 endl;
      }
      while (true < false)          # remove comparison
      {
         print 7;
      }
      f(g(1,new B),2,new B);        # f(g(1,new B),new B);
      print f(2,new B).bob endl;    # change .bob to .i
      print foo(2).a endl;          # remove .a
   }
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "Return statement failed type check; given type 'void, expected type 'int"))
 (lambda () (front-end "../mile1/test_cases/2_c.json")))

#| 2.mini (< operator with boolean operands)
struct A
{
   int i;
   int j1;
   bool b;
   struct A a;
};
struct B
{
   struct A a;
};

int i, j, k;
struct B b, bb, bbb;
bool bob;

fun f(int i, struct B j) struct A
{
   int l, k;
   return b.a.a.a.a;
}

fun foo(int n) int
{
   if (n > 0)                       
   {
      return 1;
   }
   else
   {
      return n * foo(n - 1);
   }
}

fun g(int i, struct B j) int
{
   int g, m, k;
   return g;                  
}

fun main() int
{
   struct A a;
   int i, j, k;
   bool b;
      int h;
   {
      i = --2;
      if (i < g(1,null))
      {
         print 1;
      }

      if (i > g(1,null))
      {
         print 1;
      }
      else
      {
         print 3 endl;
      }
      while (true < false)          # remove comparison
      {
         print 7;
      }
      f(g(1,new B),2,new B);        # f(g(1,new B),new B);
      print f(2,new B).bob endl;    # change .bob to .i
      print foo(2).a endl;          # remove .a
   }
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "Binary operator '< requires operands of types '(int int); given expressions #t of type 'bool and #f of type 'bool"))
 (lambda () (front-end "../mile1/test_cases/2_d.json")))

#| 2.mini (function invocation has too many arguments)
struct A
{
   int i;
   int j1;
   bool b;
   struct A a;
};
struct B
{
   struct A a;
};

int i, j, k;
struct B b, bb, bbb;
bool bob;

fun f(int i, struct B j) struct A
{
   int l, k;
   return b.a.a.a.a;
}

fun foo(int n) int
{
   if (n > 0)                       
   {
      return 1;
   }
   else
   {
      return n * foo(n - 1);
   }
}

fun g(int i, struct B j) int
{
   int g, m, k;
   return g;                    
}

fun main() int
{
   struct A a;
   int i, j, k;
   bool b;
      int h;
   {
      i = --2;
      if (i < g(1,null))
      {
         print 1;
      }

      if (i > g(1,null))
      {
         print 1;
      }
      else
      {
         print 3 endl;
      }
      while (true)       
      {
         print 7;
      }
      f(g(1,new B),2,new B);        # f(g(1,new B),new B);
      print f(2,new B).bob endl;    # change .bob to .i
      print foo(2).a endl;          # remove .a
   }
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "Invocation of function 'f failed type check; given expressions (list (Invocation 'g (list 1 (Allocation 'B))) 2 (Allocation 'B)) of types '(int int B), expected types '(int B)"))
 (lambda () (front-end "../mile1/test_cases/2_e.json")))

#| 2.mini (invalid structure field name)
struct A
{
   int i;
   int j1;
   bool b;
   struct A a;
};
struct B
{
   struct A a;
};

int i, j, k;
struct B b, bb, bbb;
bool bob;

fun f(int i, struct B j) struct A
{
   int l, k;
   return b.a.a.a.a;
}

fun foo(int n) int
{
   if (n > 0)                       
   {
      return 1;
   }
   else
   {
      return n * foo(n - 1);
   }
}

fun g(int i, struct B j) int
{
   int g, m, k;
   return g;                    
}

fun main() int
{
   struct A a;
   int i, j, k;
   bool b;
      int h;
   {
      i = --2;
      if (i < g(1,null))
      {
         print 1;
      }

      if (i > g(1,null))
      {
         print 1;
      }
      else
      {
         print 3 endl;
      }
      while (true)       
      {
         print 7;
      }
      f(g(1,new B),new B);      
      print f(2,new B).bob endl;    # change .bob to .i
      print foo(2).a endl;          # remove .a
   }
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "Structure 'A field access failed: \"lookup-type: Type check failed: type 'bob has not been defined\""))
 (lambda () (front-end "../mile1/test_cases/2_f.json")))

#| 2.mini (cannot perform field access on an integer)
struct A
{
   int i;
   int j1;
   bool b;
   struct A a;
};
struct B
{
   struct A a;
};

int i, j, k;
struct B b, bb, bbb;
bool bob;

fun f(int i, struct B j) struct A
{
   int l, k;
   return b.a.a.a.a;
}

fun foo(int n) int
{
   if (n > 0)                       
   {
      return 1;
   }
   else
   {
      return n * foo(n - 1);
   }
}

fun g(int i, struct B j) int
{
   int g, m, k;
   return g;                    
}

fun main() int
{
   struct A a;
   int i, j, k;
   bool b;
      int h;
   {
      i = --2;
      if (i < g(1,null))
      {
         print 1;
      }

      if (i > g(1,null))
      {
         print 1;
      }
      else
      {
         print 3 endl;
      }
      while (true)       
      {
         print 7;
      }
      f(g(1,new B),new B);      
      print f(2,new B).i endl;  
      print foo(2).a endl;          # remove .a
   }
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "Field access requires a structure type; given expression (Invocation 'foo '(2)) of type 'int"))
 (lambda () (front-end "../mile1/test_cases/2_g.json")))

#| ret.mini (with added main to pass static analysis)
fun main() int
{ 
   return 5;
}

fun foo(int i) int      # standard case
{
   return i;
}

fun bar(int i) int      # control flow case
{
   if (i > 0)
   {
      return 1;
   }
   else
   {
      if (i < 0)
      {
         return -1;
      }
      else
      {
         return 0;
      }
   }
}
|#
(check-equal? (front-end "../mile1/test_cases/ret.json")
              (Program
               '()
               '()
               (list
                (Function 'main '() 'int '() (list (Return 5)))
                (Function 'foo (list (Declaration 'int 'i)) 'int '() (list (Return 'i)))
                (Function
                 'bar
                 (list (Declaration 'int 'i))
                 'int
                 '()
                 (list
                  (If-Else
                   (Binary '> 'i 0)
                   (Block (list (Return 1)))
                   (Block (list (If-Else (Binary '< 'i 0)
                                         (Block (list (Return (Unary '- 1))))
                                         (Block (list (Return 0))))))))))))

#| simple_prog.mini
int i, j;

fun main() int
{
   return 4;
} |#
(check-equal? (front-end "../mile1/test_cases/simple_prog.json")
              (Program
               '()
               (list (Declaration 'int 'i) (Declaration 'int 'j))
               (list (Function 'main '() 'int '() (list (Return 4))))))

#| redeclare_globals.mini
int i, j;
bool i;

fun main() int
{
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "Redeclaration of variable with identifier 'i is not allowed"))
 (lambda () (front-end "../mile1/test_cases/redeclare_globals.json")))

#| redeclare_params.mini
int i, j;

fun foo(int a, int a) int
{
   return 0;
}

fun main() int
{
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "Function 'foo's parameters failed type check : \"bind-declarations: Redeclaration of variable with identifier 'a is not allowed\""))
 (lambda () (front-end "../mile1/test_cases/redeclare_params.json")))

#| redeclare_locals.mini
int i, j;

fun main() int
{
   int a;
   bool a;
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "Function 'main's local variables failed type check : \"bind-declarations: Redeclaration of variable with identifier 'a is not allowed\""))
 (lambda () (front-end "../mile1/test_cases/redeclare_locals.json")))

#| redeclare_functions.mini
int i, j;

fun main() int
{
   return 0;
}

fun f() int
{
   return 0;
}

fun f(int a) void
{
   return;
} |#
(check-exn
 (regexp
  (regexp-quote "Redeclaration of function with identifier 'f is not allowed"))
 (lambda () (front-end "../mile1/test_cases/redeclare_functions.json")))

#| redeclare_structures.mini
struct A
{
   int i;
   int j1;
   bool b;
   struct A a;
};
struct A
{
   int a;
};

fun main() int
{
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "Redeclaration of structure with identifier 'A is not allowed"))
 (lambda () (front-end "../mile1/test_cases/redeclare_structures.json")))

#| redeclare_fields.mini
struct A
{
   int i;
   bool i;
};

fun main() int
{
   return 0;
} |#
(check-exn
 (regexp
  (regexp-quote "Redeclaration of variable with identifier 'i is not allowed"))
 (lambda () (front-end "../mile1/test_cases/redeclare_fields.json")))

;; TEST CASES for type-check-program
(check-equal? (type-check-program (Program
                                   '()
                                   (list (Declaration 'int 'i) (Declaration 'int 'j))
                                   (list (Function 'main '() 'int '() (list (Return 4))))))
              (list (Function-Binding 'main '() 'int)))
; (structure names are in a separate namespace from variables)
(check-equal? (type-check-program (Program
                                   (list (Struct 'A '()))
                                   (list (Declaration 'int 'A) (Declaration 'int 'j))
                                   (list (Function 'main '() 'int '() (list (Return 4))))))
              (list (Function-Binding 'main '() 'int)))
; (structure names are in a separate namespace from functions)
(check-equal? (type-check-program (Program
                                   (list (Struct 'A '()))
                                   (list (Declaration 'int 'i) (Declaration 'int 'j))
                                   (list (Function 'A '() 'int '() (list (Return 4))))))
              (list (Function-Binding 'A '() 'int)))

;; TEST CASES for check-valid-main
(check-valid-main (list (Function-Binding 'g (list 'int 'int) 'bool)
                        (Function-Binding 'h (list 'bool) 'void)
                        (Function-Binding 'main '() 'int)))
(check-exn
 (regexp
  (regexp-quote "No main function defined; every valid program must have a function named 'main that takes no arguments and returns an 'int"))
 (lambda () (check-valid-main (list (Function-Binding 'g (list 'int 'int) 'bool)
                                    (Function-Binding 'h (list 'bool) 'void)))))
(check-exn
 (regexp
  (regexp-quote "'main has an invalid signature, expected '() -> 'int, given '(int) -> 'int"))
 (lambda () (check-valid-main (list (Function-Binding 'g (list 'int 'int) 'bool)
                                    (Function-Binding 'h (list 'bool) 'void)
                                    (Function-Binding 'main (list 'int) 'int)))))
(check-exn
 (regexp
  (regexp-quote "'main has an invalid signature, expected '() -> 'int, given '() -> 'bool"))
 (lambda () (check-valid-main (list (Function-Binding 'g (list 'int 'int) 'bool)
                                    (Function-Binding 'h (list 'bool) 'void)
                                    (Function-Binding 'main '() 'bool)))))

;; TEST CASES for type-check-functions
(check-equal? (type-check-functions
               (list (Function 'main '() 'int '() (list (Return 4))))
               (list (Struct-Binding 'A (list (Type-Binding 'i 'int)
                                              (Type-Binding 'j 'bool))))
               (list (Type-Binding 'a 'int) (Type-Binding 'b 'int))
               '())
              (list (Function-Binding 'main '() 'int)))
; (function recursion)
(check-equal? (type-check-functions (list (Function 'f (list (Declaration 'int 'a)) 'int (list (Declaration 'int 'i))
                                                    (list (Return (Invocation 'f (list (Binary '- 'a 1)))))))
                                    '()
                                    '()
                                    '())
              (list (Function-Binding 'f (list 'int) 'int)))
(check-exn
 (regexp
  (regexp-quote "All paths through function 'f must return with type 'int"))
 (lambda () (type-check-functions
             (list (Function 'main '() 'int '() (list (Return 4)))
                   (Function 'f '() 'int (list (Declaration 'int 'i))
                             (list (If-Else
                                    true
                                    (Block (list (Return 4)))
                                    (Block (list (Assignment 'i 5)))))))
             '() '() '())))
(check-exn
 (regexp
  (regexp-quote  "Redeclaration of function with identifier 'f is not allowed"))
 (lambda () (type-check-functions
             (list (Function 'main '() 'int '() (list (Return 4)))
                   (Function 'f '() 'int '() (list (Return 4)))
                   (Function 'f '() 'int '()
                             (list (If-Else
                                    true
                                    (Block (list (Return 4)))
                                    (Block (list (Return 5)))))))
             (list (Struct-Binding 'A (list (Type-Binding 'i 'int)
                                            (Type-Binding 'j 'bool))))
             (list (Type-Binding 'a 'int) (Type-Binding 'b 'int))
             '())))

;; TEST CASES for all-paths-return?
(check-equal? (all-paths-return? (list (Return 'i))) #t)
(check-equal? (all-paths-return?
               (list (If-Else
                      (Binary '> 'i 0)
                      (Block (list (Return 1)))
                      (Block (list (If-Else
                                    (Binary '< 'i 0)
                                    (Block (list (Return (Unary '- 1))))
                                    (Block (list (Return 0)))))))))
              #t)
(check-equal? (all-paths-return?
               (list (If-Else
                      (Binary '> 'i 0)
                      (Block (list (Assignment 'i 5)))
                      (Block (list (If-Else
                                    (Binary '< 'i 0)
                                    (Block (list (Return (Unary '- 1))))
                                    (Block (list (Return 0)))))))))
              #f)
(check-equal? (all-paths-return?
               (list (If-Else
                      (Binary '> 'i 0)
                      (Block (list (Return 1)))
                      (Block (list (If-Else
                                    (Binary '< 'i 0)
                                    (Block (list (Assignment 'i 0)))
                                    (Block (list (Return 0)))))))))
              #f)
(check-equal? (all-paths-return?
               (list (If-Else
                      (Binary '> 'i 0)
                      (Block (list (Return 1)))
                      (Block (list (If-Else
                                    (Binary '< 'i 0)
                                    (Block (list (Return (Unary '- 1))))
                                    (Block (list (Print 5 #f)))))))))
              #f)
(check-equal? (all-paths-return?
               (list (If
                      (Binary '> 'i 0)
                      (Block (list (Return 1))))))
              #f)
(check-equal? (all-paths-return?
               (list (If
                      (Binary '> 'i 0)
                      (Block (list (Delete 'i))))))
              #f)
(check-equal? (all-paths-return?
               (list (If
                      (Binary '> 'i 0)
                      (Block (list (Delete 'i))))
                     (Return 3)))
              #t)
(check-equal? (all-paths-return?
               (list (If
                      (Binary '> 'i 0)
                      (Block (list (Return 'a))))
                     (Invocation 'f (list 5 2))))
              #f)

;; TEST CASES for bind-function
(check-equal? (bind-function (Function 'main '() 'int '() (list (Return 4))) '())
              (Function-Binding 'main '() 'int))
(check-exn
 (regexp
  (regexp-quote "Function 'f's parameters failed type check : \"bind-declarations: Redeclaration of variable with identifier 'a is not allowed\""))
 (lambda () (bind-function (Function 'f (list (Declaration 'int 'a) (Declaration 'bool 'a)) 'int '() (list (Return 4))) '())))

;; TEST CASES for type-check-function
(type-check-function (Function 'main '() 'int '() (list (Return 4))) '() '() '())
(type-check-function (Function 'f (list (Declaration 'int 'a)) 'int (list (Declaration 'int 'i))
                             (list (If-Else
                                    true
                                    (Block (list (Return 4)))
                                    (Block (list (Assignment 'i 5))))
                                   (Return 'a)))
                     '() '() '())
; (function parameter hides global declaration)
(type-check-function (Function 'main (list (Declaration 'int 'a)) 'int '() (list (Return 'a))) '() (list (Type-Binding 'a 'bool)) '())
(check-exn
 (regexp
  (regexp-quote "Function 'f's local variables failed type check : \"bind-structs: verify-type: verify-struct-id: Identifier 'B is not defined or is out of scope\""))
 (lambda () (type-check-function (Function 'f '() 'int (list (Declaration 'int 'a) (Declaration 'B 'b)) (list (Return 4)))
                                 '() '() '())))
; (local variable may not redeclare a parameter)
(check-exn
 (regexp
  (regexp-quote "Function 'f's local variables failed type check : \"bind-declarations: Redeclaration of variable with identifier 'b is not allowed\""))
 (lambda () (type-check-function (Function 'f (list (Declaration 'bool 'b)) 'int (list (Declaration 'int 'a) (Declaration 'B 'b)) (list (Return 4)))
                                 '() '() '())))
            
;; TEST CASES for type-check-statements
(type-check-statements (list (Return 5) (Assignment 'i true)) (Environment '() '() '() (cons (Type-Binding 'i 'bool) base-local-environment)) 'int)

;; TEST CASES for type-check-statement
(type-check-statement (Block (list (Assignment 'g false) (Print 'h #f)))
                      (Environment '() (list (Type-Binding 'g 'int) (Type-Binding 'h 'int)) '() (cons (Type-Binding 'g 'bool) base-local-environment))
                      'void)
(type-check-statement (Assignment 's 'null)
                      (Environment (list (Struct-Binding 'S (list (Type-Binding 'a 'int) (Type-Binding 'b 'bool))))
                                   (list (Type-Binding 's 'S) (Type-Binding 'h 'int))
                                   '()
                                   base-local-environment)
                      'void)
(type-check-statement (If
                       true
                       (Block (list (Return 4))))
                      (Environment '()
                                   (list (Type-Binding 'i 'int))
                                   '()
                                   base-local-environment)
                      'int)
(type-check-statement (If-Else
                       true
                       (Block (list (Return 4)))
                       (Block (list (Assignment 'i 5))))
                      (Environment '()
                                   (list (Type-Binding 'i 'int))
                                   '()
                                   base-local-environment)
                      'int)
(type-check-statement (Loop
                       true
                       (Block (list (Return-Void))))
                      (Environment '()'()'() base-local-environment)
                      'void)
(type-check-statement (Delete 'a)
                      (Environment (list (Struct-Binding 'A '())) '()'() (cons (Type-Binding 'a 'A) base-local-environment))
                      'int)
(type-check-statement (Return (Invocation 'f (list 'i 'j)))
                      (Environment '()
                                   '()
                                   (list (Function-Binding 'f (list 'int 'int) 'int))
                                   (append (list (Type-Binding 'i 'int)
                                                 (Type-Binding 'j 'int))
                                           base-local-environment))
                      'int)
(check-exn
 (regexp
  (regexp-quote "Assignment 'g = #f failed type checking: cannot assign type 'bool to a variable of type 'int"))
 (lambda () (type-check-statement (Assignment 'g false)
                                  (Environment '() (list (Type-Binding 'g 'int) (Type-Binding 'h 'int)) '() base-local-environment)
                                  'void)))
(check-exn
 (regexp
  (regexp-quote "Print requires an integer argument; given type 'bool"))
 (lambda () (type-check-statement (Print 'b false)
                                  (Environment '() (list (Type-Binding 'b 'bool) (Type-Binding 'h 'int)) '() base-local-environment)
                                  'void)))
(check-exn
 (regexp
  (regexp-quote "If statements require boolean guards; given expression 'i of type 'int"))
 (lambda () (type-check-statement (If
                                   'i
                                   (Block (list (Return 4))))
                                  (Environment '() (list (Type-Binding 'i 'int)) '() base-local-environment)
                                  'void)))
(check-exn
 (regexp
  (regexp-quote "If statements require boolean guards; given expression 'i of type 'int"))
 (lambda () (type-check-statement (If-Else
                                   'i
                                   (Block (list (Return 4)))
                                   (Block (list (Assignment 'i 5))))
                                  (Environment '() (list (Type-Binding 'i 'int)) '() base-local-environment)
                                  'void)))
(check-exn
 (regexp
  (regexp-quote "While loops require boolean guards; given expression (Selector (Selector (Selector 'a 'a) 'a) 'a) of type 'A"))
 (lambda () (type-check-statement (Loop
                                   (Selector (Selector (Selector 'a 'a) 'a) 'a)
                                   (Block (list (Return 4))))
                                  (Environment (list (Struct-Binding 'A (list (Type-Binding 'i 'int) (Type-Binding 'a 'A))))
                                               (list (Type-Binding 'a 'A))
                                               '()
                                               base-local-environment)
                                  'int)))
(check-exn
 (regexp
  (regexp-quote "Delete requires a structure type; given expression (Selector (Selector (Selector 'a 'a) 'a) 'i) of type 'int"))
 (lambda () (type-check-statement (Delete (Selector (Selector (Selector 'a 'a) 'a) 'i))
                                  (Environment (list (Struct-Binding 'A (list (Type-Binding 'i 'int) (Type-Binding 'a 'A))))
                                               '()
                                               '()
                                               (cons (Type-Binding 'a 'A) base-local-environment))
                                  'void)))
(check-exn
 (regexp
  (regexp-quote "Return expression 'a failed type check; given type 'A, expected type 'int"))
 (lambda () (type-check-statement (Return 'a)
                                  (Environment (list (Struct-Binding 'A (list (Type-Binding 'i 'int))))
                                               '()
                                               '()
                                               (cons (Type-Binding 'a 'A) base-local-environment))
                                  'int)))
(check-exn
 (regexp
  (regexp-quote "Return statement failed type check; given type 'void, expected type 'int"))
 (lambda () (type-check-statement (Return-Void)
                                  (Environment '() '() '() '())
                                  'int)))
(check-exn
 (regexp
  (regexp-quote "Invocation of function 'f failed type check; given expressions '(i j) of types '(int int), expected types '(int bool)"))
 (lambda () (type-check-statement (Invocation 'f (list 'i 'j))
                                  (Environment '()
                                               '()
                                               (list (Function-Binding 'f (list 'int 'bool) 'int))
                                               (append (list (Type-Binding 'i 'int)
                                                             (Type-Binding 'j 'int))
                                                       base-local-environment))
                                  'int)))

;; TEST CASES for type-check-expression
(check-equal? (type-check-expression (Binary '+ 5 3)
                                     (Environment '() '() base-function-environment '()))
              'int)
(check-equal? (type-check-expression (Binary '> 5 3)
                                     (Environment '() '() base-function-environment '()))
              'bool)
(check-equal? (type-check-expression (Binary '&& true false)
                                     (Environment '() '() base-function-environment base-local-environment))
              'bool)
(check-equal? (type-check-expression (Binary '* (Binary '/ (Selector 'a 'i) 'j) (Invocation 'f (list 'j)))
                                     (Environment (list (Struct-Binding 'A (list (Type-Binding 'i 'int))))
                                                  (list (Type-Binding 'j 'int) (Type-Binding 'a 'A))
                                                  (cons (Function-Binding 'f (list 'int) 'int) base-function-environment)
                                                  base-local-environment))
              'int)
(check-equal? (type-check-expression (Unary '- 5)
                                     (Environment '() '() base-function-environment '()))
              'int)
(check-equal? (type-check-expression (Unary '! 'b)
                                     (Environment '()
                                                  (list (Type-Binding 'b 'bool))
                                                  base-function-environment
                                                  '()))
              'bool)
(check-equal? (type-check-expression (Selector 'a 'i)
                                     (Environment (list (Struct-Binding 'A (list (Type-Binding 'i 'int))))
                                                  (list (Type-Binding 'a 'A))
                                                  base-function-environment
                                                  '()))
              'int)
(check-equal? (type-check-expression (Invocation 'f (list 'i 'null))
                                     (Environment (list (Struct-Binding 'A (list (Type-Binding 'i 'int))))
                                                  '()
                                                  (list (Function-Binding 'f (list 'int 'A) 'int))
                                                  (append (list (Type-Binding 'i 'int)
                                                                (Type-Binding 'j 'int))
                                                          base-local-environment)))
              'int)
(check-equal? (type-check-expression (Allocation 'A)
                                     (Environment (list (Struct-Binding 'A (list (Type-Binding 'int 'i))))
                                                  '()
                                                  '()
                                                  '()))
              'A)
; (local declaration hides global declaration)
(check-equal? (type-check-expression (Binary '+ 4 'a)
                                     (Environment '()
                                                  (list (Type-Binding 'a 'bool))
                                                  base-function-environment
                                                  (list (Type-Binding 'a 'int))))
              'int)
(check-exn
 (regexp
  (regexp-quote "Equality operator '== requires operands of matching types (either 'int or structure); given expressions 5 of type 'int and 'a of type 'A"))
 (lambda () (type-check-expression (Binary '== 5 'a)
                                   (Environment (list (Struct-Binding 'A (list (Type-Binding 'i 'int))))
                                                (list (Type-Binding 'a 'A))
                                                base-function-environment
                                                base-local-environment))))
(check-exn
 (regexp
  (regexp-quote "Equality operator '!= requires operands of matching types (either 'int or structure); given expressions #t of type 'bool and #t of type 'bool"))
 (lambda () (type-check-expression (Binary '!= true true)
                                   (Environment '() '() base-function-environment base-local-environment))))
(check-exn
 (regexp
  (regexp-quote "Binary operator '+ requires operands of types '(int int); given expressions #t of type 'bool and 9 of type 'int"))
 (lambda () (type-check-expression (Binary '+ true 9)
                                   (Environment '() '() base-function-environment base-local-environment))))
(check-exn
 (regexp
  (regexp-quote "Unary operator '! requires one operand of type 'bool; given expression 100 of type 'int"))
 (lambda () (type-check-expression (Unary '! 100)
                                   (Environment '() '() base-function-environment '()))))
(check-exn
 (regexp
  (regexp-quote "Unary operator '- requires one operand of type 'int; given expression #f of type 'bool"))
 (lambda () (type-check-expression (Unary '- false)
                                   (Environment '() '() base-function-environment base-local-environment))))
(check-exn
 (regexp
  (regexp-quote "Structure 'A field access failed: \"lookup-type: Type check failed: type 'b has not been defined\""))
 (lambda () (type-check-expression (Selector 'a 'b)
                                   (Environment (list (Struct-Binding 'A (list (Type-Binding 'i 'int))))
                                                (list (Type-Binding 'a 'A))
                                                base-function-environment
                                                base-local-environment))))
(check-exn
 (regexp
  (regexp-quote "Type check failed: type 'a has not been defined"))
 (lambda () (type-check-expression (Selector 'a 'b)
                                   (Environment (list (Struct-Binding 'A (list (Type-Binding 'i 'int))))
                                                '()
                                                base-function-environment
                                                base-local-environment))))
(check-exn
 (regexp
  (regexp-quote "Field access requires a structure type; given expression 'i of type 'int"))
 (lambda () (type-check-expression (Selector 'i 'b)
                                   (Environment (list (Struct-Binding 'A (list (Type-Binding 'i 'int))))
                                                (list (Type-Binding 'i 'int))
                                                base-function-environment
                                                base-local-environment))))
(check-exn
 (regexp
  (regexp-quote "Invocation of function 'f failed type check; given expressions '(i j) of types '(int int), expected types '(int bool)"))
 (lambda () (type-check-expression (Invocation 'f (list 'i 'j))
                                   (Environment '()
                                                '()
                                                (list (Function-Binding 'f (list 'int 'bool) 'int))
                                                (append (list (Type-Binding 'i 'int)
                                                              (Type-Binding 'j 'int))
                                                        base-local-environment)))))
(check-exn
 (regexp
  (regexp-quote "Type check failed: struct 'B has not been defined"))
 (lambda () (type-check-expression (Allocation 'B)
                                   (Environment (list (Struct-Binding 'A (list (Type-Binding 'int 'i))))
                                                '()
                                                '()
                                                '()))))

;; TEST CASES for bind-structs
(check-equal? (bind-structs (list (Struct 'A (list (Declaration 'int 'i)
                                                   (Declaration 'int 'j1)
                                                   (Declaration 'bool 'b)
                                                   (Declaration 'A 'a)))
                                  (Struct 'B (list (Declaration 'A 'a))))
                            '())
              (list (Struct-Binding 'A (list (Type-Binding 'i 'int)
                                             (Type-Binding 'j1 'int)
                                             (Type-Binding 'b 'bool)
                                             (Type-Binding 'a 'A)))
                    (Struct-Binding 'B (list (Type-Binding 'a 'A)))))
; (structure recursion)
(check-equal? (bind-structs (list (Struct 'A (list (Declaration 'A 'a)))) '())
              (list (Struct-Binding 'A (list (Type-Binding 'a 'A)))))
(check-exn
 (regexp
  (regexp-quote "Identifier 'B is not defined or is out of scope"))
 (lambda () (bind-structs (list (Struct 'A (list (Declaration 'int 'i)
                                                 (Declaration 'int 'j1)
                                                 (Declaration 'bool 'b)
                                                 (Declaration 'B 'a))))
                          '())))
(check-exn
 (regexp
  (regexp-quote "Redeclaration of structure with identifier 'A is not allowed"))
 (lambda () (bind-structs (list (Struct 'A (list (Declaration 'int 'i)
                                                 (Declaration 'int 'j1)
                                                 (Declaration 'bool 'b)
                                                 (Declaration 'A 'a)))
                                (Struct 'A (list (Declaration 'int 'k))))
                          '())))

;; TEST CASES for bind-struct
(check-equal? (bind-struct (Struct 'A (list (Declaration 'int 'i)
                                            (Declaration 'int 'j1)
                                            (Declaration 'bool 'b)
                                            (Declaration 'A 'a)))
                           '())
              (Struct-Binding 'A (list (Type-Binding 'i 'int)
                                       (Type-Binding 'j1 'int)
                                       (Type-Binding 'b 'bool)
                                       (Type-Binding 'a 'A))))

;; TEST CASES for bind-declaration
(check-equal? (bind-declarations (list (Declaration 'int 'i)
                                       (Declaration 'int 'j1)
                                       (Declaration 'bool 'b)
                                       (Declaration 'A 'a))
                                 (list 'A))
              (list (Type-Binding 'i 'int)
                    (Type-Binding 'j1 'int)
                    (Type-Binding 'b 'bool)
                    (Type-Binding 'a 'A)))
(check-exn
 (regexp
  (regexp-quote "Redeclaration of variable with identifier 'a is not allowed"))
 (lambda () (bind-declarations (list (Declaration 'int 'i)
                                     (Declaration 'int 'j1)
                                     (Declaration 'bool 'a)
                                     (Declaration 'A 'a))
                               (list 'A))))

;; TEST CASES for bind-declaration
(check-equal? (bind-declaration (Declaration 'int 'i))
              (Type-Binding 'i 'int))

;; TEST CASES for first-id-unique?
(check-equal? (first-id-unique? 'a (list 'b 'c 'd)) #t)
(check-equal? (first-id-unique? 'a (list 'a 'c 'd)) #f)
(check-equal? (first-id-unique? 'c (list 'a 'c 'd)) #f)
(check-equal? (first-id-unique? 'c '()) #t)

;; TEST CASES for verify-types
(verify-types (list 'int 'bool 'A 'B) (list 'A 'B))
(check-exn (regexp (regexp-quote "Identifier 'B is not defined or is out of scope"))
           (lambda () (verify-types (list 'int 'bool 'A 'B) (list 'A))))

;; TEST CASES for verify-type
(verify-type 'int (list 'A 'B))
(verify-type 'bool (list 'A 'B))
(verify-type 'A (list 'A 'B))
(check-exn (regexp (regexp-quote "Identifier 'C is not defined or is out of scope"))
           (lambda () (verify-type 'C (list 'A 'B))))

;; TEST CASES for verify-struct-id
(verify-struct-id 'A (list 'A 'B))
(check-exn (regexp (regexp-quote "Identifier 'C is not defined or is out of scope"))
           (lambda () (verify-type 'C (list 'A 'B))))

;; TEST CASES for lookup-function
(check-equal? (lookup-function 'f (list (Function-Binding 'f (list 'int 'int) 'bool)
                                        (Function-Binding 'main '() 'int)))
              (Function-Binding 'f (list 'int 'int) 'bool))
(check-exn
 (regexp
  (regexp-quote "Type check failed: function 'g has not been defined"))
 (lambda () (lookup-function 'g (list (Function-Binding 'f (list 'int 'int) 'bool)
                                      (Function-Binding 'main '() 'int)))))

;; TEST CASES for lookup-struct
(check-equal? (lookup-struct 'a (list (Struct-Binding 'a (list (Type-Binding 'b 'int)))))
              (list (Type-Binding 'b 'int)))
(check-equal? (lookup-struct 'a (list (Struct-Binding 'x (list (Type-Binding 'c 'bool)
                                                               (Type-Binding 'd 'a)))
                                      (Struct-Binding 'a (list (Type-Binding 'b 'int)))))
              (list (Type-Binding 'b 'int)))
(check-exn
 (regexp
  (regexp-quote "Type check failed: struct 'b has not been defined"))
 (lambda () (lookup-struct 'b (list (Struct-Binding 'a '())))))

;; TEST CASES for lookup-type
(check-equal? (lookup-type 'a (list (Type-Binding 'a 'int))) 'int)
(check-equal? (lookup-type 'x (list (Type-Binding 'a 'int)
                                    (Type-Binding 'x 'bool)))
              'bool)
(check-exn
 (regexp
  (regexp-quote "Type check failed: type 'a has not been defined"))
 (lambda () (lookup-type 'a (list (Type-Binding 'b 'int)))))
(check-exn
 (regexp
  (regexp-quote "Type check failed: type 's has not been defined"))
 (lambda () (lookup-type 's '())))

;; TEST CASES for valid-arguments?
(check-equal? (valid-arguments? (list 'int 'int) (list 'int 'int)) #t)
(check-equal? (valid-arguments? (list 'int 'bool) (list 'int 'int)) #f)
(check-equal? (valid-arguments? (list 'int 'int 'int) (list 'int 'int)) #f)
(check-equal? (valid-arguments? (list 'int 'a) (list 'int 'null)) #t)
(check-equal? (valid-arguments? '() '()) #t)
(check-equal? (valid-arguments? '() (list 'int)) #f)

;; TEST CASES for structure-type?
(check-equal? (structure-type? 'int) #f)
(check-equal? (structure-type? 'bool) #f)
(check-equal? (structure-type? 'a) #t)