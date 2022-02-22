#lang typed/racket

(require typed/rackunit)
(require typed/json)
(require racket/block)
(provide (all-defined-out))

;; RACKET MODULE FOR PARSING

;; a separate untyped racket module to handle the json AST created by the given incomplete parser
;; in a different module since typed/racket does not handle json data well
(module json-parse racket
  (provide parse-str)
  (provide parse)
  (provide print-json)
  (require json)
  (require "definitions.rkt")

  ;; takes the json input as a string and converts it into a Racket JSExpr to be parsed
  (define (parse-str json-str)
    (parse (string->jsexpr json-str)))

  (define (print-json json-str)
    (string->jsexpr json-str))

  ;; the parser - takes as input a Racket JSExpr and produces a Program
  (define (parse json-expr)
    (match json-expr
      ['() '()]
      [(hash-table ('exp "true")) #t]
      [(hash-table ('exp "false")) #f]
      [(hash-table ('exp "null")) 'null]
      [(hash-table ('exp "read")) 'read]
      [(hash-table ('exp "num") ('value value))
       (string->number value)]
      [(hash-table ('exp "id") ('id id)) (string->symbol id)]
      [(hash-table ('exp "new") ('id id))
       (Allocation (string->symbol id))]
      [(hash-table ('exp "dot") ('left left) ('id id))
       (Selector (parse left) (string->symbol id))]
      [(hash-table ('exp "binary") ('operator operator) ('lft left) ('rht right))
       (Binary (string->symbol operator) (parse left) (parse right))]
      [(hash-table ('exp "unary") ('operator operator) ('operand operand))
       (Unary (string->symbol operator) (parse operand))]
      [(hash-table ('types types) ('declarations declarations) ('functions functions))
       (Program (parse types) (parse declarations) (parse functions))]
      [(hash-table ('type type) ('id id))
       (Declaration (string->symbol type) (string->symbol id))]
      [(hash-table ('id id) ('fields fields))
       (Struct (string->symbol id) (parse fields))]
      [(hash-table ('id id) ('parameters parameters) ('return_type return_type) ('declarations declarations) ('body body))
       (Function (string->symbol id) (parse parameters) (string->symbol return_type) (parse declarations) (parse body))]
      [(hash-table ('stmt "return") ('exp expression))
       (Return (parse expression))]
      [(hash-table ('stmt "return"))
       (Return-Void)]
      [(hash-table ('stmt "if") ('guard guard) ('then then) ('else else))
       (If-Else (parse guard) (parse then) (parse else))]
      [(hash-table ('stmt "if") ('guard guard) ('then then))
       (If (parse guard) (parse then))]
      [(hash-table ('stmt "block") ('list body))
       (Block-Expr (parse body))]
      [(hash-table ('stmt "print") ('exp expression) ('endl endl))
       (Print (parse expression) endl)]
      [(hash-table ('stmt "assign") ('source source) ('target target))
       (Assignment (parse target) (parse source))]
      [(hash-table ('stmt "while") ('guard guard) ('body body))
       (Loop (parse guard) (parse body))]
      [(hash-table ('stmt "invocation") ('id id) ('args args))
       (Invocation (string->symbol id) (parse args))]
      [(hash-table ('exp "invocation") ('id id) ('args args))
       (Invocation (string->symbol id) (parse args))]
      [(hash-table ('stmt "delete") ('exp expression))
       (Delete (parse expression))]
      [(hash-table ('left left) ('id id))
       (Selector (parse left) (string->symbol id))]
      [(hash-table ('id id)) (string->symbol id)]
      [(cons first rest)
       (cons (parse first) (parse rest))])))

(require "definitions.rkt")
(require/typed 'json-parse
               [parse-str (String -> Program)]
               [parse (String -> Program)]
               [print-json (String -> JSExpr)])
(provide parse)

;; MILESTONE 1 FUNCTIONS

;; the compiler (takes in the name of the json file)
(define (compile [json-file : String]) : String
  (define AST (parse-str (port->string (open-input-file json-file))))
  (define environment (type-check-program AST))
  (check-valid-main (Environment-functions environment))
  (define LLVM-Program (create-LLVM (Program-functions AST) environment))
  (define ARM-Program (LLVM-Program->ARM-Program LLVM-Program environment))
  (ARM-Program->string ARM-Program))

;; the compiler (takes in the json representation of the program) - this is the method that ./compile uses
(define (compile-json [json-str : String] [compiler-mode : String] [output-type : String] [source-filename : String]) : String
  (set-compiler-mode! compiler-mode)
  (define AST (parse-str json-str))
  (define environment (type-check-program AST))
  (define LLVM-Program (create-LLVM (Program-functions AST) environment))
  (cond
    [(equal? output-type "LLVM") (LLVM-Program->string LLVM-Program source-filename)]
    [(equal? output-type "assembly") (ARM-Program->string (LLVM-Program->ARM-Program LLVM-Program environment))]
    [else (error 'compile-json "Unsupported output type: ~e")]))

;; the compiler (takes in the name of the json file) - kept for unit tests
(define (test-compiler [json-file : String]) : LLVM-Program
  (define AST (parse-str (port->string (open-input-file json-file))))
  (define environment (type-check-program AST))
  (check-valid-main (Environment-functions environment))
  (create-LLVM (Program-functions AST) environment))

;; the front end of the compiler for testing (takes in the json representation of the program)
(define (front-end-json [json-str : String]) : Program
  (define AST (parse-str json-str))
  (define environment (type-check-program AST))
  AST)

;; generate the visual representation of the CFG
(define (visualize-CFG [json-str : String] [input-filename : String]) : Void
  (define AST (parse-str json-str))
  (define environment (type-check-program AST))
  (define LLVM-Program (create-LLVM (Program-functions AST) environment))
  (generate-graph LLVM-Program input-filename))

;; type-checks the program
(define (type-check-program [program : Program]) : Environment
  (match program
    [(Program types declarations functions)
     (define environment (Environment (ann (make-hash '()) Struct-Environment)
                                      (ann (make-hash '()) Variable-Environment)
                                      (base-function-env)))
     (bind-structs! types (Environment-structs environment))
     (bind-declarations! declarations (Environment-structs environment) (Environment-globals environment))
     (map (lambda ([function : Function]) (type-check-function function environment)) functions)
     (check-valid-main (Environment-functions environment))
     environment]))

;; checks for a valid main function
(define (check-valid-main [functions : Function-Environment]) : Void
  (if (hash-has-key? functions 'main)
      (let ([main-function : Function-Type (hash-ref functions 'main)])
        (cond [(not (and (equal? (Function-Type-args main-function) '())
                         (equal? (Function-Type-return-type main-function) 'int)))
               (error 'check-valid-main "'main has an invalid signature, expected '() -> 'int, given ~e -> ~e"
                      (Function-Type-args main-function) (Function-Type-return-type main-function))]))
      (error 'check-valid-main "No main function defined; every valid program must have a function named 'main that takes no arguments and returns an 'int")))

;; type checks the program's functions
(define (type-check-function [function : Function] [env : Environment]) : Void
  (define function-type (bind-function function (Environment-structs env)))
  (if (hash-has-key? (Environment-functions env) (Function-id function))
      (error 'type-check-function "Redeclaration of function ~e is not allowed." (Function-id function))
      (hash-set! (Environment-functions env) (Function-id function) function-type))
  (type-check-statements (Function-statements function) env (Function-return-type function) function-type)
  (cond [(and (not (equal? (Function-return-type function) 'void))
              (not (all-paths-return? (Function-statements function))))
         (error 'type-check-function "All paths through function ~e must return with type ~e"
                (Function-id function) (Function-return-type function))]))

;; do all paths through the function return a value? assumes the function has a non-void return type
(define (all-paths-return? [statements : (Listof Statement)]) : Boolean
  (if (empty? statements)
      #f
      (match (first statements)
        [(Block-Expr statement-list) (all-paths-return? (append statement-list (rest statements)))]
        [(Assignment target source) (all-paths-return? (rest statements))]
        [(Print expression endl) (all-paths-return? (rest statements))]
        [(If guard then-block) (and (all-paths-return? (append (Block-Expr-statements then-block) (rest statements)))
                                  (all-paths-return? (rest statements)))]
        [(If-Else guard then-block else-block) (and (all-paths-return? (append (Block-Expr-statements then-block) (rest statements)))
                                                  (all-paths-return? (append (Block-Expr-statements else-block) (rest statements))))]
        [(Loop guard body) (and (all-paths-return? (append (Block-Expr-statements body) (rest statements)))
                                (all-paths-return? (rest statements)))]
        [(Delete expression) (all-paths-return? (rest statements))]
        [(Return expression) #t]
        [(Return-Void) #f]
        [(Invocation id arguments) (all-paths-return? (rest statements))])))
    
;; binds a function name to a list of argument types and a return type
(define (bind-function [function : Function] [structs : Struct-Environment]) : Function-Type
  (match function
    [(Function name parameters return-type declarations statements)
     (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'bind-function "Function ~e's parameters/locals failed type check : ~e"
                                                             name (exn-message exn)))])
       (define locals (base-local-env))
       (bind-declarations! (append parameters declarations) structs locals)
       (Function-Type (map Declaration-type parameters) return-type locals))]))

;; type checks a list of statements
(define (type-check-statements [statements : (Listof Statement)] [env : Environment] [expected-return-type : Return-Type] [func-type : Function-Type]) : Void
  (cond [(not (empty? statements))
         (type-check-statement (first statements) env expected-return-type func-type)
         (type-check-statements (rest statements) env expected-return-type func-type)]))

;; type checks a single statement
(define (type-check-statement [statement : Statement] [env : Environment] [expected-return-type : Return-Type] [func-type : Function-Type]) : Void
  (match statement 
    [(Block-Expr statements)
     (type-check-statements statements env expected-return-type func-type)]
    [(Assignment target source)
     (let ([source-type : Type (type-check-expression source env func-type)]
           [target-type : Type (type-check-expression target env func-type)])
       (cond [(not (or (equal? source-type target-type)
                       (and (equal? source-type 'null) (structure-type? target-type))))
              (error 'type-check-statement "Assignment ~e = ~e failed type checking: cannot assign type ~e to a variable of type ~e"
                     target source source-type target-type)]))]
    [(Print expression endl) 
     (let ([expression-type : Type (type-check-expression expression env func-type)])
       (cond [(not (equal? expression-type 'int))
              (error 'type-check-statement "Print requires an integer argument; given type ~e" expression-type)]))]
    [(If guard then-block)
     (let ([guard-type : Type (type-check-expression guard env func-type)])
       (cond [(not (equal? guard-type 'bool))
              (error 'type-check-statement "If statements require boolean guards; given expression ~e of type ~e" guard guard-type)])
       (type-check-statements (Block-Expr-statements then-block) env expected-return-type func-type))]
    [(If-Else guard then-block else-block)
     (let ([guard-type : Type (type-check-expression guard env func-type)])
       (cond [(not (equal? guard-type 'bool))
              (error 'type-check-statement "If statements require boolean guards; given expression ~e of type ~e" guard guard-type)])
       (type-check-statements (Block-Expr-statements then-block) env expected-return-type func-type)
       (type-check-statements (Block-Expr-statements else-block) env expected-return-type func-type))]
    [(Loop guard body)
     (let ([guard-type : Type (type-check-expression guard env func-type)])
       (cond [(not (equal? guard-type 'bool))
              (error 'type-check-statement "While loops require boolean guards; given expression ~e of type ~e" guard guard-type)])
       (type-check-statements (Block-Expr-statements body) env expected-return-type func-type))]
    [(Delete expression)
     (let ([expression-type : Type (type-check-expression expression env func-type)])
       (cond [(not (structure-type? expression-type))
              (error 'type-check-statement "Delete requires a structure type; given expression ~e of type ~e" expression expression-type)]))]
    [(Return expression) 
     (let ([actual-return-type : Type (type-check-expression expression env func-type)])
       (cond [(not (equal? actual-return-type expected-return-type))
              (error 'type-check-statement "Return expression ~e failed type check; given type ~e, expected type ~e"
                     expression actual-return-type expected-return-type)]))]
    [(Return-Void) (cond [(not (equal? expected-return-type 'void))
                          (error 'type-check-statement "Return statement failed type check; given type 'void, expected type ~e" expected-return-type)])]
    [(Invocation id arguments)
     (let ([function-type : Function-Type (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'type-check-statement "Function ~e is not defined: ~e" id))])
                                               (hash-ref (Environment-functions env) id))]
           [actual-arg-types : (Listof Expression-Type) (map (lambda ([arg : Expression]) (type-check-expression arg env func-type)) arguments)])
       (cond [(not (valid-arguments? (Function-Type-args function-type) actual-arg-types))
              (error 'type-check-statement "Invocation of function ~e failed type check; given expressions ~e of types ~e, expected types ~e"
                     id arguments actual-arg-types (Function-Type-args function-type))]))]))

;; type checks an expression
(define (type-check-expression [expression : Expression] [env : Environment] [func-type : Function-Type]) : Expression-Type
  (match expression
    [(Binary operator left right)
     (let ([left-type : Type (type-check-expression left env func-type)]
           [right-type : Type (type-check-expression right env func-type)])
       (cond
         [(or (equal? operator '==) (equal? operator '!=))
          (if (and (equal? left-type right-type) (not (equal? left-type 'bool)))
              'bool
              (error 'type-check-expression "Equality operator ~e requires operands of matching types (either 'int or structure); given expressions ~e of type ~e and ~e of type ~e"
                     operator left left-type right right-type))]
         [else (let ([function-type : Function-Type (hash-ref (Environment-functions env) operator)])
                 (cond [(equal? (list left-type right-type) (Function-Type-args function-type))
                        (Function-Type-return-type function-type)]
                       [else (error 'type-check-expression "Binary operator ~e requires operands of types ~e; given expressions ~e of type ~e and ~e of type ~e"
                                    operator (Function-Type-args function-type) left left-type right right-type)]))]))]
    [(Unary operator operand)
     (let ([operand-type : Type (type-check-expression operand env func-type)])
       (cond
         [(equal? operator '!)
          (if (equal? operand-type 'bool)
              'bool
              (error 'type-check-expression "Unary operator '! requires one operand of type 'bool; given expression ~e of type ~e"
                     operand operand-type))]
         [(equal? operator '-)
          (if (equal? operand-type 'int)
              'int
              (error 'type-check-expression "Unary operator '- requires one operand of type 'int; given expression ~e of type ~e"
                     operand operand-type))]))]
    [(Selector left symbol) 
     (let ([left-type : Type (type-check-expression left env func-type)])
       (if (structure-type? left-type)
           (let ([fields : Field-Environment (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'type-check-expression "Structure type ~e is not defined." left-type))])
                                               (Struct-Type-fields (hash-ref (Environment-structs env) left-type)))])
             (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'type-check-expression "Structure ~e does not have field ~e" left-type symbol))])
                                                    (Field-type (hash-ref fields symbol))))
           (error 'type-check-expression "Field access requires a structure type; given expression ~e of type ~e" left left-type)))]
    [(Invocation id arguments)
     (let ([function-type : Function-Type (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'type-check-expression "Function ~e is not defined." id))])
                                            (hash-ref (Environment-functions env) id))]
           [actual-arg-types : (Listof Expression-Type) (map (lambda ([arg : Expression]) (type-check-expression arg env func-type)) arguments)])
       (if (valid-arguments? (Function-Type-args function-type) actual-arg-types)
           (Function-Type-return-type function-type)
           (error 'type-check-expression "Invocation of function ~e failed type check; given expressions ~e of types ~e, expected types ~e"
                     id arguments actual-arg-types (Function-Type-args function-type))))]
    [(Allocation id)
     (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'type-check-expression "Allocation failed; structure type ~e does not exist." id))])
       (hash-ref (Environment-structs env) id))
     id]
    [literal
     (cond
       [(equal? 'null literal) 'null]
       [(symbol? literal)
        (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'type-check-expression "Variable ~e is unbound." literal))])
          (hash-ref (Function-Type-locals func-type) literal (lambda () (hash-ref (Environment-globals env) literal))))]
       [(integer? literal) 'int]
       [(boolean? literal) 'bool]
       [else (error 'type-check-expression "Invalid expression type ~e" literal)])]))

;; binds a list of structs to their corresponding type environments
(define (bind-structs! [new-structs : (Listof Struct)] [existing-structs : Struct-Environment]) : Void
  (define struct-counter 0)
  (for-each (lambda ([struct : Struct])
              (if (hash-has-key? existing-structs (Struct-id struct))
                  (error 'bind-structs! "Redeclaration of structure with identifier ~e is not allowed" (Struct-id struct))
                  (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'bind-structs! "Structure ~e's fields failed type check: ~e"
                                                                          (Struct-id struct) (exn-message exn)))])
                    (define fields (ann (make-hash '()) Field-Environment))
                    (hash-set! existing-structs (Struct-id struct) (Struct-Type fields struct-counter))
                    (bind-fields! (Struct-fields struct) existing-structs fields)
                    (set! struct-counter (+ 1 struct-counter)))))
            new-structs))

;; binds a list of declarations to their corresponding types
(define (bind-fields! [declarations : (Listof Declaration)] [structs : Struct-Environment] [fields : Field-Environment]) : Void
  (define field-counter 0)
  (for-each (lambda ([dec : Declaration])
              (if (hash-has-key? fields (Declaration-id dec))
                  (error 'bind-declarations! "Redeclaration of structure field with identifier ~e is not allowed" (Declaration-id dec))
                  (cond [(and (structure-type? (Declaration-type dec)) (not (hash-has-key? structs (Declaration-type dec))))
                         (error 'bind-declarations! "Structure type ~e is not defined." (Declaration-type dec))]
                        [else (hash-set! fields (Declaration-id dec) (Field (Declaration-type dec) field-counter))
                              (set! field-counter (+ 1 field-counter))])))
            declarations))

;; binds a list of declarations to their corresponding types
(define (bind-declarations! [declarations : (Listof Declaration)] [structs : Struct-Environment] [var-env : Variable-Environment]) : Void
  (for-each (lambda ([dec : Declaration])
              (if (hash-has-key? var-env (Declaration-id dec))
                  (error 'bind-declarations! "Redeclaration of variable with identifier ~e is not allowed" (Declaration-id dec))
                  (cond [(and (structure-type? (Declaration-type dec)) (not (hash-has-key? structs (Declaration-type dec))))
                         (error 'bind-declarations! "Structure type ~e is not defined." (Declaration-type dec))]
                        [else (hash-set! var-env (Declaration-id dec) (Declaration-type dec))])))
            declarations))

;; are the supplied function arguments valid?
(define (valid-arguments? [expected-types : (Listof Type)] [actual-types : (Listof Expression-Type)]) : Boolean
  (cond
    [(and (empty? expected-types) (empty? actual-types)) #t]
    [(or (empty? expected-types) (empty? actual-types)) #f]
    [else (if (or (equal? (first expected-types) (first actual-types))
                  (and (structure-type? (first expected-types)) (equal? (first actual-types) 'null)))
              (valid-arguments? (rest expected-types) (rest actual-types))
              #f)]))

;; is the given type a structure type?
(define (structure-type? [type : Type]) : Boolean
  (and (not (equal? type 'int)) (not (equal? type 'bool))))

;; MILESTONE 2 FUNCTIONS

(define counter 1)
(define mode : Compiler-Mode 'registers)
(define c-lib-funcs (C-Lib-Funcs #f #f #f #f #f))
(define lib-funcs (Lib-Funcs c-lib-funcs #f))

;; creates the LLVM-Program object for the program
(define (create-LLVM [functions : (Listof Function)] [env : Environment]) : LLVM-Program
  (define struct-instrs (create-struct-instrs (Environment-structs env)))
  (define global-instrs (create-global-instrs (Environment-globals env)))
  (define func-CFGs (map (lambda ([function : Function]) (create-CFG function env)) functions))
  (LLVM-Program struct-instrs global-instrs func-CFGs c-lib-funcs))

;; creates a control flow graph (CFG) for a single function
(define (create-CFG [function : Function] [env : Environment]) : CFG
  (match function
    [(Function name parameters return-type declarations statements)
     (define entry-block (initialize-entry-block return-type parameters declarations))
     (define exit-block (Block -1 '() '() (make-hash '()) '() '() #f #f))
     (define blocks 
       (ann (make-hash
             (list (cons 'entry-block entry-block)
                   (cons 'exit-block exit-block)))
            Block-Table))
     (add-statements-LLVM! entry-block blocks statements env name)
     (map remove-trivial-phis! (map Block-LLVM (hash-values blocks)) (hash-values blocks))
     (cond [(not (empty? (Block-successors entry-block)))
            (set-Block-reachable! exit-block #t)
            (cleanup-exit-block exit-block return-type)
            (add-return-voids! entry-block exit-block (make-hash '()))]
           [(and (equal? return-type 'void)
                 (or (empty? (Block-LLVM entry-block))
                     (not (Return-Void-LLVM? (last (Block-LLVM entry-block))))))
            (add-LLVM! entry-block (Return-Void-LLVM))])
     (CFG name parameters return-type blocks (ann (make-hash '()) Stack-Table) (ann (make-hash '()) Data-Table) 0)]))

;; adds the LLVM for the given list of statements to the supplied block
;; updates the block hash table accordingly
(define (add-statements-LLVM! [cur-block : Block] [blocks : Block-Table] [statements : (Listof Statement)] [env : Environment] [func-name : Symbol]) : Block
  (if (empty? statements)
      cur-block
      (match (first statements)
        [(Block-Expr statement-list)
         (add-statements-LLVM! cur-block blocks (append statement-list (rest statements)) env func-name)]
        [(Assignment target source)
         (if (equal? source 'read)
             (add-read-LLVM! target cur-block env func-name)
             (add-assignment-LLVM! target source cur-block env func-name))
         (add-statements-LLVM! cur-block blocks (rest statements) env func-name)]
        [(Print expression endl)
         (define exp-operand (add-expression-LLVM! expression cur-block env func-name))
         (define target (Intermediate counter))
         (set! counter (+ 1 counter))
         (add-LLVM! cur-block (Print-LLVM exp-operand endl target))
         (if endl
             (set-C-Lib-Funcs-printf-newline! c-lib-funcs #t)
             (set-C-Lib-Funcs-printf-space! c-lib-funcs #t))
         (add-statements-LLVM! cur-block blocks (rest statements) env func-name)]
        [(If guard then)
         (define guard-operand (add-expression-LLVM! guard cur-block env func-name))
         (define then-block (create-block (list cur-block) #t blocks))
         (define last-then-block (add-statements-LLVM! then-block blocks (list then) env func-name))
         (define after-block (create-block (list cur-block) #t blocks))
         (add-branch! last-then-block after-block)
         (add-LLVM! cur-block (Cond-Branch-LLVM guard-operand then-block after-block))
         (add-statements-LLVM! after-block blocks (rest statements) env func-name)]
        [(If-Else guard then else)
         (define guard-operand (add-expression-LLVM! guard cur-block env func-name))
         (define then-block (create-block (list cur-block) #t blocks))
         (define last-then-block (add-statements-LLVM! then-block blocks (list then) env func-name))
         (define else-block (create-block (list cur-block) #t blocks))
         (define last-else-block (add-statements-LLVM! else-block blocks (list else) env func-name))
         (add-LLVM! cur-block (Cond-Branch-LLVM guard-operand then-block else-block))
         (define after-block (create-block (list last-then-block last-else-block) #t blocks))
         (add-branch! last-then-block after-block)
         (add-branch! last-else-block after-block)
         (add-statements-LLVM! after-block blocks (rest statements) env func-name)]
        [(Loop guard body)
         (define guard-operand (add-expression-LLVM! guard cur-block env func-name))
         (define body-block (create-block (list cur-block) #f blocks))
         (define last-body-block (add-statements-LLVM! body-block blocks (list body) env func-name))
         (define new-guard-operand (add-expression-LLVM! guard last-body-block env func-name))
         (define after-block (create-block (list cur-block last-body-block) #t blocks))
         (add-LLVM! cur-block (Cond-Branch-LLVM guard-operand body-block after-block))
         (set-Block-successors! cur-block (list body-block after-block))
         (add-loop-branch! last-body-block new-guard-operand body-block after-block cur-block)
         (cond
           [(equal? mode 'registers) (complete-phis! (Block-LLVM body-block) body-block)
                                     (set-Block-sealed! body-block #t)])
         (add-statements-LLVM! after-block blocks (rest statements) env func-name)]
        [(Delete expression)
         (define exp-operand (add-expression-LLVM! expression cur-block env func-name))
         (add-LLVMs! cur-block (list (Size-LLVM 'bitcast
                                                 exp-operand
                                                 (string-append "%struct." (symbol->string (type-check-expression expression env (hash-ref (Environment-functions env) func-name))) "*")
                                                 "i8*"
                                                 (Intermediate counter))
                                     (Free-LLVM (Intermediate counter))))
         (set! counter (+ 1 counter))
         (set-C-Lib-Funcs-free! c-lib-funcs #t)
         (add-statements-LLVM! cur-block blocks (rest statements) env func-name)]
        [(Return expression)
         (define result (add-expression-LLVM! expression cur-block env func-name))
         (define return-type (Function-Type-return-type (hash-ref (Environment-functions env) func-name)))
         (define exit-block (hash-ref blocks 'exit-block))
         (if (equal? (Block-label cur-block) (Block-label (hash-ref blocks 'entry-block)))
             (add-LLVM! cur-block (Return-Expr-LLVM return-type result))
             (block
              (cond
                [(equal? mode 'stack) (add-LLVM! cur-block (Store-LLVM return-type result (Local '.return) (if (structure-type? return-type) 1 0)))]
                [(equal? mode 'registers) (write-variable! 'return result cur-block)])
              (add-LLVM! cur-block (Branch-LLVM exit-block))
              (set-Block-successors! cur-block (list exit-block))
              (set-Block-predecessors! exit-block (cons cur-block (Block-predecessors exit-block)))))
         cur-block]
        [(Return-Void)
         (define exit-block (hash-ref blocks 'exit-block))
         (if (equal? (Block-label cur-block) (Block-label (hash-ref blocks 'entry-block)))
             (add-LLVM! cur-block (Return-Void-LLVM))
             (block
              (add-LLVM! cur-block (Branch-LLVM exit-block))
              (set-Block-successors! cur-block (list exit-block))
              (set-Block-predecessors! exit-block (cons cur-block (Block-predecessors exit-block)))))
         cur-block]
        [(Invocation id arguments)
         (define args (map (lambda ([arg : Expression]) (add-expression-LLVM! arg cur-block env func-name)) arguments))
         (define function (hash-ref (Environment-functions env) id))
         (define return-type (Function-Type-return-type function))
         (if (equal? 'void return-type)
             (add-LLVM! cur-block (Call-Void-LLVM (Global id) (Function-Type-args function) args))
             (add-LLVM! cur-block (Call-LLVM (Global id) return-type (Function-Type-args function) args (get-target))))
         (add-statements-LLVM! cur-block blocks (rest statements) env func-name)])))

;; adds the LLVM for the given expression to the supplied block
;; returns the operand that holds the result of this expression
(define (add-expression-LLVM! [expression : Expression] [cur-block : Block] [env : Environment] [func-name : Symbol]) : Operand
  (match expression
    [(Binary operator left right)
     (define left-result (add-expression-LLVM! left cur-block env func-name))
     (define right-result (add-expression-LLVM! right cur-block env func-name))
     (define target (get-target))
     (cond [(member operator (list '+ '- '* '/))
            (add-LLVM! cur-block (Arith-LLVM operator left-result right-result target))]
           [(member operator (list '|| '&& '^^))
            (add-LLVM! cur-block (Bool-LLVM operator left-result right-result target))]
           [(member operator (list '== '!= '< '> '<= '>=))
            (define type (if (or (equal? operator '==) (equal? operator '!=))
                             (type-check-expression left env (hash-ref (Environment-functions env) func-name))
                             'int))
            (add-LLVM! cur-block (Comp-LLVM operator type left-result right-result target))])
     target]
    [(Unary operator expression)
     (define result (add-expression-LLVM! expression cur-block env func-name))
     (define target (get-target))
     (cond [(equal? operator '-) (add-LLVM! cur-block (Arith-LLVM '- 0 result target))]
           [(equal? operator '!) (add-LLVM! cur-block (Bool-LLVM '^^ result #t target))])
     target]
    [(Selector left id)
     (define pointer (add-selector-LLVM! expression cur-block env func-name))
     (define type (type-check-expression left env (hash-ref (Environment-functions env) func-name)))
     (define next-target (get-target))
     (define field (hash-ref (Struct-Type-fields (hash-ref (Environment-structs env) type)) id))
     (add-LLVM! cur-block (Load-LLVM (Field-type field) pointer next-target (if (structure-type? (Field-type field)) 1 0)))
     next-target]
    [(Invocation id arguments)
     (define args (map (lambda ([arg : Expression]) (add-expression-LLVM! arg cur-block env func-name)) arguments))
     (define target (get-target))
     (define function (hash-ref (Environment-functions env) id))
     (add-LLVM! cur-block (Call-LLVM (Global id) (Function-Type-return-type function) (Function-Type-args function) args target))
     target]
    [(Allocation id)
     (define first-target (get-target))
     (define second-target (get-target))
     (set-C-Lib-Funcs-malloc! c-lib-funcs #t)
     (add-LLVM! cur-block (Malloc-LLVM first-target (get-type-size id env)))
     (add-LLVM! cur-block (Size-LLVM 'bitcast first-target "i8*" (string-append "%struct." (symbol->string id) "*") second-target))
     second-target]
    [literal
     (cond
       [(equal? 'null literal) 'null]
       [(symbol? literal)
        (define declaration (get-declaration literal (Function-Type-locals (hash-ref (Environment-functions env) func-name)) (Environment-globals env)))
        (define type (LLVM-Declaration-type declaration))
        (cond
          [(equal? mode 'stack)
           (define target (get-target))
           (add-LLVM! cur-block (Load-LLVM type (LLVM-Declaration-operand declaration) target (if (structure-type? type) 1 0)))
           target]
          [(equal? mode 'registers) (Phi-Value-operand (read-variable literal type cur-block))]
          [else (error 'add-expression-LLVM! "Unsupported compiler mode, given ~e" mode)])]
       [(integer? literal) literal]
       [(boolean? literal) literal]
       [else (error 'add-expression-LLVM! "Invalid expression type ~e" literal)])]))

;; adds the LLVM for a read instruction to the current block
(define (add-read-LLVM! [target : Expression] [cur-block : Block] [env : Environment] [func-name : Symbol]) : Void
  (set-C-Lib-Funcs-scanf! c-lib-funcs #t)
  (cond
    [(equal? mode 'stack)
     (define target-operand
       (if (symbol? target)
           (if (hash-has-key? (Function-Type-locals (hash-ref (Environment-functions env) func-name)) target)
               (Local target)
               (Global target)) 
           (add-selector-LLVM! target cur-block env func-name)))
     (add-LLVM! cur-block (Scan-LLVM target-operand (get-target)))]
    [(equal? mode 'registers)
     (cond
       [(symbol? target)
        (cond
          [(hash-has-key? (Function-Type-locals (hash-ref (Environment-functions env) func-name)) target)
           (define first-target (get-target))
           (define second-target (get-target))
           (define third-target (get-target))
           (add-LLVMs! cur-block (list (Alloca-LLVM 'int first-target)
                                       (Scan-LLVM first-target second-target)
                                       (Load-LLVM 'int first-target third-target 0)))
           (write-variable! target third-target cur-block)]
          [else (add-LLVM! cur-block (Scan-LLVM (Global target) (get-target)))])]
       [else (add-LLVM! cur-block (Scan-LLVM (add-selector-LLVM! target cur-block env func-name) (get-target)))])]
    [else (error 'add-read-LLVM! "Unsupported compiler mode: ~e" mode)]))

;; adds the LLVM for an assignment instruction to the current block
(define (add-assignment-LLVM! [target : Expression] [source : Expression] [cur-block : Block] [env : Environment] [func-name : Symbol]) : Void
  (cond
    [(equal? mode 'stack)
     (define target-operand
       (if (symbol? target)
           (if (hash-has-key? (Function-Type-locals (hash-ref (Environment-functions env) func-name)) target)
               (Local target)
               (Global target))
           (add-selector-LLVM! target cur-block env func-name)))
     (define source-operand (add-expression-LLVM! source cur-block env func-name))
     (define type (type-check-expression target env (hash-ref (Environment-functions env) func-name)))
     (add-LLVM! cur-block (Store-LLVM type source-operand target-operand (if (structure-type? type) 1 0)))]
    [(equal? mode 'registers)
     (define source-operand (add-expression-LLVM! source cur-block env func-name))
     (if (symbol? target)
         (if (hash-has-key? (Function-Type-locals (hash-ref (Environment-functions env) func-name)) target)
             (write-variable! target source-operand cur-block)
             (block
              (define type (type-check-expression target env (hash-ref (Environment-functions env) func-name)))
              (add-LLVM! cur-block (Store-LLVM type source-operand (Global target) (if (structure-type? type) 1 0)))))
         (block
          (define target-operand (add-selector-LLVM! target cur-block env func-name))
          (define type (type-check-expression target env (hash-ref (Environment-functions env) func-name)))
          (add-LLVM! cur-block (Store-LLVM type source-operand target-operand (if (structure-type? type) 1 0)))))]
    [else (error 'add-assignment-LLVM! "Unsupported compiler mode: ~e" mode)]))

;; adds the LLVM for a selector to the current block - simply gets the address of the desired field
;; this method is necessary since there is different behavior for selectors as right vs left values of assignments (need value vs address respectively)
(define (add-selector-LLVM! [target : Expression] [cur-block : Block] [env : Environment] [func-name : Symbol]) : Operand
  (match target
    [(Selector left id)
     (define type (type-check-expression left env (hash-ref (Environment-functions env) func-name)))
     (define pointer (if (symbol? left)            
                         (if (hash-has-key? (Function-Type-locals (hash-ref (Environment-functions env) func-name)) left)
                             (block
                              (define target (get-target))
                              (add-LLVM! cur-block (Load-LLVM type (Local left) target 1))
                              target)
                             (Global left))
                         (add-expression-LLVM! left cur-block env func-name)))
     (define first-target (get-target))
     (define field (hash-ref (Struct-Type-fields (hash-ref (Environment-structs env) type)) id))
     (add-LLVM! cur-block (Get-Elm-Ptr-LLVM type pointer (Field-index field) first-target))
     first-target]
    [else (error 'add-selector-LLVM! "Unsupported behavior - ~e is an invalid left expression of an assignment operation." target)]))

;; complete the phis of the given block in the process of sealing it
(define (complete-phis! [instructions : (Listof LLVM-Instr)] [cur-block : Block]) : Void     
  (cond
    [(and (not (empty? instructions)) (Phi-LLVM? (first instructions)))
     (match (first instructions)
       [(Phi-LLVM var type values target complete)
        (cond [(not complete) (add-phi-operands! (first instructions) cur-block (Block-predecessors cur-block))
                              (set-Phi-LLVM-complete! (first instructions) #t)])
        (complete-phis! (rest instructions) cur-block)])]))

;; add operands to the phi instruction by finding the value of the variable from each of the block's predecessors
(define (add-phi-operands! [phi : Phi-LLVM] [cur-block : Block] [predecessors : (Listof Block)]) : Void
  (cond
    [(not (empty? predecessors))
     (set-Phi-LLVM-values! phi (append (Phi-LLVM-values phi) (list (read-variable (Phi-LLVM-var phi) (Phi-LLVM-type phi) (first predecessors)))))
     (add-phi-operands! phi cur-block (rest predecessors))]))

;; get the value of the variable in the current block (may need to look at its predecessors and add phi nodes)
(define (read-variable [var : Symbol] [type : Type] [cur-block : Block]) : Phi-Value
  (match cur-block
    [(Block _ _ _ definitions predecessors _ _ sealed)
     (cond
       [(hash-has-key? definitions var) (Phi-Value (hash-ref definitions var) (Block-label cur-block))]
       [(not sealed) (define target (Register counter))
                     (set! counter (+ 1 counter))
                     (set-Block-LLVM! cur-block (cons (Phi-LLVM var type '() target #f) (Block-LLVM cur-block)))
                     (write-variable! var target cur-block)
                     (Phi-Value target (Block-label cur-block))]
       [(empty? predecessors) (error 'read-variable "Variable with identifier ~e has not been initialized in block ~e" var cur-block)]
       [(equal? (length predecessors) 1) (define value (read-variable var type (first predecessors)))
                                         (write-variable! var (Phi-Value-operand value) cur-block)
                                         value]
       [else (define target (Register counter))
             (set! counter (+ 1 counter))
             (define phi (Phi-LLVM var type '() target #t))
             (set-Block-LLVM! cur-block (cons phi (Block-LLVM cur-block)))
             (write-variable! var target cur-block)
             (add-phi-operands! phi cur-block predecessors)
             (Phi-Value target (Block-label cur-block))])]))

;; add the variable to the list of definitions - if it is already defined, redefine it
(define (write-variable! [var : Symbol] [value : Operand] [cur-block : Block]) : Void
  (hash-set! (Block-definitions cur-block) var value))

;; remove trivial phis - any which have the same operand from every predecessor
(define (remove-trivial-phis! [LLVMs : (Listof LLVM-Instr)] [cur-block : Block]) : Void
  (cond
    [(and (not (empty? LLVMs)) (Phi-LLVM? (first LLVMs)))
     (define phi (first LLVMs))
     (cond
       [(trivial-phi? (map Phi-Value-operand (Phi-LLVM-values (first LLVMs))))
        (set-Block-LLVM! cur-block (remove phi (Block-LLVM cur-block)))
        (set-Block-LLVM! cur-block (cons (Move-LLVM (Phi-LLVM-target phi) (Phi-Value-operand (first (Phi-LLVM-values phi))))
                                         (Block-LLVM cur-block)))])
     (remove-trivial-phis! (rest LLVMs) cur-block)]))

;; check if a phi node is trivial - are all of its operands the same?
(define (trivial-phi? [operands : (Listof Operand)]) : Boolean
  (if (empty? operands)
      #t
      (if (first-operand-matches-rest? (first operands) (rest operands))
          (trivial-phi? (rest operands))
          #f)))

;; get the size of a type in bits
(define (get-type-size [type : Type] [env : Environment]) : Integer
  (if (structure-type? type)
      (get-struct-size (hash->list (Struct-Type-fields (hash-ref (Environment-structs env) type))))
      4))

;; get the size of a struct based on its fields (assume each field is 32 bits)
(define (get-struct-size [fields : (Listof (cons Symbol Field))]) : Integer
  (if (empty? fields)
      0
      (+ 4 (get-struct-size (rest fields)))))

;; check if the first operand in a list is the same as all the rest
(define (first-operand-matches-rest? [first-op : Operand] [rest-ops : (Listof Operand)]) : Boolean
  (if (empty? rest-ops)
      #t
      (if (equal? first-op (first rest-ops))
          (first-operand-matches-rest? first-op (rest rest-ops))
          #f)))
            
;; creates the struct instructions (struct definitions)
(define (create-struct-instrs [structs : Struct-Environment]) : (Listof Struct-LLVM)
  (map (lambda ([struct : (cons Symbol Struct-Type)])
         (Struct-LLVM (car struct) (map (lambda ([field : (cons Symbol Field)])
                                           (Field-type (cdr field)))
                                         (sort (hash->list (Struct-Type-fields (cdr struct))) field<))))
       (sort (hash->list structs) struct<)))

;; creates the global instructions (declarations of global variables)
(define (create-global-instrs [globals : Variable-Environment]) : (Listof Global-LLVM)
  (map (lambda ([global : (cons Symbol Type)])
         (Global-LLVM (car global) (cdr global)))
       (hash->list globals)))

;; adds a list of LLVM instructions to a block
(define (add-LLVMs! [cur-block : Block] [LLVMs : (Listof LLVM-Instr)]) : Void
  (set-Block-LLVM! cur-block (append (Block-LLVM cur-block) LLVMs)))

;; adds an LLVM instruction to a block
(define (add-LLVM! [cur-block : Block] [LLVM : LLVM-Instr]) : Void
  (set-Block-LLVM! cur-block (append (Block-LLVM cur-block) (list LLVM))))

;; gets the declaration for a symbol from the environment
;; checks locals first (since local declarations can hide global ones) and then globals
(define (get-declaration [id : Symbol] [locals : Variable-Environment] [globals : Variable-Environment]) : LLVM-Declaration
  (if (hash-has-key? locals id)
      (LLVM-Declaration (hash-ref locals id) (Local id))
      (LLVM-Declaration (hash-ref globals id) (Global id))))

;; generates a visual representation of the CFG using dot!
(define (generate-graph [program : LLVM-Program] [input-filename : String]) : Void
  (match program
    [(LLVM-Program structs globals CFGs c-lib-funcs)
     (define out (open-output-file (string-append input-filename ".dot") #:exists 'replace))
     (display (apply string-append (map generate-digraph CFGs)) out)
     (close-output-port out)]))

;; generates the string for a single CFG (for one function)
(define (generate-digraph [cfg : CFG]) : String
  (match cfg
    [(CFG func-name parameters return-type blocks _ _ _)
     (define dot-code (string-append "digraph \"" (symbol->string func-name) "\" {\n"))
     (define entry-block (hash-ref blocks 'entry-block))
     (set! dot-code (string-append dot-code (apply string-append (map generate-label (hash-values blocks)))))
     (set! dot-code (string-append dot-code (apply string-append (map generate-edge (hash-values blocks)))))
     (string-append dot-code "}\n\n")]))

;; generate dot code for a label
(define (generate-label [cur-block : Block]) : String
  (match cur-block
    [(Block label LLVM _ definitions _ _ reachable _)
     (if reachable
         (string-append (number->string label) "[ label = \"" (number->string label) "\n" (LLVMs->string LLVM)
                        "\", xlabel = \"" (apply string-append (map definition->string (hash->list definitions))) "\"]\n")
         "")]))

;; generate dot code for an edge
(define (generate-edge [cur-block : Block]) : String
  (match cur-block
    [(Block label _ _ _ _ successors _ _)
     (if (empty? successors)
         ""
         (apply string-append (map (lambda ([successor-label : Integer])
                                     (string-append (number->string label) "->" (number->string successor-label) ";\n"))
                                   (map Block-label successors))))]))

;; converts an LLVM-Program object into its string representation!
(define (LLVM-Program->string [LLVM : LLVM-Program] [source-filename : String]) : String
  (match LLVM
    [(LLVM-Program structs globals CFGs c-lib-funcs)
     (string-append (LLVM-header-string source-filename) "\n"
                    (structs->string structs) "\n"
                    (c-lib-func-globals->string c-lib-funcs)
                    (globals->string globals) "\n"
                    (LLVM-CFGs->string CFGs)
                    (c-lib-funcs->string c-lib-funcs))]))

;; creates the header at the top of the LLVM files
(define (LLVM-header-string [source-filename : String]) : String
  (string-append "; ModuleID = '" source-filename "'\n"
                 "source_filename = \"" source-filename "\"\n"
                 "target datalayout = \"e-m:o-i64:64-f80:128-n8:16:32:64-S128\"\n"
                 "target triple = \"x86_64-apple-macosx10.15.0\"\n"))

;; converts a list of struct instructions to their string representations
(define (structs->string [structs : (Listof Struct-LLVM)]) : String
  (if (empty? structs)
      ""
      (string-append "%struct."
                     (symbol->string (Struct-LLVM-name (first structs)))
                     " = type { "
                     (type->string (first (Struct-LLVM-fields (first structs))) (Levels-Indirection 0 1))
                     (apply string-append (map (lambda ([type : Type]) (string-append ", " (type->string type (Levels-Indirection 0 1))))
                                               (rest (Struct-LLVM-fields (first structs)))))
                     " }\n"
                     (structs->string (rest structs)))))

;; creates the necessary global variables in order to use the appropriate C library functions
(define (c-lib-func-globals->string [c-lib-funcs : C-Lib-Funcs]) : String
  (define code "")
  (match c-lib-funcs
    [(C-Lib-Funcs _ _ printf-newline printf-space scanf)
     (cond [printf-newline
            (set! code (string-append code "@.str.print.newline = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n"))])
     (cond [printf-space
            (set! code (string-append code "@.str.print.space = private unnamed_addr constant [4 x i8] c\"%d \\00\", align 1\n"))])
     (cond [scanf
            (set! code (string-append code "@.str.read = private unnamed_addr constant [3 x i8] c\"%d\\00\", align 1\n"))])
     code]))

;; converts a list of global instructions to their string representations
(define (globals->string [globals : (Listof Global-LLVM)]) : String
  (if (empty? globals)
      ""
      (string-append "@"
                     (symbol->string (Global-LLVM-name (first globals)))
                     " = common global "
                     (type->string (Global-LLVM-type (first globals)) (Levels-Indirection 0 0))
                     " "
                     (default-global-value (Global-LLVM-type (first globals)))
                     ", align "
                     (align-value (Global-LLVM-type (first globals)))
                     "\n"
                     (globals->string (rest globals)))))

;; converts a list of Control Flow Graphs (CFGs) to their string representations
(define (LLVM-CFGs->string [CFGs : (Listof CFG)]) : String
  (if (empty? CFGs)
      ""
      (string-append (LLVM-CFG->string (first CFGs))
                     (LLVM-CFGs->string (rest CFGs)))))

;; converts a Control Flow Graph (CFG) to its string representation
(define (LLVM-CFG->string [cfg : CFG]) : String
  (match cfg
    [(CFG func-name parameters return-type blocks _ _ _)
     (string-append "define " (type->string return-type (Levels-Indirection 0 1))
                    " @" (symbol->string func-name) "(" (get-parameters-string parameters 0)
                    ") {\n" (block-table->string blocks))]))

;; converts the hashtable of blocks to their string representations
(define (block-table->string [blocks : Block-Table]) : String
  (define sorted-blocks (filter Block-reachable (sort (hash-values blocks) block<)))
  (string-append (block->string (first sorted-blocks) #t)
                 (apply string-append (map (lambda ([block : Block]) (block->string block #f)) (rest sorted-blocks)))))

;; converts a block to its string representation
(define (block->string [cur-block : Block] [entry-block? : Boolean]) : String
  (match cur-block
    [(Block label LLVM _ _ _ _ _ _)
     (if (empty? LLVM)
         ""
         (string-append (if entry-block?
                            ""
                            (string-append (number->string label) ":\n"))
                        (LLVMs->string LLVM) "\n"))]))

;; converts a list of LLVM instructions to their string representations
(define (LLVMs->string [LLVMs : (Listof LLVM-Instr)]) : String
  (if (empty? LLVMs)
      ""
      (string-append (LLVM->string (first LLVMs)) (LLVMs->string (rest LLVMs)))))

;; converts an LLVM instruction to its string representation
(define (LLVM->string [LLVM : LLVM-Instr]) : String
  (match LLVM
    [(Alloca-LLVM type target)
     (string-append "  " (operand->string target) " = alloca " (type->string type (Levels-Indirection 0 1))
                    ", align " (align-value type) "\n")]
    [(Arith-LLVM operator left right target)
     (string-append "  " (operand->string target) " = " (operator->string operator)
                    " i32 " (operand->string left) ", " (operand->string right) "\n")]
    [(Bool-LLVM operator left right target)
     (string-append "  " (operand->string target) " = " (operator->string operator)
                    " i32 " (operand->string left) ", " (operand->string right) "\n")]
    [(Comp-LLVM operator type left right target)
     (string-append "  " (operand->string target) " = icmp " (operator->string operator)
                    " " (type->string type (Levels-Indirection 0 1)) " " (operand->string left) ", "
                    (operand->string right) "\n")]
    [(Size-LLVM operator operand from to target)
     (string-append "  " (operand->string target) " = " (operator->string operator) " "
                    from " " (operand->string operand) " to " to "\n")]
    [(Get-Elm-Ptr-LLVM type pointer field-offset target)
     (string-append "  " (operand->string target) " = getelementptr " (type->string type (Levels-Indirection 0 0))
                    ", " (type->string type (Levels-Indirection 0 1)) " " (operand->string pointer) ", i32 0, i32 "
                    (number->string field-offset) "\n")]
    [(Call-LLVM func-name return-type arg-types args target)
     (string-append "  " (operand->string target) " = call " (type->string return-type (Levels-Indirection 0 1))
                    " " (operand->string func-name) "(" (get-arguments-string arg-types args)
                    ")\n")]
    [(Call-Void-LLVM func-name arg-types args)
     (string-append "  call void " (operand->string func-name) "(" (get-arguments-string arg-types args) ")\n")]
    [(Malloc-LLVM target size)
     (string-append "  " (operand->string target) " = call i8* @malloc (i64 " (number->string size) ")\n")]
    [(Free-LLVM target)
     (string-append "  call void @free(i8* " (operand->string target) ")\n")]
    [(Load-LLVM type source target indir)
     (string-append "  " (operand->string target) " = load "
                    (type->string type (Levels-Indirection indir indir)) ", "
                    (type->string type (Levels-Indirection (+ 1 indir) (+ 1 indir))) " "
                    (operand->string source) ", align "
                    (align-value type) "\n")]
    [(Store-LLVM type source target indir)
     (string-append "  store " (type->string type (Levels-Indirection indir indir)) " " (operand->string source) ", "
                    (type->string type (Levels-Indirection (+ 1 indir) (+ 1 indir))) " " (operand->string target) ", align "
                    (align-value type) "\n")]
    [(Print-LLVM operand endl target)
     (if endl
         (string-append "  " (operand->string target) " = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.print.newline, i64 0, i64 0), i32 " (operand->string operand) ")\n")
         (string-append "  " (operand->string target) " = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.print.space, i64 0, i64 0), i32 " (operand->string operand) ")\n"))]
    [(Scan-LLVM var result)
     (string-append "  " (operand->string result) " = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.read, i64 0, i64 0), i32* " (operand->string var) ")\n")]
    [(Branch-LLVM block)
     (string-append "  br label %" (number->string (Block-label block)) "\n")]
    [(Cond-Branch-LLVM cond true false)
     (string-append "  br i1 " (operand->string cond) ", label %" (number->string (Block-label true))
                    ", label %" (number->string (Block-label false)) "\n")]
    [(Return-Expr-LLVM type expr)
     (string-append "  ret " (type->string type (Levels-Indirection 0 1)) " " (operand->string expr) "\n}\n")]
    [(Return-Void-LLVM) "  ret void\n}\n"]
    [(Phi-LLVM _ type values target _)
     (string-append "  " (operand->string target) " = phi " (type->string type (Levels-Indirection 0 1))
                    " [ " (operand->string (Phi-Value-operand (first values))) ", %" (number->string (Phi-Value-block-label (first values))) " ]"
                    (apply string-append (map (lambda ([val : Phi-Value])
                                                (string-append ", [ " (operand->string (Phi-Value-operand val)) ", %"
                                                               (number->string (Phi-Value-block-label val)) " ]"))
                                              (rest values))) "\n")]
    [(Move-LLVM target source) (string-append "  " (operand->string target) " = " (operand->string source) "\n")]))

;; creates the string to declare the necessary C library functions
(define (c-lib-funcs->string [c-lib-funcs : C-Lib-Funcs]) : String
  (define code "")
  (match c-lib-funcs 
    [(C-Lib-Funcs malloc free printf-newline printf-space scanf)
     (cond [malloc (set! code (string-append code "\ndeclare i8* @malloc(i64)\n"))])
     (cond [free (set! code (string-append code "\ndeclare void @free(i8*)\n"))])
     (cond [(or printf-newline printf-space) (set! code (string-append code "\ndeclare i32 @printf(i8*, ...)\n"))])
     (cond [scanf (set! code (string-append code "\ndeclare i32 @scanf(i8*, ...)\n"))])
     code]))

;; converts types to their string representation
(define (type->string [type : Type] [levels : Levels-Indirection]) : String
  (cond
    [(equal? type 'void) "void"]
    [(equal? type 'int) (string-append "i32" (make-string (Levels-Indirection-primitive levels) #\*))]
    [(equal? type 'bool) (string-append "i1" (make-string (Levels-Indirection-primitive levels) #\*))]
    [else (string-append "%struct." (symbol->string type) (make-string (Levels-Indirection-struct levels) #\*))]))

;; converts operands to their string representation
(define (operand->string [operand : Operand]) : String
  (match operand
    [(Local id) (string-append "%" (symbol->string id))]
    [(Global id) (string-append "@" (symbol->string id))]
    [(Intermediate value) (string-append "%" (number->string value))]
    [(Block label _ _ _ _ _ _ _) (string-append "%" (number->string label))]
    [(Register num) (string-append "%" (number->string num))]
    [Literal (literal->string operand)]))

;; converts operators to their string representation
(define (operator->string [operator : LLVM-Operator]) : String
  (cond
    [(equal? operator '+) "add"]
    [(equal? operator '-) "sub"]
    [(equal? operator '*) "mul"]
    [(equal? operator '/) "sdiv"]
    [(equal? operator '||) "or"]
    [(equal? operator '&&) "and"]
    [(equal? operator '^^) "xor"]
    [(equal? operator '==) "eq"]
    [(equal? operator '!=) "ne"]
    [(equal? operator '<) "slt"]
    [(equal? operator '>) "sgt"]
    [(equal? operator '<=) "sle"]
    [(equal? operator '>=) "sge"]
    [(equal? operator 'trunc) "trunc"]
    [(equal? operator 'zext) "zext"]
    [(equal? operator 'bitcast) "bitcast"])) 

;; convert a definition to a string (in order to view the definitions in the dot file)
(define (definition->string [definition : (Pairof Symbol Operand)]) : String
  (string-append (symbol->string (car definition)) ": " (operand->string (cdr definition)) "\n"))

;; convert a literal to a string
(define (literal->string [literal : Operand]) : String
  (cond
    [(equal? literal 'null) "null"]
    [(boolean? literal) (if literal "true" "false")]
    [(integer? literal) (number->string literal)]
    [else (error 'literal->string "Unsupported literal: ~e" literal)]))
     
;; gets the default global value for the given type
(define (default-global-value [type : Type]) : String
  (cond
    [(or (equal? type 'int) (equal? type 'bool)) "0"]
    [else "zeroinitializer"]))

;; gets the alignment value for the given type
(define (align-value [type : Type]) : String
  (cond
    [(equal? type 'int) "4"]
    [(equal? type 'bool) "1"]
    [else "8"]))

;; forms the string of parameters for a function declaration
(define (get-parameters-string [parameters : (Listof Declaration)] [param-idx : Integer]) : String
  (if (empty? parameters)
      ""
      (block
       (define first-param-str
         (string-append (type->string (Declaration-type (first parameters)) (Levels-Indirection 0 1))                            
                        (cond
                          [(equal? mode 'stack)
                           (if (structure-type? (Declaration-type (first parameters)))
                               (string-append " %" (symbol->string (Declaration-id (first parameters))))
                               "")]
                          [(equal? mode 'registers) (string-append " %" (number->string param-idx))]
                          [else (error 'get-parameters-string "Unsupported compiler mode: ~e" mode)])))
       (if (empty? (rest parameters))
           first-param-str
           (string-append first-param-str ", " (get-parameters-string (rest parameters) (+ 1 param-idx)))))))

;; forms the string of arguments for a function invocation
(define (get-arguments-string [arg-types : (Listof Type)] [args : (Listof Operand)]) : String
  (if (empty? arg-types)
      ""
      (if (empty? (rest arg-types))
          (string-append (type->string (first arg-types) (Levels-Indirection 0 1))
                         " " (operand->string (first args)))
          (string-append (type->string (first arg-types) (Levels-Indirection 0 1))
                         " " (operand->string (first args))
                         ", " (get-arguments-string (rest arg-types) (rest args))))))

;; initialize the entry block of a function
;; this entails allocating declarations and storing parameter values on the stack
(define (initialize-entry-block [return-type : Return-Type] [parameters : (Listof Declaration)] [declarations : (Listof Declaration)]) : Block
  (set! counter 0)
  (define entry-block (Block -1 '() '() (make-hash '()) '() '() #t #t))
  (cond
    [(equal? mode 'stack)
     (cond [(not (equal? return-type 'void))
            (add-LLVM! entry-block (Alloca-LLVM return-type (Local '.return)))])
     (allocate-declarations entry-block parameters declarations)
     (store-parameters entry-block parameters)]
    [(equal? mode 'registers)
     (map (lambda ([var : Symbol])
            (write-variable! var (Register counter) entry-block)
            (set! counter (+ 1 counter)))
          (map Declaration-id parameters))]
    [else (error 'initialize-entry-block "Unsupported compiler mode ~e:" mode)])
  (set-Block-label! entry-block counter)
  (set! counter (+ 1 counter))
  entry-block)

;; create the alloca instructions in the entry block
;; for structures, we use the address that is passed into the function as the pointer to the struct (so we don't need to allocate more space for them)
(define (allocate-declarations [entry-block : Block] [parameters : (Listof Declaration)] [declarations : (Listof Declaration)]) : Void
  (define non-struct-params (filter (lambda ([param : Declaration]) (not (structure-type? (Declaration-type param)))) parameters))
  (add-LLVMs! entry-block
              (map (lambda ([dec : Declaration]) (Alloca-LLVM (Declaration-type dec) (Local (Declaration-id dec))))
                   (append non-struct-params declarations))))

;; store the values of all non-struct parameters into local variables on the stack
;; we don't allocate or store structure parameters (as explained above)
(define (store-parameters [entry-block : Block] [parameters : (Listof Declaration)]) : Void
  (define non-struct-params (filter (lambda ([param : Declaration]) (not (structure-type? (Declaration-type param)))) parameters))
  (add-LLVMs! entry-block (map (lambda ([param : Declaration])
                                (define target (Intermediate counter))
                                (set! counter (+ 1 counter))
                                (Store-LLVM (Declaration-type param) target (Local (Declaration-id param)) 0))
                              non-struct-params)))

;; clean up the exit block
;; this entails adding return instructions and setting the label (we couldn't set it before since block labels must be sequential)
(define (cleanup-exit-block [exit-block : Block] [return-type : Return-Type]) : Void
  (set-Block-sealed! exit-block #t)
  (if (equal? return-type 'void)
      (add-LLVM! exit-block (Return-Void-LLVM))
      (cond
        [(equal? mode 'stack)
         (add-LLVMs! exit-block (list (Load-LLVM return-type (Local '.return) (Intermediate (+ 1 counter)) (if (structure-type? return-type) 1 0))
                                      (Return-Expr-LLVM return-type (Intermediate (+ 1 counter)))))]
        [(equal? mode 'registers)
         (add-LLVM! exit-block (Return-Expr-LLVM return-type (Phi-Value-operand (read-variable 'return return-type exit-block))))]))
  (set-Block-label! exit-block counter))

;; creates a block and puts it in the hash table
(define (create-block [predecessors : (Listof Block)] [sealed : Boolean] [blocks : Block-Table]) : Block
  (define cur-block (Block counter '() '() (make-hash '()) predecessors '() #t sealed))
  (map (lambda ([pred : Block]) (set-Block-successors! pred (cons cur-block (Block-successors pred)))) predecessors)
  (hash-set! blocks counter cur-block)
  (set! counter (+ 1 counter))
  cur-block)

;; gets the target for an expression
(define (get-target) : Target
  (define target : Target (Intermediate -1))
  (cond
    [(equal? mode 'stack) (set! target (Intermediate counter))]
    [(equal? mode 'registers) (set! target (Register counter))])
  (set! counter (+ 1 counter))
  target)

;; add a branch between the two blocks if the from-block did not return (this would be indicated by a branch instruction already present)
(define (add-branch! [from-block : Block] [to-block : Block]) : Void
  (cond [(not (Branch-LLVM? (last (Block-LLVM from-block))))
         (set-Block-successors! from-block (list to-block))
         (set-Block-predecessors! to-block (cons from-block (Block-predecessors to-block)))
         (add-LLVM! from-block (Branch-LLVM to-block))]))

;; add a conditional branch to the two blocks if the current block did not return (this would be indicated by a branch instruction already present)
(define (add-loop-branch! [cur-block : Block] [guard : Operand] [body-block : Block] [after-block : Block] [header-block : Block]) : Void
  (cond [(not (Branch-LLVM? (last (Block-LLVM cur-block))))
         (set-Block-successors! cur-block (list body-block after-block))
         (set-Block-predecessors! body-block (list cur-block header-block))
         (add-LLVM! cur-block (Cond-Branch-LLVM guard body-block after-block))]))

;; defines the inequality used to sort blocks in the block hash table (integer comparison on the block labels)
(define (block< [block1 : Block] [block2 : Block]) : Boolean
  (< (Block-label block1) (Block-label block2)))

;; defines the inequality used to sort structs in the struct environment hash table (used to preserve struct definition order)
(define (struct< [struct1 : (cons Symbol Struct-Type)] [struct2 : (cons Symbol Struct-Type)]) : Boolean
  (< (Struct-Type-index (cdr struct1)) (Struct-Type-index (cdr struct2))))

;; defines the inequality used to sort fields in the field environment hash table (used to preserve field definition order)
(define (field< [field1 : (cons Symbol Field)] [field2 : (cons Symbol Field)]) : Boolean
  (< (Field-index (cdr field1)) (Field-index (cdr field2))))

;; set compiler mode from string
(define (set-compiler-mode! [flag : String]) : Void
  (set! mode (cond
               [(equal? flag "stack") 'stack]
               [(equal? flag "registers") 'registers]
               [else (error 'set-compiler-mode "Compiler mode ~e does not exist. Supported modes: \"-stack\" and \"-registers\"" flag)])))

;; is this instruction a control flow instruction?
(define (LLVM-control-flow-instr? [instruction : LLVM-Instr]) : Boolean
  (or (Cond-Branch-LLVM? instruction) (Branch-LLVM? instruction) (Return-Expr-LLVM? instruction) (Return-Void-LLVM? instruction)))

;; add "ret void" instructions at the end of every block in a void function without an existing control flow statement
(define (add-return-voids! [cur-block : Block] [exit-block : Block] [visited : Block-Table]) : Void
  (hash-set! visited (Block-label cur-block) cur-block)
  (cond [(or (empty? (Block-LLVM cur-block)) (not (LLVM-control-flow-instr? (last (Block-LLVM cur-block)))))
         (add-LLVM! cur-block (Branch-LLVM exit-block))])
  (for-each (lambda ([unvisited-block : Block])
              (add-return-voids! unvisited-block exit-block visited))
            (filter (lambda ([block : Block])
                      (not (hash-has-key? visited (Block-label block))))
                    (Block-successors cur-block))))

;; MILESTONE 4 FUNCTIONS

(define stack-counter 1)
(define data-counter 0)

;; convert an LLVM program to an ARM program (add the ARM code to each block)
(define (LLVM-Program->ARM-Program [LLVM-Prog : LLVM-Program] [env : Environment]) : ARM-Program
  (match LLVM-Prog
    [(LLVM-Program structs globals CFGs c-lib-funcs)
     (define global-ARM (create-global-ARM globals env))
     (for-each (lambda ([cfg : CFG])
                 (set! stack-counter 1)
                 (set! data-counter 0)
                 (define sorted-blocks (filter Block-reachable (sort (hash-values (CFG-blocks cfg)) block<)))
                 (for-each (lambda ([cur-block : Block])
                             (LLVMs->ARM (Block-LLVM cur-block) cur-block (CFG-blocks cfg) cfg))
                           sorted-blocks))
               CFGs)
     (ARM-Program global-ARM CFGs lib-funcs)]))

;; create the global instructions for ARM code
(define (create-global-ARM [globals : (Listof Global-LLVM)] [env : Environment]) : (Listof Global-ARM)
  (if (empty? globals)
      '()
      (cons (Global-ARM (Global-LLVM-name (first globals)) (get-type-size (Global-LLVM-type (first globals)) env))
            (create-global-ARM (rest globals) env))))

;; convert a list of LLVM instructions to ARM code
(define (LLVMs->ARM [LLVMs : (Listof LLVM-Instr)] [cur-block : Block] [blocks : Block-Table] [cfg : CFG]) : Void
  (cond
    [(not (empty? LLVMs))
     (cond
       [(Alloca-LLVM? (first LLVMs))
        (LLVMs->ARM (move-stack-pointer! LLVMs cur-block 0 cfg) cur-block blocks cfg)]
       [else (for-each (lambda ([LLVM : LLVM-Instr])
                         (LLVM->ARM LLVM cur-block blocks cfg))
                       LLVMs)])]))

;; convert an LLVM instruction to ARM code
(define (LLVM->ARM [LLVM : LLVM-Instr] [cur-block : Block] [blocks : Block-Table] [cfg : CFG]) : Void
  (match LLVM
    [(Alloca-LLVM type target)
     (error 'LLVM->ARM "All alloca instructions should occur at the beginning of the function.")]
    [(Arith-LLVM operator left right target)
     (if (not (equal? operator '/))
         (add-ARM! cur-block (Arith-ARM operator target (get-arm-operand! left cur-block cfg) (get-arm-operand! right cur-block cfg)))
         (block
          (set-Lib-Funcs-division! lib-funcs #t)
          (add-ARMs! cur-block (list (Move-ARM (Register 0) (get-arm-operand! left cur-block cfg))
                                     (Move-ARM (Register 1) (get-arm-operand! right cur-block cfg))
                                     (Branch-Label-ARM 'aeabi_idiv)
                                     (Move-ARM target (Register 0))))))]
    [(Bool-LLVM operator left right target)
     (add-ARM! cur-block (Bool-ARM operator target (get-arm-operand! left cur-block cfg) (get-arm-operand! right cur-block cfg)))]
    [(Comp-LLVM operator type left right target)
     (add-ARMs! cur-block (list (Move-ARM target 0)
                                (Comp-ARM (get-arm-operand! left cur-block cfg) (get-arm-operand! right cur-block cfg))
                                (Move-Cond-ARM operator target 1)))]
    [(Size-LLVM operator operand from to target)
     (cond
       [(equal? operator 'bitcast)
        (add-ARM! cur-block (Move-ARM (get-arm-operand! target cur-block cfg) (get-arm-operand! operand cur-block cfg)))]
       [else (error 'LLVM->ARM "Unsupported operator - not yet implemented.")])]
    [(Get-Elm-Ptr-LLVM type pointer field-offset target)
     (add-ARM! cur-block (Arith-ARM '+ target (get-arm-operand! pointer cur-block cfg) (* field-offset 4)))]
    [(Call-LLVM func-name _ _ args target)
     (define arg-num 0)
     (for-each (lambda ([arg : Operand])
                 (add-ARM! cur-block (Move-ARM (Register arg-num) (get-arm-operand! arg cur-block cfg)))
                 (set! arg-num (+ 1 arg-num)))
               args)
     (add-ARMs! cur-block (list (Branch-Label-ARM (Global-id func-name))
                                (Move-ARM target (Register 0))))]
    [(Call-Void-LLVM func-name arg-types args)
     (define arg-num 0)
     (for-each (lambda ([arg : Operand])
                 (add-ARM! cur-block (Move-ARM (Register arg-num) (get-arm-operand! arg cur-block cfg)))
                 (set! arg-num (+ 1 arg-num)))
               args)
     (add-ARM! cur-block (Branch-Label-ARM (Global-id func-name)))]
    [(Malloc-LLVM target size)
     (add-ARMs! cur-block (list (Move-Word-ARM (Register 0) size)
                                (Branch-Label-ARM 'malloc)
                                (Move-ARM target (Register 0))))]
    [(Free-LLVM target)
     (add-ARMs! cur-block (list (Move-ARM (Register 0) target)
                                (Branch-Label-ARM 'free)))]
    [(Load-LLVM _ source target _)
     (if (Local? source)
         (add-ARM! cur-block (Load-Stack-ARM (get-arm-operand! target cur-block cfg)
                                             (if (hash-has-key? (CFG-stack cfg) source)
                                                 (hash-ref (CFG-stack cfg) source)
                                                 (error 'LLVM->ARM "Local variable ~e is used undefined" (Local-id source)))))
         (add-ARM! cur-block (Load-Reg-ARM (get-arm-operand! target cur-block cfg) (get-arm-operand! source cur-block cfg))))]
    [(Store-LLVM type source target indir)
     (define source-operand
       (if (integer? source)
           (block
            (define target (get-register))
            (add-ARM! cur-block (Move-ARM target source))
            target)
           (get-arm-operand! source cur-block cfg)))
     (if (Local? target)
         (add-ARM! cur-block (Store-Stack-ARM source-operand (get-stack-offset cur-block target cfg)))
         (add-ARM! cur-block (Store-Reg-ARM source-operand (get-arm-operand! target cur-block cfg))))]
    [(Print-LLVM operand endl target)
     (add-ARMs! cur-block (list (Move-ARM (Register 1) (get-arm-operand! operand cur-block cfg))
                                (Move-Word-Constant-ARM (Register 0) (if endl '.PRINTLN_FMT '.PRINT_FMT))
                                (Move-Top-Constant-ARM (Register 0) (if endl '.PRINTLN_FMT '.PRINT_FMT))
                                (Branch-Label-ARM 'printf)))]
    [(Scan-LLVM target result)
     (add-ARMs! cur-block (list (Move-Word-Constant-ARM (Register 1) '.read_scratch)
                                (Move-Top-Constant-ARM (Register 1) '.read_scratch)
                                (Move-Word-Constant-ARM (Register 0) '.READ_FMT)
                                (Move-Top-Constant-ARM (Register 0) '.READ_FMT)
                                (Branch-Label-ARM 'scanf)))
     (define target-operand (get-arm-operand! target cur-block cfg))
     (add-ARMs! cur-block (list (Move-Word-Constant-ARM target-operand '.read_scratch)
                                (Move-Top-Constant-ARM target-operand '.read_scratch)
                                (Load-Reg-ARM target-operand target-operand)))]
    [(Branch-LLVM block)
     (add-ARM! cur-block (Branch-ARM (Block-label block)))]
    [(Cond-Branch-LLVM cond true false)
     (add-ARMs! cur-block (list (Comp-ARM (get-arm-operand! cond cur-block cfg) 1)
                                (Branch-Equal-ARM (Block-label true))
                                (Branch-ARM (Block-label false))))]
    [(Return-Expr-LLVM type expr)
     (add-ARM! cur-block (Move-ARM (Register 0) (get-arm-operand! expr cur-block cfg)))]
    [(Phi-LLVM _ _ values target _)
     (for-each (lambda ([phi-val : Phi-Value])
                 (define definition-block (hash-ref blocks (Phi-Value-block-label phi-val)))
                 (insert-temp-move! target (Phi-Value-operand phi-val) definition-block '() (Block-ARM definition-block)))
               values)]
    [(Move-LLVM target source)
     (add-ARM! cur-block (Move-ARM (get-arm-operand! target cur-block cfg) (get-arm-operand! source cur-block cfg)))]))

(define (move-stack-pointer! [LLVMs : (Listof LLVM-Instr)] [cur-block : Block] [alloc-size : Integer] [cfg : CFG]) : (Listof LLVM-Instr)
  (cond
    [(and (not (empty? LLVMs)) (Alloca-LLVM? (first LLVMs)))
     (move-stack-pointer! (rest LLVMs) cur-block (+ 4 alloc-size) cfg)]
    [else
     (add-ARM! cur-block (Arith-ARM '- (Special-Register 'sp) (Special-Register 'sp) alloc-size))
     (set-CFG-alloc-size! cfg alloc-size)
     LLVMs]))

(define (get-stack-offset [cur-block : Block] [local : Local] [cfg : CFG]) : Integer
  (if (hash-has-key? (CFG-stack cfg) local)
      (hash-ref (CFG-stack cfg) local)
      (block
       (define stack-offset (- (CFG-alloc-size cfg) (* 4 stack-counter)))
       (set! stack-counter (+ 1 stack-counter))
       (hash-set! (CFG-stack cfg) local stack-offset)
       stack-offset)))

;; insert a move instruction to a temporary register to handle phi nodes from the LLVM code
;; since we are in SSA form, this move can happen anywhere in the definition block
;;    thus, put it as far toward the end of the block as possible to reduce register pressure
(define (insert-temp-move! [temp : Operand] [value : Operand] [cur-block : Block] [prev-ARM : (Listof ARM-Instr)] [post-ARM : (Listof ARM-Instr)]) : Void
  (if (not (empty? post-ARM))
      (if (ARM-control-flow-instr? (first post-ARM))
          (set-Block-ARM! cur-block (append prev-ARM (list (Move-ARM temp value)) post-ARM))
          (insert-temp-move! temp value cur-block (append prev-ARM (list (first post-ARM))) (rest post-ARM)))
      (error 'insert-temp-move! "Unsupported behavior - we reached the end of definition block ~e without a control flow instruction." (Block-label cur-block))))

;; convert an ARM-Program to its string representation
(define (ARM-Program->string [ARM-Prog : ARM-Program]) : String
  (match ARM-Prog
    [(ARM-Program global-decs CFGs lib-funcs)
     (define func-counter 0)
     (string-append (apply string-append (map (lambda ([cfg : CFG])
                                                (define func-idx func-counter)
                                                (set! func-counter (+ 1 func-counter))
                                                (ARM-CFG->string cfg func-idx))
                                              CFGs))
                    (apply string-append (map global-ARM->string global-decs))
                    (lib-funcs->string lib-funcs))]))

;; convert a CFG from ARM to its string representation
(define (ARM-CFG->string [cfg : CFG] [func-idx : Integer]) : String
  (match cfg
    [(CFG func-name _ _ blocks stack data alloc-size)
     (define sorted-blocks (filter Block-reachable (sort (hash-values (CFG-blocks cfg)) block<)))
     (string-append "   .globl   " (symbol->string func-name) "\n"
                    "   .p2align 2\n"
                    "   .type " (symbol->string func-name) ",%function\n"
                    "   .code 32\n"
                    (symbol->string func-name) ":\n"
                    "   .fnstart\n"
                    (if (not (equal? alloc-size 0))
                        (string-append "   .pad #" (number->string alloc-size))
                        "")
                    (apply string-append (map (lambda ([ARM : ARM-Instr])
                                                (ARM->string ARM func-idx))
                                              (Block-ARM (first sorted-blocks))))
                    (apply string-append (map (lambda ([cur-block : Block])
                                                (string-append ".LBB" (number->string func-idx) "_" (number->string (Block-label cur-block)) ":\n"
                                                               (apply string-append (map (lambda ([ARM : ARM-Instr])
                                                                                           (ARM->string ARM func-idx))
                                                                                         (Block-ARM cur-block)))))
                                              (rest sorted-blocks)))
                    (apply string-append (map (lambda ([data-entry : (cons Global Data-Entry)])
                                                (ARM-data->string data-entry func-idx))
                                              (sort (hash->list data) data<)))
                    ".Lfunc_end" (number->string func-idx) ":\n"
                    "   .size " (symbol->string func-name) ", .Lfunc_end" (number->string func-idx) "-" (symbol->string func-name) "\n"
                    "   .cantunwind\n"
                    "   .fnend\n\n")]))

;; convert an ARM instruction to its string representation
(define (ARM->string [ARM : ARM-Instr] [func-idx : Integer]) : String
  (match ARM
    [(Arith-ARM operator target left right)
     (string-append "   " (ARM-operator->string operator) " " (ARM-operand->string target) ", "
                    (ARM-operand->string left) ", " (ARM-operand->string right) "\n")]
    [(Bool-ARM operator target left right)
     (string-append "   " (ARM-operator->string operator) " " (ARM-operand->string target) ", "
                    (ARM-operand->string left) ", " (ARM-operand->string right) "\n")]
    [(Move-ARM target source)
     (string-append "   mov " (ARM-operand->string target) ", " (ARM-operand->string source) "\n")]
    [(Branch-Label-ARM label)
     (string-append "   bl " (symbol->string label) "\n")]
    [(Comp-ARM left right)
     (string-append "   cmp " (ARM-operand->string left) ", " (ARM-operand->string right) "\n")]
    [(Move-Cond-ARM operator target source)
     (string-append "   " (ARM-operator->string operator) " " (ARM-operand->string target) ", "
                    (ARM-operand->string source) "\n")]
    [(Move-Word-ARM target source)
     (string-append "   movw " (ARM-operand->string target) ", " (ARM-operand->string source) "\n")]
    [(Move-Word-Lower-ARM target source)
     (string-append "   movw " (ARM-operand->string target) ", #:lower16:" (number->string source) "\n")]
    [(Move-Word-Constant-ARM target source)
     (string-append "   movw " (ARM-operand->string target) ", #:lower16:" (symbol->string source) "\n")]
    [(Move-Top-ARM target source)
     (string-append "   movt " (ARM-operand->string target) ", #:upper16:" (number->string source) "\n")]
    [(Move-Top-Constant-ARM target source)
     (string-append "   movt " (ARM-operand->string target) ", #:upper16:" (symbol->string source) "\n")]
    [(Load-Reg-ARM target source)
     (string-append "   ldr " (ARM-operand->string target) ", [" (operand->string source) "]\n")]
    [(Load-Data-ARM target data-idx)
     (string-append "   ldr " (ARM-operand->string target) ", .LCPI" (number->string func-idx) "_" (number->string data-idx) "\n")]
    [(Load-Stack-ARM target stack-offset)
     (string-append "   ldr " (ARM-operand->string target) ", [sp, " (if (equal? stack-offset 0)
                                                                       ""
                                                                       (string-append ", #" (number->string stack-offset)))
                    "]\n")]
    [(Store-Reg-ARM source target)
     (string-append "   str " (ARM-operand->string source) ", [" (operand->string target) "]\n")]
    [(Store-Stack-ARM source stack-offset)
     (string-append "   str " (ARM-operand->string source) ", [sp" (if (equal? stack-offset 0)
                                                                       ""
                                                                       (string-append ", #" (number->string stack-offset)))
                    "]\n")]
    [(Branch-Equal-ARM block-label)
     (string-append "   beq " (ARM-label->string block-label func-idx) "\n")]
    [(Branch-ARM block-label)
     (string-append "   b " (ARM-label->string block-label func-idx) "\n")]))


(define (ARM-label->string [block-label : Integer] [func-idx : Integer]) : String
  (string-append ".LBB" (number->string func-idx) "_" (number->string block-label)))

;; convert an ARM data definition to a string
(define (ARM-data->string [data : (cons Global Data-Entry)] [func-idx : Integer]) : String
  (string-append ".LCPI" (number->string func-idx) "_" (number->string (Data-Entry-index (cdr data))) ":\n"
                 "   .long " (symbol->string (Global-id (car data))) "\n"))

;; convert a global ARM declaration to a string
(define (global-ARM->string [global-dec : Global-ARM]) : String
  (string-append "   .type " (symbol->string (Global-ARM-id global-dec)) ",%object\n"
                 "   .comm " (symbol->string (Global-ARM-id global-dec)) "," (number->string (Global-ARM-size global-dec)) ",4\n"))

;; convert ARM library functions to a string
(define (lib-funcs->string [lib-funcs : Lib-Funcs]) : String
  (match lib-funcs
    [(Lib-Funcs c-lib-funcs division)
     (match c-lib-funcs
       [(C-Lib-Funcs _ _ printf-newline printf-space scanf)
        (string-append
         (if scanf (string-append "   .type .read_scratch,%object\n"
                                  "   .comm .read_scratch,4,4\n")
             "")
         (if (or printf-newline printf-space scanf)
             (string-append "   .section .rodata\n"
                            (if printf-newline (string-append "   .align 2\n"
                                                              ".PRINTLN_FMT:\n"
                                                              "   .asciz \"%ld\\n\"\n")
                                "")
                            (if printf-space (string-append "   .align 2\n"
                                                            ".PRINT_FMT:\n"
                                                            "   .asciz \"%ld \"\n")
                                "")
                            (if scanf (string-append "   .align 2\n"
                                                     ".READ_FMT:\n"
                                                     "   .asciz \"%ld\"\n")
                                ""))
             "")
         (if division "   .global __aeabi_idiv\n" ""))])]))

;; convert an ARM operator to its corresponding string
(define (ARM-operator->string [operator : ARM-Operator]) : String
  (cond
    [(equal? operator '+) "add"]
    [(equal? operator '-) "sub"]
    [(equal? operator '*) "mul"]
    [(equal? operator '||) "orr"]
    [(equal? operator '&&) "and"]
    [(equal? operator '^^) "eor"]
    [(equal? operator '==) "moveq"]
    [(equal? operator '!=) "movne"]
    [(equal? operator '<) "movlt"]
    [(equal? operator '>) "movgt"]
    [(equal? operator '<=) "movle"]
    [(equal? operator '>=) "movge"]))

;; convert an operand to a string in ARM code
(define (ARM-operand->string [operand : Operand]) : String
  (match operand
    [(Local id) (error 'ARM-operand->string "Unsupported operand type in ARM code: LOCAL")]
    [(Global id) (error 'ARM-operand->string "Unsupported operand type in ARM code: GLOBAL")]
    [(Intermediate value) (string-append "r" (number->string value))]
    [(Block _ _ _ _ _ _ _ _) (error 'ARM-operand->string "Unsupported operand type in ARM code: BLOCK")]
    [(Register num) (string-append "r" (number->string num))]
    [(Special-Register name) (symbol->string name)]
    [Literal (ARM-literal->string operand)]))

;; convert a literal to a string in ARM code
(define (ARM-literal->string [literal : Operand]) : String
  (cond
    [(equal? literal 'null) "#0"]
    [(boolean? literal) (if literal "#1" "#0")]
    [(integer? literal) (string-append "#" (number->string literal))]
    [else (error 'literal->string "Unsupported literal: ~e" literal)]))

;; get the ARM operand corresponding to an LLVM operand
(define (get-arm-operand! [operand : Operand] [cur-block : Block] [cfg : CFG]) : Operand
  (cond
    [(Global? operand)
     (if (hash-has-key? (CFG-data cfg) operand)
         (Data-Entry-register (hash-ref (CFG-data cfg) operand))
         (block
          (define target-register (get-register))
          (define data-idx (get-data-counter))
          (add-ARM! cur-block (Load-Data-ARM target-register data-idx))
          (hash-set! (CFG-data cfg) operand (Data-Entry target-register data-idx))
          target-register))]
    [(integer? operand)
     (if (<= operand 4095)
         operand
         (block
          (define target-register (Register counter))
          (add-ARM! cur-block (Move-Word-Lower-ARM target-register operand))
          (add-ARM! cur-block (Move-Top-ARM target-register operand))
          (set! counter (+ 1 counter))
          target-register))]
    [else operand]))

;; get the register corresponding to the current counter
(define (get-register) : Register
  (define register (Register counter))
  (set! counter (+ 1 counter))
  register)

;; get the index corresponding to the next piece of data and increment the counter
(define (get-data-counter) : Integer
  (define data data-counter)
  (set! data-counter (+ 1 data-counter))
  data)

;; adds a list of ARM instructions to a block
(define (add-ARMs! [cur-block : Block] [ARMs : (Listof ARM-Instr)]) : Void
  (set-Block-ARM! cur-block (append (Block-ARM cur-block) ARMs)))

;; adds an ARM instruction to a block
(define (add-ARM! [cur-block : Block] [ARM : ARM-Instr]) : Void
  (set-Block-ARM! cur-block (append (Block-ARM cur-block) (list ARM))))

;; defines the inequality used to sort data entries in the data hash table (used to preserve data definition order)
(define (data< [data1 : (cons Global Data-Entry)] [data2 : (cons Global Data-Entry)]) : Boolean
  (< (Data-Entry-index (cdr data1)) (Data-Entry-index (cdr data2))))

;; is this operand a literal? 
(define (literal? [operand : Operand]) : Boolean
  (or (integer? operand) (boolean? operand) (equal? operand 'null)))

;; is this ARM instruction a control flow instruction?
(define (ARM-control-flow-instr? [instr : ARM-Instr]) : Boolean
  (or (Branch-Label-ARM? instr) (Branch-Equal-ARM? instr) (Branch-ARM? instr)))