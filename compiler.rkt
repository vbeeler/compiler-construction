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
       (define bin-op (if (equal? (string->symbol operator) '\|\|) '|| (string->symbol operator)))
       (Binary bin-op (parse left) (parse right))]
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
(define (compile-json [json-str : String] [compiler-mode : String] [output-type : String] [register-allocation : String]
                      [useless-code-elimination : String] [source-filename : String]) : String
  (set-compiler-mode! compiler-mode)
  (set-register-allocation! register-allocation)
  (set-useless-code-elimination! useless-code-elimination)
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
        [(If guard then-block) (all-paths-return? (rest statements))]
        [(If-Else guard then-block else-block) (or (and (all-paths-return? (Block-Expr-statements then-block))
                                                        (all-paths-return? (Block-Expr-statements else-block)))
                                                   (all-paths-return? (rest statements)))]
        [(Loop guard body) (all-paths-return? (rest statements))]
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
       (cond [(not (or (equal? actual-return-type expected-return-type)
                       (equal? actual-return-type 'null)))
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
     (let ([left-type : Expression-Type (type-check-expression left env func-type)]
           [right-type : Expression-Type (type-check-expression right env func-type)])
       (cond
         [(or (equal? operator '==) (equal? operator '!=))
          (if (or (and (equal? left-type 'int) (equal? right-type 'int))
                  (and (structure-type? left-type) (structure-type? right-type)
                       (or (equal? left-type right-type) (equal? left-type 'null) (equal? right-type 'null))))
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

(define INTM_COUNTER 0)
(define BLOCK_COUNTER 0)
(define MODE : Compiler-Mode 'registers)
(define C_LIB_FUNCS (C-Lib-Funcs #f #f #f #f #f))
(define LIB_FUNCS (Lib-Funcs C_LIB_FUNCS #f))

;; creates the LLVM-Program object for the program
(define (create-LLVM [functions : (Listof Function)] [env : Environment]) : LLVM-Program
  (define struct-instrs (create-struct-instrs (Environment-structs env)))
  (define global-instrs (create-global-instrs (Environment-globals env)))
  (define func-CFGs (map (lambda ([function : Function]) (create-CFG function env)) functions))
  (LLVM-Program struct-instrs global-instrs func-CFGs C_LIB_FUNCS))

;; creates a control flow graph (CFG) for a single function
(define (create-CFG [function : Function] [env : Environment]) : CFG
  (match function
    [(Function name parameters return-type declarations statements)
     (define stack (ann (make-hash '()) Stack-Table))
     (define intm-counter (Counter 0))
     (define entry-block (initialize-entry-block return-type parameters declarations stack intm-counter))
     (define arg-num-max-four (if (< (length parameters) 4)
                                  (length parameters)
                                  4))
     (define exit-block (Block -1 '() '() '() (make-hash '()) '() '() #f #f '() (Live-Analysis (Bitset 0) (Bitset 0) (Bitset 0)) 1))
     (define blocks 
       (ann (make-hash
             (list (cons 'entry-block entry-block)
                   (cons 'exit-block exit-block)))
            Block-Table))
     (add-statements-LLVM! entry-block blocks statements env name intm-counter)
     (map remove-trivial-phis! (map Block-LLVM (hash-values blocks)) (hash-values blocks))
     (cond [(not (empty? (Block-successors entry-block)))
            (set-Block-reachable! exit-block #t)
            (cleanup-exit-block exit-block return-type intm-counter)
            (add-return-voids! entry-block exit-block (make-hash '()))]
           [(and (equal? return-type 'void)
                 (or (empty? (Block-LLVM entry-block))
                     (not (Return-Void-LLVM? (last (Block-LLVM entry-block))))))
            (add-LLVM! entry-block (Return-Void-LLVM))])
     (cond [USELESS_CODE_ELIMINATION? (useless-code-elimination! blocks)])
     (CFG name parameters return-type blocks stack 0 0 '() intm-counter)]))

;; adds the LLVM for the given list of statements to the supplied block
;; updates the block hash table accordingly
(define (add-statements-LLVM! [cur-block : Block] [blocks : Block-Table] [statements : (Listof Statement)] [env : Environment] [func-name : Symbol] [counter : Counter]) : Block
  (if (empty? statements)
      cur-block
      (match (first statements)
        [(Block-Expr statement-list)
         (add-statements-LLVM! cur-block blocks (append statement-list (rest statements)) env func-name counter)]
        [(Assignment target source)
         (if (equal? source 'read)
             (add-read-LLVM! target cur-block env func-name counter)
             (add-assignment-LLVM! target source cur-block env func-name counter))
         (add-statements-LLVM! cur-block blocks (rest statements) env func-name counter)]
        [(Print expression endl)
         (define exp-operand (add-expression-LLVM! expression cur-block env func-name counter))
         (define intermediate (get-intermediate counter))
         (add-LLVM! cur-block (Print-LLVM exp-operand endl intermediate))
         (if endl
             (set-C-Lib-Funcs-printf-newline! C_LIB_FUNCS #t)
             (set-C-Lib-Funcs-printf-space! C_LIB_FUNCS #t))
         (add-statements-LLVM! cur-block blocks (rest statements) env func-name counter)]
        [(If guard then)
         (define guard-operand (add-expression-LLVM! guard cur-block env func-name counter))
         (define then-block (create-block (list cur-block) #t blocks (/ (Block-execution-frequency cur-block) 2)))
         (define last-then-block (add-statements-LLVM! then-block blocks (list then) env func-name counter))
         (define after-block (create-block (list cur-block) #t blocks (Block-execution-frequency cur-block)))
         (add-branch! last-then-block after-block)
         (add-LLVM! cur-block (Cond-Branch-LLVM guard-operand then-block after-block))
         (add-statements-LLVM! after-block blocks (rest statements) env func-name counter)]
        [(If-Else guard then else)
         (define guard-operand (add-expression-LLVM! guard cur-block env func-name counter))
         (define then-block (create-block (list cur-block) #t blocks (/ (Block-execution-frequency cur-block) 2)))
         (define last-then-block (add-statements-LLVM! then-block blocks (list then) env func-name counter))
         (define else-block (create-block (list cur-block) #t blocks (/ (Block-execution-frequency cur-block) 2)))
         (define last-else-block (add-statements-LLVM! else-block blocks (list else) env func-name counter))
         (add-LLVM! cur-block (Cond-Branch-LLVM guard-operand then-block else-block))
         (define after-block (create-block (list last-then-block last-else-block) #t blocks (Block-execution-frequency cur-block)))
         (add-branch! last-then-block after-block)
         (add-branch! last-else-block after-block)
         (add-statements-LLVM! after-block blocks (rest statements) env func-name counter)]
        [(Loop guard body)
         (define guard-operand (add-expression-LLVM! guard cur-block env func-name counter))
         (define body-block (create-block (list cur-block) #f blocks (* (Block-execution-frequency cur-block) 10)))
         (define last-body-block (add-statements-LLVM! body-block blocks (list body) env func-name counter))
         (define new-guard-operand (add-expression-LLVM! guard last-body-block env func-name counter))
         (define after-block (create-block (list cur-block last-body-block) #t blocks (Block-execution-frequency cur-block)))
         (add-LLVM! cur-block (Cond-Branch-LLVM guard-operand body-block after-block))
         (set-Block-successors! cur-block (list body-block after-block))
         (add-loop-branch! last-body-block new-guard-operand body-block after-block cur-block)
         (cond
           [(equal? MODE 'registers) (complete-phis! (Block-LLVM body-block) body-block counter)
                                     (set-Block-sealed! body-block #t)])
         (add-statements-LLVM! after-block blocks (rest statements) env func-name counter)]
        [(Delete expression)
         (define intermediate (get-intermediate counter))
         (define exp-operand (add-expression-LLVM! expression cur-block env func-name counter))
         (add-LLVMs! cur-block (list (Size-LLVM 'bitcast
                                                 exp-operand
                                                 (string-append "%struct." (symbol->string (type-check-expression expression env (hash-ref (Environment-functions env) func-name))) "*")
                                                 "i8*"
                                                 intermediate)
                                     (Free-LLVM intermediate)))
         (set-C-Lib-Funcs-free! C_LIB_FUNCS #t)
         (add-statements-LLVM! cur-block blocks (rest statements) env func-name counter)]
        [(Return expression)
         (define result (add-expression-LLVM! expression cur-block env func-name counter))
         (define return-type (Function-Type-return-type (hash-ref (Environment-functions env) func-name)))
         (define exit-block (hash-ref blocks 'exit-block))
         (if (equal? (Block-label cur-block) (Block-label (hash-ref blocks 'entry-block)))
             (add-LLVM! cur-block (Return-Expr-LLVM return-type result))
             (block
              (cond
                [(equal? MODE 'stack) (add-LLVM! cur-block (Store-LLVM return-type result (Local '.return) (if (structure-type? return-type) 1 0)))]
                [(equal? MODE 'registers) (write-variable! 'return result cur-block)])
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
         (define args (map (lambda ([arg : Expression]) (add-expression-LLVM! arg cur-block env func-name counter)) arguments))
         (define function (hash-ref (Environment-functions env) id))
         (define return-type (Function-Type-return-type function))
         (if (equal? 'void return-type)
             (add-LLVM! cur-block (Call-Void-LLVM (Global id) (Function-Type-args function) args))
             (add-LLVM! cur-block (Call-LLVM (Global id) return-type (Function-Type-args function) args (get-intermediate counter))))
         (add-statements-LLVM! cur-block blocks (rest statements) env func-name counter)])))

;; adds the LLVM for the given expression to the supplied block
;; returns the operand that holds the result of this expression
(define (add-expression-LLVM! [expression : Expression] [cur-block : Block] [env : Environment] [func-name : Symbol] [counter : Counter]) : Operand
  (match expression
    [(Binary operator left right)
     (define left-result (add-expression-LLVM! left cur-block env func-name counter))
     (define right-result (add-expression-LLVM! right cur-block env func-name counter))
     (define intermediate (get-intermediate counter))
     (cond [(member operator (list '+ '- '* '/))
            (add-LLVM! cur-block (Arith-LLVM operator left-result right-result intermediate))]
           [(member operator (list '|| '&& '^^))
            (add-LLVM! cur-block (Bool-LLVM operator left-result right-result intermediate))]
           [(member operator (list '== '!= '< '> '<= '>=))
            (define type (if (or (equal? operator '==) (equal? operator '!=))
                             (type-check-expression left env (hash-ref (Environment-functions env) func-name))
                             'int))
            (add-LLVM! cur-block (Comp-LLVM operator type left-result right-result intermediate))])
     intermediate]
    [(Unary operator expression)
     (define result (add-expression-LLVM! expression cur-block env func-name counter))
     (define intermediate (get-intermediate counter))
     (cond [(equal? operator '-) (add-LLVM! cur-block (Arith-LLVM '- 0 result intermediate))]
           [(equal? operator '!) (add-LLVM! cur-block (Bool-LLVM '^^ result #t intermediate))])
     intermediate]
    [(Selector left id)
     (define pointer (add-selector-LLVM! expression cur-block env func-name counter))
     (define type (type-check-expression left env (hash-ref (Environment-functions env) func-name)))
     (define intermediate (get-intermediate counter))
     (define field (hash-ref (Struct-Type-fields (hash-ref (Environment-structs env) type)) id))
     (add-LLVM! cur-block (Load-LLVM (Field-type field) pointer intermediate (if (structure-type? (Field-type field)) 1 0)))
     intermediate]
    [(Invocation id arguments)
     (define args (map (lambda ([arg : Expression]) (add-expression-LLVM! arg cur-block env func-name counter)) arguments))
     (define intermediate (get-intermediate counter))
     (define function (hash-ref (Environment-functions env) id))
     (add-LLVM! cur-block (Call-LLVM (Global id) (Function-Type-return-type function) (Function-Type-args function) args intermediate))
     intermediate]
    [(Allocation id)
     (define first-intermediate (get-intermediate counter))
     (define second-intermediate (get-intermediate counter))
     (set-C-Lib-Funcs-malloc! C_LIB_FUNCS #t)
     (add-LLVM! cur-block (Malloc-LLVM first-intermediate (get-type-size id env)))
     (add-LLVM! cur-block (Size-LLVM 'bitcast first-intermediate "i8*" (string-append "%struct." (symbol->string id) "*") second-intermediate))
     second-intermediate]
    [literal
     (cond
       [(equal? 'null literal) 'null]
       [(symbol? literal)
        (define declaration (get-declaration literal (Function-Type-locals (hash-ref (Environment-functions env) func-name)) (Environment-globals env)))
        (define type (LLVM-Declaration-type declaration))
        (define operand (LLVM-Declaration-operand declaration))
        (cond
          [(equal? MODE 'stack)
           (define intermediate (get-intermediate counter))
           (add-LLVM! cur-block (Load-LLVM type (LLVM-Declaration-operand declaration) intermediate (if (structure-type? type) 1 0)))
           intermediate]
          [(equal? MODE 'registers)
           (if (Local? operand)
               (Phi-Value-operand (read-variable literal type cur-block counter))
               operand)]
          [else (error 'add-expression-LLVM! "Unsupported compiler mode, given ~e" MODE)])]
       [(integer? literal) literal]
       [(boolean? literal) literal]
       [else (error 'add-expression-LLVM! "Invalid expression type ~e" literal)])]))

;; adds the LLVM for a read instruction to the current block
(define (add-read-LLVM! [target : Expression] [cur-block : Block] [env : Environment] [func-name : Symbol] [counter : Counter]) : Void
  (set-C-Lib-Funcs-scanf! C_LIB_FUNCS #t)
  (cond
    [(equal? MODE 'stack)
     (define target-operand
       (if (symbol? target)
           (if (hash-has-key? (Function-Type-locals (hash-ref (Environment-functions env) func-name)) target)
               (Local target)
               (Global target)) 
           (add-selector-LLVM! target cur-block env func-name counter)))
     (add-LLVM! cur-block (Scan-LLVM target-operand (get-intermediate counter)))]
    [(equal? MODE 'registers)
     (cond
       [(symbol? target)
        (cond
          [(hash-has-key? (Function-Type-locals (hash-ref (Environment-functions env) func-name)) target)
           (define first-intermediate (get-intermediate counter))
           (define second-intermediate (get-intermediate counter))
           (add-LLVMs! cur-block (list (Scan-LLVM (Global '.read_scratch) first-intermediate)
                                       (Load-LLVM 'int (Global '.read_scratch) second-intermediate 0)))
           (write-variable! target second-intermediate cur-block)]
          [else (add-LLVM! cur-block (Scan-LLVM (Global target) (get-intermediate counter)))])]
       [else (add-LLVM! cur-block (Scan-LLVM (add-selector-LLVM! target cur-block env func-name counter) (get-intermediate counter)))])]
    [else (error 'add-read-LLVM! "Unsupported compiler mode: ~e" MODE)]))

;; adds the LLVM for an assignment instruction to the current block
(define (add-assignment-LLVM! [target : Expression] [source : Expression] [cur-block : Block] [env : Environment] [func-name : Symbol] [counter : Counter]) : Void
  (cond
    [(equal? MODE 'stack)
     (define target-operand
       (if (symbol? target)
           (if (hash-has-key? (Function-Type-locals (hash-ref (Environment-functions env) func-name)) target)
               (Local target)
               (Global target))
           (add-selector-LLVM! target cur-block env func-name counter)))
     (define source-operand (add-expression-LLVM! source cur-block env func-name counter))
     (define type (type-check-expression target env (hash-ref (Environment-functions env) func-name)))
     (add-LLVM! cur-block (Store-LLVM type source-operand target-operand (if (structure-type? type) 1 0)))]
    [(equal? MODE 'registers)
     (define source-operand (add-expression-LLVM! source cur-block env func-name counter))
     (if (symbol? target)
         (if (hash-has-key? (Function-Type-locals (hash-ref (Environment-functions env) func-name)) target)
             (write-variable! target source-operand cur-block)
             (block
              (define type (type-check-expression target env (hash-ref (Environment-functions env) func-name)))
              (add-LLVM! cur-block (Store-LLVM type source-operand (Global target) (if (structure-type? type) 1 0)))))
         (block
          (define target-operand (add-selector-LLVM! target cur-block env func-name counter))
          (define type (type-check-expression target env (hash-ref (Environment-functions env) func-name)))
          (add-LLVM! cur-block (Store-LLVM type source-operand target-operand (if (structure-type? type) 1 0)))))]
    [else (error 'add-assignment-LLVM! "Unsupported compiler mode: ~e" MODE)]))

;; adds the LLVM for a selector to the current block - simply gets the address of the desired field
;; this method is necessary since there is different behavior for selectors as right vs left values of assignments (need value vs address respectively)
(define (add-selector-LLVM! [target : Expression] [cur-block : Block] [env : Environment] [func-name : Symbol] [counter : Counter]) : Operand
  (match target
    [(Selector left id)
     (define type (type-check-expression left env (hash-ref (Environment-functions env) func-name)))
     (define pointer (if (symbol? left)            
                         (if (hash-has-key? (Function-Type-locals (hash-ref (Environment-functions env) func-name)) left)
                             (cond
                               [(equal? MODE 'stack) (define intermediate (get-intermediate counter))
                                                     (add-LLVM! cur-block (Load-LLVM type (Local left) intermediate 1))
                                                     intermediate]
                               [(equal? MODE 'registers) (Phi-Value-operand (read-variable left type cur-block counter))]
                               [else (error 'add-selector-LLVM "Unsupported compiler mode ~e" MODE)])
                             (Global left))
                         (add-expression-LLVM! left cur-block env func-name counter)))
     (define next-intermediate (get-intermediate counter))
     (define field (hash-ref (Struct-Type-fields (hash-ref (Environment-structs env) type)) id))
     (add-LLVM! cur-block (Get-Elm-Ptr-LLVM type pointer (Field-index field) next-intermediate))
     next-intermediate]
    [else (error 'add-selector-LLVM! "Unsupported behavior - ~e is an invalid left expression of an assignment operation." target)]))

;; complete the phis of the given block in the process of sealing it
(define (complete-phis! [instructions : (Listof LLVM-Instr)] [cur-block : Block] [counter : Counter]) : Void     
  (cond
    [(and (not (empty? instructions)) (Phi-LLVM? (first instructions)))
     (match (first instructions)
       [(Phi-LLVM var type values target complete)
        (cond [(not complete) (add-phi-operands! (first instructions) cur-block (Block-predecessors cur-block) counter)
                              (set-Phi-LLVM-complete! (first instructions) #t)])
        (complete-phis! (rest instructions) cur-block counter)])]))

;; add operands to the phi instruction by finding the value of the variable from each of the block's predecessors
(define (add-phi-operands! [phi : Phi-LLVM] [cur-block : Block] [predecessors : (Listof Block)] [counter : Counter]) : Void
  (cond
    [(not (empty? predecessors))
     (set-Phi-LLVM-values! phi (append (Phi-LLVM-values phi) (list (read-variable (Phi-LLVM-var phi) (Phi-LLVM-type phi) (first predecessors) counter))))
     (add-phi-operands! phi cur-block (rest predecessors) counter)]))

;; get the value of the variable in the current block (may need to look at its predecessors and add phi nodes)
(define (read-variable [var : Symbol] [type : Type] [cur-block : Block] [counter : Counter]) : Phi-Value
  (match cur-block
    [(Block _ _ _ _ definitions predecessors _ _ sealed _ _ _)
     (cond
       [(hash-has-key? definitions var) (Phi-Value (hash-ref definitions var) (Block-label cur-block))]
       [(not sealed) (define intermediate (get-intermediate counter))
                     (set-Block-LLVM! cur-block (cons (Phi-LLVM var type '() intermediate #f) (Block-LLVM cur-block)))
                     (write-variable! var intermediate cur-block)
                     (Phi-Value intermediate (Block-label cur-block))]
       [(empty? predecessors) (error 'read-variable "Variable with identifier ~e has not been initialized in block ~e" var cur-block)]
       [(equal? (length predecessors) 1) (define value (read-variable var type (first predecessors) counter))
                                         (write-variable! var (Phi-Value-operand value) cur-block)
                                         value]
       [else (define intermediate (get-intermediate counter))
             (define phi (Phi-LLVM var type '() intermediate #t))
             (set-Block-LLVM! cur-block (cons phi (Block-LLVM cur-block)))
             (write-variable! var intermediate cur-block)
             (add-phi-operands! phi cur-block predecessors counter)
             (Phi-Value intermediate (Block-label cur-block))])]))

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
    [(LLVM-Program structs globals CFGs C_LIB_FUNCS)
     (define out (open-output-file (string-append input-filename ".dot") #:exists 'replace))
     (display (apply string-append (map generate-digraph CFGs)) out)
     (close-output-port out)]))

;; generates the string for a single CFG (for one function)
(define (generate-digraph [cfg : CFG]) : String
  (match cfg
    [(CFG func-name parameters return-type blocks _ _ _ _ _)
     (define dot-code (string-append "digraph \"" (symbol->string func-name) "\" {\n"))
     (define entry-block (hash-ref blocks 'entry-block))
     (set! dot-code (string-append dot-code (apply string-append (map generate-label (hash-values blocks)))))
     (set! dot-code (string-append dot-code (apply string-append (map generate-edge (hash-values blocks)))))
     (string-append dot-code "}\n\n")]))

;; generate dot code for a label
(define (generate-label [cur-block : Block]) : String
  (match cur-block
    [(Block label LLVM _ _ definitions _ _ reachable _ _ _ _)
     (if reachable
         (string-append (number->string label) "[ label = \"" (number->string label) "\n" (LLVMs->string LLVM)
                        "\", xlabel = \"" (apply string-append (map definition->string (hash->list definitions))) "\"]\n")
         "")]))

;; generate dot code for an edge
(define (generate-edge [cur-block : Block]) : String
  (match cur-block
    [(Block label _ _ _ _ _ successors _ _ _ _ _)
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
            (set! code (string-append code "@.str.read = private unnamed_addr constant [3 x i8] c\"%d\\00\", align 1\n"
                                      "@.read_scratch = common global i32 0, align 4\n"))])
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
    [(CFG func-name parameters return-type blocks _ _ _ _ _)
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
    [(Block label LLVM _ _ _ _ _ _ _ _ _ _)
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
    [(Block label _ _ _ _ _ _ _ _ _ _ _) (string-append "%" (number->string label))]
    [(Register num) (string-append "%" (number->string num))]
    [(Parameter num) (string-append "p" (number->string num))]
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
                          [(equal? MODE 'stack)
                           (if (structure-type? (Declaration-type (first parameters)))
                               (string-append " %" (symbol->string (Declaration-id (first parameters))))
                               "")]
                          [(equal? MODE 'registers) (string-append " %" (number->string param-idx))]
                          [else (error 'get-parameters-string "Unsupported compiler mode: ~e" MODE)])))
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
(define (initialize-entry-block [return-type : Return-Type] [parameters : (Listof Declaration)] [declarations : (Listof Declaration)] [stack : Stack-Table] [counter : Counter]) : Block
  (define entry-block (Block (get-block-counter) '() '() '() (make-hash '()) '() '() #t #t '() (Live-Analysis (Bitset 0) (Bitset 0) (Bitset 0)) 1))
  (cond
    [(equal? MODE 'stack)
     (cond [(not (equal? return-type 'void))
            (add-LLVM! entry-block (Alloca-LLVM return-type (Local '.return)))])
     (allocate-declarations entry-block parameters declarations)
     (store-parameters entry-block parameters counter)]
    [(equal? MODE 'registers)
     (define arg-num 0)
     (define arg-ids (map Declaration-id parameters))
     (for-each (lambda ([var : Symbol])
                 (cond [(< arg-num 4)
                        (write-variable! var (Parameter arg-num) entry-block)]
                       [else (write-variable! var (Parameter arg-num) entry-block)
                             (hash-set! stack (Parameter arg-num) (* 4 (- arg-num 3)))])
                 (set! arg-num (+ 1 arg-num)))
               arg-ids)]
    [else (error 'initialize-entry-block "Unsupported compiler mode ~e:" MODE)])
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
(define (store-parameters [entry-block : Block] [parameters : (Listof Declaration)] [counter : Counter]) : Void
  (define non-struct-params (filter (lambda ([param : Declaration]) (not (structure-type? (Declaration-type param)))) parameters))
  (add-LLVMs! entry-block (map (lambda ([param : Declaration])
                                (Store-LLVM (Declaration-type param) (get-intermediate counter) (Local (Declaration-id param)) 0))
                              non-struct-params)))

;; clean up the exit block
;; this entails adding return instructions and setting the label (we couldn't set it before since block labels must be sequential)
(define (cleanup-exit-block [exit-block : Block] [return-type : Return-Type] [counter : Counter]) : Void
  (set-Block-sealed! exit-block #t)
  (if (equal? return-type 'void)
      (add-LLVM! exit-block (Return-Void-LLVM))
      (cond
        [(equal? MODE 'stack)
         (define intermediate (get-intermediate counter))
         (add-LLVMs! exit-block (list (Load-LLVM return-type (Local '.return) intermediate (if (structure-type? return-type) 1 0))
                                      (Return-Expr-LLVM return-type intermediate)))]
        [(equal? MODE 'registers)
         (add-LLVM! exit-block (Return-Expr-LLVM return-type (Phi-Value-operand (read-variable 'return return-type exit-block counter))))]))
  (set-Block-label! exit-block (get-block-counter)))

;; creates a block and puts it in the hash table
(define (create-block [predecessors : (Listof Block)] [sealed : Boolean] [blocks : Block-Table] [execution-frequency : Real]) : Block
  (define block-idx (get-block-counter))
  (define cur-block (Block block-idx '() '() '() (make-hash '()) predecessors '() #t sealed '() (Live-Analysis (Bitset 0) (Bitset 0) (Bitset 0)) execution-frequency))
  (map (lambda ([pred : Block]) (set-Block-successors! pred (cons cur-block (Block-successors pred)))) predecessors)
  (hash-set! blocks block-idx cur-block)
  cur-block)

;; gets a new target for an expression
(define (get-intermediate [counter : Counter]) : Operand
  (define intermediate (Intermediate (Counter-num counter)))
  (set-Counter-num! counter (+ 1 (Counter-num counter)))
  intermediate)

;; add a branch between the two blocks if the from-block did not return (this would be indicated by a branch instruction already present)
(define (add-branch! [from-block : Block] [to-block : Block]) : Void
  (cond [(or (empty? (Block-LLVM from-block)) (not (Branch-LLVM? (last (Block-LLVM from-block)))))
         (set-Block-successors! from-block (list to-block))
         (set-Block-predecessors! to-block (cons from-block (Block-predecessors to-block)))
         (add-LLVM! from-block (Branch-LLVM to-block))]))

;; add a conditional branch to the two blocks if the current block did not return (this would be indicated by a branch instruction already present)
(define (add-loop-branch! [cur-block : Block] [guard : Operand] [body-block : Block] [after-block : Block] [header-block : Block]) : Void
  (cond [(or (empty? (Block-LLVM cur-block)) (not (Branch-LLVM? (last (Block-LLVM cur-block)))))
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
  (set! MODE (cond
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

(define STACK_COUNTER 0)
(define SPILL_STACK_COUNTER 0)

;; convert an LLVM program to an ARM program (add the ARM code to each block)
(define (LLVM-Program->ARM-Program [LLVM-Prog : LLVM-Program] [env : Environment]) : ARM-Program
  (match LLVM-Prog
    [(LLVM-Program structs globals CFGs c-lib-funcs)
     (define global-ARM (create-global-ARM globals env))
     (for-each (lambda ([cfg : CFG])
                 (set! STACK_COUNTER 0)
                 (set! SPILL_STACK_COUNTER 0)
                 (define sorted-blocks (filter Block-reachable (sort (hash-values (CFG-blocks cfg)) block<)))
                 (for-each (lambda ([cur-block : Block])
                             (LLVMs->ARM (Block-LLVM cur-block) cur-block (CFG-blocks cfg) cfg))
                           sorted-blocks)
                 (cond [REGISTER_ALLOCATION? (allocate-registers! cfg)]))
               CFGs)
     (ARM-Program global-ARM CFGs LIB_FUNCS)]))

;; create the global instructions for ARM code
(define (create-global-ARM [globals : (Listof Global-LLVM)] [env : Environment]) : (Listof Global-ARM)
  (if (empty? globals)
      '()
      (cons (Global-ARM (Global-LLVM-name (first globals)) (get-type-size (Global-LLVM-type (first globals)) env))
            (create-global-ARM (rest globals) env))))

;; convert a list of LLVM instructions to ARM code
(define (LLVMs->ARM [LLVMs : (Listof LLVM-Instr)] [cur-block : Block] [blocks : Block-Table] [cfg : CFG]) : Void
  (for-each (lambda ([LLVM : LLVM-Instr])
              (LLVM->ARM LLVM cur-block blocks cfg))
            LLVMs))

;; convert an LLVM instruction to ARM code
(define (LLVM->ARM [LLVM : LLVM-Instr] [cur-block : Block] [blocks : Block-Table] [cfg : CFG]) : Void
  (cond [(not (Return-Void-LLVM? LLVM))
         (match LLVM
           [(Alloca-LLVM type target)
            (place-local-on-stack! target cfg)]
           [(Arith-LLVM operator left right target) 
            (if (not (equal? operator '/))
                (add-ARM! cur-block (Arith-ARM operator target (get-reg-operand! left cur-block cfg) (if (equal? operator '+)
                                                                                                         (get-arm-operand! right cur-block cfg)
                                                                                                         (get-reg-operand! right cur-block cfg))))
                (block
                 (set-Lib-Funcs-division! LIB_FUNCS #t)
                 (add-ARM! cur-block (Branch-Label-ARM '__aeabi_idiv (list (get-arm-operand! left cur-block cfg) (get-arm-operand! right cur-block cfg)) (list target) '()))))]
           [(Bool-LLVM operator left right target)
            (add-ARM! cur-block (Bool-ARM operator target (get-reg-operand! left cur-block cfg) (get-arm-operand! right cur-block cfg)))]
           [(Comp-LLVM operator type left right target)
            (add-ARMs! cur-block (list (Comp-ARM (get-reg-operand! left cur-block cfg) (get-arm-operand! right cur-block cfg))
                                       (Move-ARM target 0)
                                       (Move-Cond-ARM operator target 1)))]
           [(Size-LLVM operator operand from to target)
            (cond
              [(equal? operator 'bitcast)
               (add-ARM! cur-block (Move-ARM (get-arm-operand! target cur-block cfg) (get-arm-operand! operand cur-block cfg)))]
              [else (error 'LLVM->ARM "Unsupported operator - not yet implemented.")])]
           [(Get-Elm-Ptr-LLVM type pointer field-offset target)
            (add-ARM! cur-block (Arith-ARM '+ target (get-arm-operand! pointer cur-block cfg) (* field-offset 4)))]
           [(Call-LLVM func-name _ _ args target)
            (cond
              [(<= (length args) 4)
               (define reg-args (map (lambda ([arg : Operand])
                                       (get-arm-operand! arg cur-block cfg))
                                     args))
               (add-ARM! cur-block (Branch-Label-ARM (Global-id func-name) reg-args (list target) '()))]
              [else (define reg-args (map (lambda ([arg : Operand])
                                            (get-arm-operand! arg cur-block cfg))
                                          (take args 4)))
                    (define store-stack-ARMs (map (lambda ([arg : Operand] [arg-idx : Integer])
                                                    (place-arg-on-stack arg arg-idx cur-block cfg))
                                                  (list-tail args 4) (range 4 (length args))))
                    (add-ARM! cur-block (Branch-Label-ARM (Global-id func-name) reg-args (list target) store-stack-ARMs))])]
           [(Call-Void-LLVM func-name arg-types args)
            (cond
              [(<= (length args) 4)
               (define reg-args (map (lambda ([arg : Operand])
                                       (get-arm-operand! arg cur-block cfg))
                                     args))
               (add-ARM! cur-block (Branch-Label-ARM (Global-id func-name) reg-args '() '()))]
              [else (define reg-args (map (lambda ([arg : Operand])
                                            (get-arm-operand! arg cur-block cfg))
                                          (take args 4)))
                    (define store-stack-ARMs (map (lambda ([arg : Operand] [arg-idx : Integer])
                                                    (place-arg-on-stack arg arg-idx cur-block cfg))
                                                  (list-tail args 4) (range 4 (length args))))
                    (add-ARM! cur-block (Branch-Label-ARM (Global-id func-name) reg-args '() store-stack-ARMs))])]
           [(Malloc-LLVM target size)
            (add-ARM! cur-block (Branch-Label-ARM 'malloc (list size) (list target) '()))]
           [(Free-LLVM target)
            (add-ARM! cur-block (Branch-Label-ARM 'free (list target) '() '()))]
           [(Load-LLVM _ source target _)
            (define target-operand (get-arm-operand! target cur-block cfg))
            (cond [(Local? source)
                   (add-ARM! cur-block (Load-Stack-ARM target-operand (get-stack-offset source cfg) 'fp))]
                  [(Global? source)
                   (define intermediate (get-intermediate (CFG-intermediate-counter cfg)))
                   (add-ARMs! cur-block (list (Move-Word-Constant-ARM intermediate (Global-id source))
                                              (Move-Top-Constant-ARM intermediate (Global-id source))))
                   (add-ARM! cur-block (Load-Reg-ARM target-operand intermediate))]
                  [else (add-ARM! cur-block (Load-Reg-ARM target-operand source))])]
           [(Store-LLVM type source target indir)
            (define source-operand (get-reg-operand! source cur-block cfg))
            (cond [(Local? target)
                   (add-ARM! cur-block (Store-Stack-ARM source-operand (get-stack-offset target cfg) 'fp))]
                  [(Global? target)
                   (define intermediate (get-intermediate (CFG-intermediate-counter cfg)))
                   (add-ARMs! cur-block (list (Move-Word-Constant-ARM intermediate (Global-id target))
                                              (Move-Top-Constant-ARM intermediate (Global-id target))))
                   (add-ARM! cur-block (Store-Reg-ARM source-operand intermediate))]
                  [else (add-ARM! cur-block (Store-Reg-ARM source-operand target))])]
           [(Print-LLVM operand endl target)
            (add-ARM! cur-block (Branch-Label-ARM 'printf (list (if endl '.PRINTLN_FMT '.PRINT_FMT) (get-arm-operand! operand cur-block cfg)) '() '()))]
           [(Scan-LLVM target result)
            (define target-operand
              (cond [(Global? target) (Global-id target)]
                    [(Local? target) '.read_scratch]
                    [else target]))
            (add-ARM! cur-block (Branch-Label-ARM 'scanf (list '.READ_FMT target-operand) '() '()))
            (cond [(Local? target)
                   (define intermediate (get-intermediate (CFG-intermediate-counter cfg)))
                   (add-ARMs! cur-block (list (Move-Word-Constant-ARM intermediate 'read_scratch)
                                              (Move-Top-Constant-ARM intermediate 'read_scratch)
                                              (Load-Reg-ARM intermediate intermediate)
                                              (Store-Stack-ARM intermediate (get-stack-offset target cfg) 'fp)))])]
           [(Branch-LLVM block)
            (add-ARMs! cur-block (append (get-temp-moves! cur-block (CFG-intermediate-counter cfg))
                                         (list (Branch-ARM (Block-label block)))))
            (set-Block-add-temp-moves! cur-block '())]
           [(Cond-Branch-LLVM conditional true-block false-block)
            (cond [(and (not (empty? (Block-add-temp-moves cur-block))) (back-edge? cur-block true-block))
                   (add-ARMs! cur-block (append (list (Comp-ARM (get-reg-operand! conditional cur-block cfg) 1)
                                                      (Branch-Not-Equal-ARM (Block-label false-block)))
                                                (get-temp-moves! cur-block (CFG-intermediate-counter cfg))
                                                (list (Branch-ARM (Block-label true-block)))))
                   (set-Block-add-temp-moves! cur-block '())]
                  [else (cond [(not (empty? (Block-add-temp-moves cur-block)))
                               (propagate-temp-moves! cur-block)])
                        (add-ARMs! cur-block (list (Comp-ARM (get-reg-operand! conditional cur-block cfg) 1)
                                                   (Branch-Equal-ARM (Block-label true-block))
                                                   (Branch-ARM (Block-label false-block))))])]
           [(Return-Expr-LLVM type expr)
            (add-ARM! cur-block (Move-ARM (Register 0) (get-arm-operand! expr cur-block cfg)))]
           [(Phi-LLVM _ _ values target _)
            (for-each (lambda ([phi-val : Phi-Value])
                        (define def-label (Phi-Value-block-label phi-val))
                        (define entry-block (hash-ref blocks 'entry-block))
                        (define definition-block (if (equal? def-label (Block-label entry-block))
                                                     entry-block
                                                     (hash-ref blocks def-label)))
                        (insert-temp-move! target (Phi-Value-operand phi-val) definition-block '() (Block-ARM definition-block) cfg))
                      values)]
           [(Move-LLVM target source)
            (add-ARM! cur-block (Move-ARM (get-arm-operand! target cur-block cfg) (get-arm-operand! source cur-block cfg)))])]))

;; propagate the temporary move instructions of a block (used to resolve phi nodes)
;; these move instructions were only stored in the block, as opposed to added immediately, if they are related to a while loop
;; we must place these move instructions after the conditional branch out of the loop, and before the branch back into the loop
;; do a breadth first search to find the first back-edge and place them there
(define (propagate-temp-moves! [cur-block : Block]) : Void
  (define queue (map (lambda ([successor-block : Block])
                       (Edge cur-block successor-block))
                     (Block-successors cur-block)))
  (prop-temp-moves! cur-block queue))

;; helper function for propagating temporary move instructions (above)
(define (prop-temp-moves! [cur-block : Block] [queue : (Listof Edge)]) : Void
  (cond [(not (empty? queue))
         (define edge (first queue))
         (cond [(back-edge? (Edge-first-block edge) (Edge-next-block edge))
                (set-Block-add-temp-moves! (Edge-first-block edge) (append (Block-add-temp-moves (Edge-first-block edge))
                                                                           (Block-add-temp-moves cur-block)))
                (set-Block-add-temp-moves! cur-block '())]
               [else (define edges-to-add (map (lambda ([successor-block : Block])
                                                 (Edge (Edge-next-block edge) successor-block))
                                               (Block-successors (Edge-next-block edge))))
                     (prop-temp-moves! cur-block (append (rest queue) edges-to-add))])]
        [else (error 'prop-temp-moves! "Unsupported behavior - Unadded phi node move instructions ~e" (Block-add-temp-moves cur-block))]))

;; place the given operand on the stack
(define (place-local-on-stack! [local : Local] [cfg : CFG]) : Void
  (define stack-offset (+ (- 8 (* -4 STACK_COUNTER))))
  (set! STACK_COUNTER (+ 1 STACK_COUNTER))
  (set-CFG-alloc-size! cfg (+ 4 (CFG-alloc-size cfg)))
  (hash-set! (CFG-stack cfg) local stack-offset))

;; get the stack offset for the given local variable (or place it on the stack if it's not already there)
(define (get-stack-offset [local : Local] [cfg : CFG]) : Integer
  (if (hash-has-key? (CFG-stack cfg) local)
      (hash-ref (CFG-stack cfg) local)
      (error 'get-stack-offset "Cannot get the stack offset for ~e, since it has not been placed on the stack" local)))

;; get the stack offset for the given spilled register (or place it on the stack if it's not already there)
(define (get-spill-stack-offset [operand : Operand] [cfg : CFG]) : Integer
  (if (hash-has-key? (CFG-stack cfg) operand)
      (hash-ref (CFG-stack cfg) operand)
      (block
       (define stack-offset (+ (CFG-arg-alloc-size cfg) (* 4 SPILL_STACK_COUNTER)))
       (set! SPILL_STACK_COUNTER (+ 1 SPILL_STACK_COUNTER))
       (set-CFG-alloc-size! cfg (+ 4 (CFG-alloc-size cfg)))
       (hash-set! (CFG-stack cfg) operand stack-offset)
       stack-offset)))

;; get the stack offset for a given argument number
(define (get-arg-stack-offset [arg-num : Integer]) : Integer
  (* 4 (- arg-num 3)))

;; place an argument on the stack when the number of arguments exceeds 4
(define (place-arg-on-stack [arg : Operand] [arg-num : Integer] [cur-block : Block] [cfg : CFG]) : Store-Stack-ARM
  (set-CFG-alloc-size! cfg (+ 4 (CFG-alloc-size cfg)))
  (cond [(< (CFG-arg-alloc-size cfg) (* 4 (- arg-num 3)))
         (set-CFG-arg-alloc-size! cfg (* 4 (- arg-num 3)))])
  (define stack-offset (* 4 (- arg-num 4)))
  (Store-Stack-ARM (get-reg-operand! arg cur-block cfg) stack-offset 'sp))

;; get the register operand (needed since some instructions need to use registers)
(define (get-reg-operand! [operand : Operand] [cur-block : Block] [cfg : CFG]) : Operand
  (cond
    [(or (integer? operand) (boolean? operand) (equal? operand 'null))
     (define intermediate (get-intermediate (CFG-intermediate-counter cfg)))
     (add-ARM! cur-block (Move-ARM intermediate (get-arm-operand! operand cur-block cfg)))
     intermediate]
    [else (get-arm-operand! operand cur-block cfg)]))

;; insert a move instruction to a temporary register to handle phi nodes from the LLVM code
;; since we are in SSA form, this move can happen anywhere in the definition block
;;    thus, put it as far toward the end of the block as possible to reduce register pressure
(define (insert-temp-move! [temp : Operand] [value : Operand] [cur-block : Block] [prev-ARM : (Listof ARM-Instr)] [post-ARM : (Listof ARM-Instr)] [cfg : CFG]) : Void
  (cond [(not (empty? post-ARM))
         (cond [(ARM-branch-instr? (first post-ARM))
                (set-Block-ARM! cur-block prev-ARM)
                (define value-operand (get-arm-operand! value cur-block cfg))
                (set-Block-ARM! cur-block (append (Block-ARM cur-block) (list (Move-ARM temp value-operand)) post-ARM))]
               [else (insert-temp-move! temp value cur-block (append prev-ARM (list (first post-ARM))) (rest post-ARM) cfg)])]
        [else (set-Block-add-temp-moves! cur-block (cons (Move-ARM temp value) (Block-add-temp-moves cur-block)))]))

;; add the temporary move instructions that were originally ignored until the block became filled
;; I was failing a test case since the source of a future move was getting overwritten with the target of a previous one - handle this by
;;    adding temporary intermediates to store the appropriate values
(define (get-temp-moves! [cur-block : Block] [counter : Counter]) : (Listof Move-ARM)
  (define targets : (Listof Operand) '())
  (define moves : (Listof Move-ARM) '())
  (for-each (lambda ([cur-move : Move-ARM])
              (cond [(not (member cur-move moves))
                     (cond [(member (Move-ARM-source cur-move) targets)
                            (define intermediate (get-intermediate counter))
                            (set! moves (cons (Move-ARM intermediate (Move-ARM-source cur-move))
                                              (append moves (list (Move-ARM (Move-ARM-target cur-move) intermediate)))))]
                           [else (set! moves (append moves (list cur-move)))])])
              (set! targets (append targets (list (Move-ARM-target cur-move)))))
            (Block-add-temp-moves cur-block))
  moves)

;; convert an ARM-Program to its string representation
(define (ARM-Program->string [ARM-Prog : ARM-Program]) : String
  (match ARM-Prog
    [(ARM-Program global-decs CFGs lib-funcs)
     (string-append "   .arch armv7-a\n"
                    (apply string-append (map global-ARM->string global-decs)) "\n"
                    "   .text\n"
                    (apply string-append (map (lambda ([cfg : CFG])
                                                (ARM-CFG->string cfg))
                                              CFGs))
                    (lib-funcs->string lib-funcs))]))

;; convert a CFG from ARM to its string representation
(define (ARM-CFG->string [cfg : CFG]) : String
  (match cfg
    [(CFG func-name _ _ blocks stack alloc-size _ saved-registers _)
     (define sorted-blocks (filter Block-reachable (sort (hash-values (CFG-blocks cfg)) block<)))
     (string-append "   .align 2\n"
                    "   .global   " (symbol->string func-name) "\n"
                    (symbol->string func-name) ":\n"
                    (string-append ".LU" (number->string (Block-label (hash-ref blocks 'entry-block))) ":\n")
                     "   push {fp, lr}\n"
                     "   add fp, sp, #4\n" 
                    (if (not (empty? saved-registers))
                        (string-append "   push {" (string-append "r" (number->string (first saved-registers)))
                                       (apply string-append (map (lambda ([reg : Integer]) (string-append ", r" (number->string reg))) (rest saved-registers))) "}\n")
                        "")
                    (if (not (equal? 0 alloc-size))
                        (string-append "   sub sp, sp, #" (number->string alloc-size) "\n")
                        "")
                    (apply string-append (map (lambda ([ARM : ARM-Instr])
                                                (ARM->string ARM))
                                              (if REGISTER_ALLOCATION?
                                                  (Block-allocated-ARM (first sorted-blocks))
                                                  (Block-ARM (first sorted-blocks)))))
                    (apply string-append (map (lambda ([cur-block : Block])
                                                (string-append ".LU" (number->string (Block-label cur-block)) ":\n"
                                                               (apply string-append (map (lambda ([ARM : ARM-Instr])
                                                                                           (ARM->string ARM))
                                                                                         (if REGISTER_ALLOCATION?
                                                                                             (Block-allocated-ARM cur-block)
                                                                                             (Block-ARM cur-block))))))
                                              (rest sorted-blocks)))
                    (if (not (equal? 0 alloc-size))
                        (string-append "   add sp, sp, #" (number->string alloc-size) "\n")
                        "")
                    (if (not (empty? saved-registers))
                        (string-append "   pop {" (string-append "r" (number->string (first saved-registers)))
                                       (apply string-append (map (lambda ([reg : Integer]) (string-append ", r" (number->string reg))) (rest saved-registers))) "}\n")
                        "")
                    "   pop {fp, pc}\n"
                    "   .size " (symbol->string func-name) ", .-" (symbol->string func-name) "\n")]))

;; convert an ARM instruction to its string representation
(define (ARM->string [ARM : ARM-Instr]) : String
  (match ARM
    [(Arith-ARM operator target left right)
     (string-append "   " (ARM-operator->string operator) " " (ARM-operand->string target) ", "
                    (ARM-operand->string left) ", " (ARM-operand->string right) "\n")]
    [(Bool-ARM operator target left right)
     (string-append "   " (ARM-operator->string operator) " " (ARM-operand->string target) ", "
                    (ARM-operand->string left) ", " (ARM-operand->string right) "\n")]
    [(Move-ARM target source)
     (string-append "   mov " (ARM-operand->string target) ", " (ARM-operand->string source) "\n")]
    [(Branch-Label-ARM label args targets store-stack-ARMs)
     (string-append (apply string-append (map ARM->string store-stack-ARMs))
                    (apply string-append (map (lambda ([arg : (U Operand Symbol)] [arg-idx : Integer])
                                                (cond [(equal? 'null arg) (ARM->string (Move-ARM (Register arg-idx) 'null))]
                                                      [(symbol? arg) (string-append (ARM->string (Move-Word-Constant-ARM (Register arg-idx) arg))
                                                                                    (ARM->string (Move-Top-Constant-ARM (Register arg-idx) arg)))]
                                                      [else (ARM->string (Move-ARM (Register arg-idx) arg))]))
                                              (reverse args) (reverse (range 0 (length args)))))
                    (string-append "   bl " (symbol->string label) "\n")
                    (if (empty? targets) "" (ARM->string (Move-ARM (first targets) (Register 0)))))]
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
     (string-append "   ldr " (ARM-operand->string target) ", [" (ARM-operand->string source) "]\n")]
    [(Load-Stack-ARM target stack-offset base-pointer)
     (string-append "   ldr " (ARM-operand->string target) ", [" (symbol->string base-pointer)
                    (if (equal? stack-offset 0)
                        ""
                        (string-append ", #" (number->string stack-offset)))
                    "]\n")]
    [(Store-Reg-ARM source target)
     (string-append "   str " (ARM-operand->string source) ", [" (ARM-operand->string target) "]\n")]
    [(Store-Stack-ARM source stack-offset base-pointer)
     (string-append "   str " (ARM-operand->string source) ", [" (symbol->string base-pointer)
                    (if (equal? stack-offset 0)
                        ""
                        (string-append ", #" (number->string stack-offset)))
                    "]\n")]
    [(Branch-Equal-ARM block-label)
     (string-append "   beq " (ARM-label->string block-label) "\n")]
    [(Branch-Not-Equal-ARM block-label)
     (string-append "   bne " (ARM-label->string block-label) "\n")]
    [(Branch-ARM block-label)
     (string-append "   b " (ARM-label->string block-label) "\n")]
    [(Push-ARM registers)
     (string-append "   push {r" (number->string (first registers)) (apply string-append (map (lambda ([reg : Integer])
                                                                                                (string-append ", r" (number->string reg)))
                                                                                              (rest registers)))
                    "}\n")]
    [(Pop-ARM registers)
     (string-append "   pop {r" (number->string (first registers)) (apply string-append (map (lambda ([reg : Integer])
                                                                                                (string-append ", r" (number->string reg)))
                                                                                              (rest registers)))
                    "}\n")]))

;; convert an ARM block label to a string
(define (ARM-label->string [block-label : Integer]) : String
  (string-append ".LU" (number->string block-label)))

;; convert a global ARM declaration to a string
(define (global-ARM->string [global-dec : Global-ARM]) : String
  (string-append "   .comm " (symbol->string (Global-ARM-id global-dec)) ",4,4\n"))

;; convert ARM library functions to a string
(define (lib-funcs->string [lib-funcs : Lib-Funcs]) : String
  (match lib-funcs
    [(Lib-Funcs c-lib-funcs division)
     (match c-lib-funcs
       [(C-Lib-Funcs _ _ printf-newline printf-space scanf)
        (string-append
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
         (if scanf "   .comm .read_scratch,4,4\n" "")
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
    [(Intermediate value) (string-append "i" (number->string value))]
    [(Block _ _ _ _ _ _ _ _ _ _ _ _) (error 'ARM-operand->string "Unsupported operand type in ARM code: BLOCK")]
    [(Register num) (string-append "r" (number->string num))]
    [(Parameter num) (string-append "p" (number->string num))]
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
     (define intermediate (get-intermediate (CFG-intermediate-counter cfg)))
     (define next-intermediate (get-intermediate (CFG-intermediate-counter cfg)))
     (add-ARMs! cur-block (list (Move-Word-Constant-ARM intermediate (Global-id operand))
                                (Move-Top-Constant-ARM intermediate (Global-id operand))
                                (Load-Reg-ARM next-intermediate intermediate)))
     next-intermediate]
    [(Local? operand)
     (define intermediate (get-intermediate (CFG-intermediate-counter cfg)))
     (add-ARM! cur-block (Load-Stack-ARM intermediate (get-stack-offset operand cfg) 'fp))
     intermediate]
    [(integer? operand)
     (if (<= operand 4095)
         operand
         (block
          (define intermediate (get-intermediate (CFG-intermediate-counter cfg)))
          (add-ARM! cur-block (Move-Word-Lower-ARM intermediate operand))
          (add-ARM! cur-block (Move-Top-ARM intermediate operand))
          intermediate))]
    [else operand]))

;; get the next block index and increment the block counter
(define (get-block-counter) : Integer
  (define block-idx BLOCK_COUNTER)
  (set! BLOCK_COUNTER (+ 1 BLOCK_COUNTER))
  block-idx)

;; adds a list of ARM instructions to a block
(define (add-ARMs! [cur-block : Block] [ARMs : (Listof ARM-Instr)]) : Void
  (set-Block-ARM! cur-block (append (Block-ARM cur-block) ARMs)))

;; adds an ARM instruction to a block
(define (add-ARM! [cur-block : Block] [ARM : ARM-Instr]) : Void
  (set-Block-ARM! cur-block (append (Block-ARM cur-block) (list ARM))))

;; is this operand a literal? 
(define (literal? [operand : Operand]) : Boolean
  (or (integer? operand) (boolean? operand) (equal? operand 'null)))

;; is this ARM instruction a control flow instruction?
(define (ARM-branch-instr? [instr : ARM-Instr]) : Boolean
  (or (Branch-Equal-ARM? instr) (Branch-Not-Equal-ARM? instr) (Branch-ARM? instr)))

;; MILESTONE 5 FUNCTIONS

;; give the option to turn off register allocation for debugging
(define REGISTER_ALLOCATION? : Boolean #t)
(define DEBUGGING : Boolean #f)

;; use untyped racket so that I can use the heap data structure without difficulty
(module color-graph racket
  (require "definitions.rkt")
  (require data/heap)
  (provide color-graph!)
  (provide UNCHOSEN_COLOR)
  (provide NUM_OPEN_REGISTERS)

  (define DEBUGGING #f)
  (define UNCHOSEN_COLOR -1)
  (define FIRST_REG_NUM 0)
  (define LAST_REG_NUM 8)
  (define NUM_OPEN_REGISTERS 9)

  ;; color the interference graph to assign intermediates to registers
  (define (color-graph! interference-graph cfg)
    (define priority-queue (make-heap I-Node-degree<=))
    (heap-add-all! priority-queue (hash-values interference-graph))
    (define stack (build-stack priority-queue '()))
    (color-nodes! stack interference-graph cfg)
    (cond [DEBUGGING (displayln (CFG-func-name cfg))
                     (for-each (lambda (node)
                                 (displayln node))
                               (hash-values interference-graph))]))

  ;; build the stack to determine the order to color the nodes
  (define (build-stack priority-queue stack)
    (cond [(not (equal? (heap-count priority-queue) 0))
           (define node (choose-node priority-queue))
           (remove-node priority-queue node)
           (build-stack priority-queue (cons node stack))]
          [else stack]))

  ;; color each node of the graph
  (define (color-nodes! stack interference-graph cfg)
    (cond [(not (empty? stack))
           (define node (first stack))
           (cond [(equal? (I-Node-color node) UNCHOSEN_COLOR)
                  (color-node! node interference-graph cfg)])
           (color-nodes! (rest stack) interference-graph cfg)]))

  ;; color a single node in the interference graph
  (define (color-node! node interference-graph cfg)
    (define interfering-colors (set))
    (for-each (lambda (operand)
                (define color (I-Node-color (hash-ref interference-graph operand)))
                (cond [(not (equal? color UNCHOSEN_COLOR))
                       (set! interfering-colors (set-add interfering-colors color))]))
              (set->list (I-Node-interferences node)))
    (cond [(Parameter? (I-Node-operand node))
           (cond [(< (Parameter-num (I-Node-operand node)) 4)
                  (cond [(not (set-member? interfering-colors (Parameter-num (I-Node-operand node))))
                         (set-I-Node-color! node (Parameter-num (I-Node-operand node)))]
                        [else (set-I-Node-insert-move! node #t)
                              (set-color! node interfering-colors FIRST_REG_NUM interference-graph cfg)])])]
          [else (set-color! node interfering-colors FIRST_REG_NUM interference-graph cfg)]))

  ;; set the color of a node to the first available color
  (define (set-color! node interfering-colors try-color interference-graph cfg)
    (cond [(set-member? interfering-colors try-color)
           (define next-color (+ 1 try-color))
           (if (<= next-color LAST_REG_NUM)
               (set-color! node interfering-colors next-color interference-graph cfg)
               (spill-register! node interference-graph))]
          [else (set-I-Node-color! node try-color)
                (define saved-registers (CFG-saved-registers cfg))
                (cond [(and (>= try-color 4) (not (member try-color saved-registers)))
                       (set-CFG-saved-registers! cfg (append saved-registers (list try-color)))])]))

  ;; mark a register as spilled and remove all of its interferences from the graph
  (define (spill-register! node interference-graph)
    (set-I-Node-spilled! node #t)
    (set-I-Node-interferences! node (set))
    (for-each (lambda (other-node)
                (define interferences (I-Node-interferences other-node))
                (cond [(set-member? interferences node)
                       (set-I-Node-interferences! other-node (set-remove interferences node))
                       (set-I-Node-degree! other-node (- (I-Node-degree other-node) 1))]))
              (hash-values interference-graph)))

  ;; choose which node to place on the stack next
  ;; if possible, choose an unconstrained node
  (define (choose-node priority-queue)
    (define least-degree-node (heap-min priority-queue))
    (cond [(< (I-Node-degree least-degree-node) NUM_OPEN_REGISTERS)
           least-degree-node]
          [else (choose-constrained-node priority-queue)]))

  ;; choose the node with the lowest cost/degree ratio to place on the stack since we already know a node will be spilled
  (define (choose-constrained-node priority-queue)
    (define lowest-ratio -1)
    (define constrained-node (heap-min priority-queue))
    (for-each (lambda (node)
                (cond [(not (equal? (I-Node-degree node) 0))
                       (define cost-degree-ratio (/ (I-Node-spill-cost node) (I-Node-degree node)))
                       (cond [(or (< cost-degree-ratio lowest-ratio) (equal? lowest-ratio -1))
                              (set! constrained-node node)
                              (set! lowest-ratio cost-degree-ratio)])]))
              (vector->list (heap->vector priority-queue)))
    constrained-node)

  ;; remove a node from the priority queue and remove its interferences from the graph
  (define (remove-node priority-queue node)
    (heap-remove! priority-queue node)
    (define cur-operand (I-Node-operand node))
    (for-each (lambda (other-node)
                (define interferences (I-Node-interferences other-node))
                (cond [(set-member? interferences cur-operand)
                       ;;(set-I-Node-interferences! other-node (set-remove interferences cur-operand))
                       (set-I-Node-degree! other-node (- (I-Node-degree other-node) 1))]))
              (vector->list (heap->vector priority-queue))))

  ;; determine the ordering of nodes in the priority queue by degree from least to greatest
  (define (I-Node-degree<= node1 node2)
    (<= (I-Node-degree node1) (I-Node-degree node2))))

;; use the untyped 'color-graph racket module (created above) within typed racket
(require/typed 'color-graph
               [color-graph! (Interference-Graph CFG -> Void)]
               [UNCHOSEN_COLOR Integer]
               [NUM_OPEN_REGISTERS Integer])
;; use untyped racket sets in typed racket
(require/typed racket/set
               [set-add (-> (Setof Operand) Operand (Setof Operand))])

;; allocate registers for the ARM assembly
(define (allocate-registers! [cfg : CFG]) : Void
  (match cfg
    [(CFG _ parameters _ blocks _ _ _ _ _)
     (define sorted-blocks (filter Block-reachable (sort (hash-values blocks) block<)))
     (for-each (lambda ([cur-block : Block])
                 (cond [(Block-reachable cur-block)
                        (gather-live-info! cur-block (Block-ARM cur-block))]))
               sorted-blocks)
     (propagate-live-info! sorted-blocks)
     (define interference-graph (create-interference-graph sorted-blocks))
     (color-graph! interference-graph cfg)
     (cond [DEBUGGING (displayln "Gen:")
                      (for-each (lambda ([cur-block : Block])
                                  (display "Block ")
                                  (display (Block-label cur-block))
                                  (display "- ")
                                  (displayln (bitset->operands (Live-Analysis-gen (Block-live cur-block)))))
                                sorted-blocks)
                      (displayln "Kill:")
                      (for-each (lambda ([cur-block : Block])
                                  (display "Block ")
                                  (display (Block-label cur-block))
                                  (display "- ")
                                  (displayln (bitset->operands (Live-Analysis-kill (Block-live cur-block)))))
                                sorted-blocks)
                      (displayln "Out:")])
     (define entry-block (hash-ref blocks 'entry-block))
     (set-Block-allocated-ARM! entry-block (get-param-moves interference-graph cfg))
     (for-each (lambda ([cur-block : Block])
                 (cond [DEBUGGING
                        (display "Block ")
                        (display (Block-label cur-block))
                        (display "- ")
                        (displayln (bitset->operands (Live-Analysis-out (Block-live cur-block))))])
                 (set-Block-allocated-ARM! cur-block (append (Block-allocated-ARM cur-block)
                                                             (rewrite-with-registers! (Block-ARM cur-block) interference-graph cfg))))
               sorted-blocks)]))

;; gather the local live info for each basic block
(define (gather-live-info! [cur-block : Block] [ARMs : (Listof ARM-Instr)]) : Void
  (cond [(not (empty? ARMs))
         (define ARM (first ARMs))
         (for-each (lambda ([source : Operand])
                     (define bit-index (operand->bit-index source))
                     (cond [(not (bitwise-bit-set? (Bitset-bits (Live-Analysis-kill (Block-live cur-block))) bit-index))
                            (set-live! cur-block 'gen (bitwise-ior (Bitset-bits (Live-Analysis-gen (Block-live cur-block)))
                                                                   (arithmetic-shift 1 bit-index)))]))
                   (relevant-coloring-operands (get-ARM-sources ARM)))
         (for-each (lambda ([target : Operand])
                     (define bit-index (operand->bit-index target))
                     (set-live! cur-block 'kill (bitwise-ior (Bitset-bits (Live-Analysis-kill (Block-live cur-block)))
                                                             (arithmetic-shift 1 bit-index))))
                   (relevant-coloring-operands (get-ARM-targets ARM)))
         (gather-live-info! cur-block (rest ARMs))]))

;; propagate the live info between blocks
(define (propagate-live-info! [sorted-blocks : (Listof Block)]) : Void
  (define info-changed? : Boolean #f)
  (for-each (lambda ([cur-block : Block])
              (define live-out (get-live-out cur-block))
              (cond [(not (equal? live-out (Bitset-bits (Live-Analysis-out (Block-live cur-block)))))
                     (set-live! cur-block 'out live-out)
                     (set! info-changed? #t)]))
            sorted-blocks)
  (cond [info-changed? (propagate-live-info! sorted-blocks)]))

;; get the live out registers for a single block
(define (get-live-out [cur-block : Block]) : Integer
  (finite-union (cons 0 (map (lambda ([successor : Block])
                               (bitwise-ior (Bitset-bits (Live-Analysis-gen (Block-live successor)))
                                            (bitwise-and (Bitset-bits (Live-Analysis-out (Block-live successor)))
                                                         (bitwise-not (Bitset-bits (Live-Analysis-kill (Block-live successor)))))))
                             (filter Block-reachable (Block-successors cur-block))))))

;; perform the finite union of a list of bits (integers)
(define (finite-union [bits : (Listof Integer)]) : Integer
  (if (empty? (rest bits))
      (first bits)
      (bitwise-ior (first bits) (finite-union (rest bits)))))

;; get the bit index of an operand 
(define (operand->bit-index [operand : Operand]) : Integer
  (match operand
    [(Register num) num]
    [(Parameter num) (+ 4 num)]
    [(Intermediate num) (+ 8 num)]
    [else (error "Unsupported behavior for graph coloring - operand ~e is not of the supported types." operand)]))

;; convert a bit index to the corresponding operand
(define (bit-index->operand [bit-index : Integer]) : Operand
  (cond
    [(< bit-index 4)
     (Register bit-index)]
    [(< bit-index 8)
     (Parameter (- bit-index 4))]
    [else (Intermediate (- bit-index 8))]))

;; convert a bitset representation to the corresponding operands 
(define (bitset->operands [bitset : Bitset]) : (Setof Operand)
  (define operands : (Setof Operand) (set))
  (for-each (lambda ([bit-index : Integer])
              (cond [(bitwise-bit-set? (Bitset-bits bitset) bit-index)
                     (set! operands (set-add operands (bit-index->operand bit-index)))]))
            (range (integer-length (Bitset-bits bitset))))
  operands)

;; create the interference graph by iterating through the instructions from the bottom up
(define (create-interference-graph [sorted-blocks : (Listof Block)]) : Interference-Graph
  (define graph (ann (make-hash '()) Interference-Graph))
  (for-each (lambda ([cur-block : Block])
              (define live-now (Bitset (Bitset-bits (Live-Analysis-out (Block-live cur-block)))))
              (for-each (lambda ([instr : ARM-Instr])
                          (create-interferences! instr graph live-now cur-block))
                        (reverse (Block-ARM cur-block)))
              (define live-now-registers (set->list (bitset->operands live-now)))
              (add-interference-edges! live-now-registers live-now-registers cur-block graph))
            sorted-blocks)
  graph)

;; create the interferences associated with a single ARM instruction
(define (create-interferences! [ARM : ARM-Instr] [graph : Interference-Graph] [live-now : Bitset] [cur-block : Block]) : Void
  (cond [DEBUGGING 
         (displayln "")
         (display "ARM Instruction:    ")
         (displayln ARM)])
  (define live-now-registers (set->list (bitset->operands live-now)))
  (cond [DEBUGGING 
         (display "Live now registers:   ")
         (displayln live-now-registers)])
  (define sources (relevant-coloring-operands (get-ARM-sources ARM)))
  (cond [DEBUGGING 
         (display "Sources:   ")
         (displayln sources)])
  (define targets (relevant-coloring-operands (get-ARM-targets ARM)))
  (cond [DEBUGGING 
         (display "Targets:   ")
         (displayln targets)])
  (add-interference-edges! live-now-registers targets cur-block graph)
  (add-interference-edges! targets live-now-registers cur-block graph)
  (cond [DEBUGGING 
         (display "Live now:    ")
         (displayln (set->list (bitset->operands live-now)))])
  (clear-bits! live-now targets)
  (cond [DEBUGGING 
         (display "Live now after cleared bits:    ")
         (displayln (set->list (bitset->operands live-now)))])
  (set-bits! live-now sources)
  (cond [DEBUGGING 
         (display "Live now after set bits:    ")
         (displayln (set->list (bitset->operands live-now)))])
  (for-each (lambda ([source : Operand])
              (cond [(not (hash-has-key? graph source))
                     (define color (if (Register? source)
                                       (Register-num source)
                                       UNCHOSEN_COLOR))
                     (hash-set! graph source (I-Node source (set) 0 color #f (Block-execution-frequency cur-block) #f))]))
            sources)
  (cond [(Branch-Label-ARM? ARM)
         (for-each (lambda ([arg : Operand])
                     (define node (hash-ref graph arg))
                     (set-I-Node-interferences! node (set-add (set-add (set-add (set-add (I-Node-interferences node)
                                                                                         (Register 0))
                                                                                (Register 1))
                                                                       (Register 2))
                                                              (Register 3))))
                   (relevant-coloring-operands (non-constant-args (Branch-Label-ARM-args ARM))))]))

;; clear the bits in the bitset corresponding to the given operands
(define (clear-bits! [bitset : Bitset] [operands : (Listof Operand)]) : Void
  (for-each (lambda ([bit-index : Integer])
              (define length (integer-length (Bitset-bits bitset)))
              (define all-ones (- (expt 2 length) 1))
              (define bitmask (- all-ones (arithmetic-shift 1 bit-index)))
              (set-Bitset-bits! bitset (bitwise-and (Bitset-bits bitset) bitmask)))
            (map (lambda ([operand : Operand])
                   (operand->bit-index operand))
                 operands)))

;; set the bits in the bitset corresponding to the given operands
(define (set-bits! [bitset : Bitset] [operands : (Listof Operand)]) : Void
  (for-each (lambda ([bit-index : Integer])
              (define bitmask (arithmetic-shift 1 bit-index))
              (set-Bitset-bits! bitset (bitwise-ior (Bitset-bits bitset) bitmask)))
            (map (lambda ([operand : Operand])
                   (operand->bit-index operand))
                 operands)))

;; add interferences edges from the given from-operands to the given to-operands
(define (add-interference-edges! [from-operands : (Listof Operand)] [to-operands : (Listof Operand)] [cur-block : Block] [graph : Interference-Graph]) : Void
  (for-each (lambda ([source : Operand])
              (define interferes-with (filter (lambda ([to-operand : Operand])
                                                (not (equal? source to-operand)))
                                              to-operands))
              (cond [(hash-has-key? graph source)
                     (define node (hash-ref graph source))
                     (set-I-Node-interferences! node (set-adds (I-Node-interferences node) interferes-with))
                     (cond [DEBUGGING 
                            (display "Adding ")
                            (display interferes-with)
                            (display " as interferences to ")
                            (display source)
                            (display ":    ")
                            (displayln (I-Node-interferences node))])
                     (set-I-Node-degree! node (set-count (I-Node-interferences node)))]
                    [else (define interferences (list->set interferes-with))
                          (define color (if (Register? source)
                                            (Register-num source)
                                            UNCHOSEN_COLOR))
                          (define node (I-Node source interferences (set-count interferences) color #f (Block-execution-frequency cur-block) #f))
                          (hash-set! graph source node)]))
            from-operands))

;; set the bits for the current block of the corresponding type of live range analysis 
(define (set-live! [cur-block : Block] [analysis-type : Symbol] [num : Integer]) : Void
  (define live (Block-live cur-block))
  (cond [(equal? analysis-type 'gen) (set-Bitset-bits! (Live-Analysis-gen live) num)]
        [(equal? analysis-type 'kill) (set-Bitset-bits! (Live-Analysis-kill live) num)]
        [(equal? analysis-type 'out) (set-Bitset-bits! (Live-Analysis-out live) num)]))

;; rewrite each instruction by converting intermediates to their corresponding registers
(define (rewrite-with-registers! [ARM : (Listof ARM-Instr)] [interference-graph : Interference-Graph] [cfg : CFG]) : (Listof ARM-Instr)
  (cond [(empty? ARM) '()]
        [else
         (define memory-access (Memory-Access '() '()))
         (match (first ARM)
           [(Arith-ARM operator target left right)
            (define new-operands (rewrite-operands (list target) (list left right) interference-graph memory-access cfg))
            (append (Memory-Access-loads memory-access)
                    (list (Arith-ARM operator (first new-operands) (second new-operands) (third new-operands)))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Bool-ARM operator target left right)
            (define new-operands (rewrite-operands (list target) (list left right) interference-graph memory-access cfg))
            (append (Memory-Access-loads memory-access)
                    (list (Bool-ARM operator (first new-operands) (second new-operands) (third new-operands)))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Move-ARM target source)
            (define new-operands (rewrite-operands (list target) (list source) interference-graph memory-access cfg))
            (append (Memory-Access-loads memory-access)
                    (list (Move-ARM (first new-operands) (second new-operands)))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Branch-Label-ARM label args targets store-stack-ARMs)
            (define new-args (map (lambda ([arg : (U Operand Symbol)])
                                    (if (symbol? arg)
                                        arg
                                        (first (rewrite-operands '() (list arg) interference-graph memory-access cfg))))
                                  args))
            (define new-targets (rewrite-operands targets '() interference-graph memory-access cfg))
            (define new-store-stack-ARMs (map (lambda ([store-ARM : Store-Stack-ARM])
                                                (Store-Stack-ARM (first (rewrite-operands '() (list (Store-Stack-ARM-source store-ARM)) interference-graph memory-access cfg))
                                                                 (Store-Stack-ARM-stack-offset store-ARM) (Store-Stack-ARM-base-pointer store-ARM)))
                                              store-stack-ARMs))
            (append (Memory-Access-loads memory-access)
                    (list (Branch-Label-ARM label new-args new-targets new-store-stack-ARMs))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Comp-ARM left right)
            (define new-operands (rewrite-operands '() (list left right) interference-graph memory-access cfg))
            (append (Memory-Access-loads memory-access)
                    (list (Comp-ARM (first new-operands) (second new-operands)))
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Move-Cond-ARM operator target source)
            (define new-operands (rewrite-operands (list target) (list source) interference-graph memory-access cfg))
            (append (Memory-Access-loads memory-access)
                    (list (Move-Cond-ARM operator (first new-operands) (second new-operands)))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Move-Word-ARM target source)
            (define new-operands (rewrite-operands (list target) (list source) interference-graph memory-access cfg))
            (append (Memory-Access-loads memory-access)
                    (list (Move-Word-ARM (first new-operands) (second new-operands)))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Move-Word-Lower-ARM target num)
            (define new-operands (rewrite-operands (list target) '() interference-graph memory-access cfg))
            (append (list (Move-Word-Lower-ARM (first new-operands) num))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Move-Word-Constant-ARM target constant)
            (define new-operands (rewrite-operands (list target) '() interference-graph memory-access cfg))
            (append (list (Move-Word-Constant-ARM (first new-operands) constant))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Move-Top-ARM target num)
            (define new-operands (rewrite-operands (list target) '() interference-graph memory-access cfg))
            (append (list (Move-Top-ARM (first new-operands) num))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Move-Top-Constant-ARM target constant)
            (define new-operands (rewrite-operands (list target) '() interference-graph memory-access cfg))
            (append (list (Move-Top-Constant-ARM (first new-operands) constant))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Load-Reg-ARM target source)
            (define new-operands (rewrite-operands (list target) (list source) interference-graph memory-access cfg))
            (append (Memory-Access-loads memory-access)
                    (list (Load-Reg-ARM (first new-operands) (second new-operands)))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Load-Stack-ARM target stack-offset base-pointer)
            (define new-operands (rewrite-operands (list target) '() interference-graph memory-access cfg))
            (append (list (Load-Stack-ARM (first new-operands) stack-offset base-pointer))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Store-Reg-ARM source target)
            (define new-operands (rewrite-operands (list target) (list source) interference-graph memory-access cfg))
            (append (Memory-Access-loads memory-access)
                    (list (Store-Reg-ARM (second new-operands) (first new-operands)))
                    (Memory-Access-stores memory-access)
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Store-Stack-ARM source stack-offset base-pointer)
            (define new-operands (rewrite-operands '() (list source) interference-graph memory-access cfg))
            (append (Memory-Access-loads memory-access)
                    (list (Store-Stack-ARM (first new-operands) stack-offset base-pointer))
                    (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Branch-Equal-ARM block-label)
            (cons (Branch-Equal-ARM block-label)
                  (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Branch-Not-Equal-ARM block-label)
            (cons (Branch-Not-Equal-ARM block-label)
                  (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Branch-ARM block-label)
            (cons (Branch-ARM block-label)
                  (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Push-ARM registers)
            (cons (Push-ARM registers)
                  (rewrite-with-registers! (rest ARM) interference-graph cfg))]
           [(Pop-ARM registers)
            (cons (Pop-ARM registers)
                  (rewrite-with-registers! (rest ARM) interference-graph cfg))])]))

;; rewrite a list of operands to registers based on their colors in the interference graph
(define (rewrite-operands [targets : (Listof Operand)] [sources : (Listof Operand)] [graph : Interference-Graph] [memory-access : Memory-Access] [cfg : CFG]) : (Listof Operand)
  (append
   (map (lambda ([target : Operand])
          (cond [(or (literal? target) (Register? target)) target]
                [(and (Parameter? target) (>= (Parameter-num target) 4))
                 (define spill-register (first-spill-register cfg))
                 (set-Memory-Access-stores! memory-access (list (Store-Stack-ARM spill-register (get-arg-stack-offset (Parameter-num target)) 'fp)))
                 spill-register]
                [(hash-has-key? graph target)
                 (define node (hash-ref graph target))
                 (cond [(I-Node-spilled node)
                        (define spill-register (first-spill-register cfg))
                        (set-Memory-Access-stores! memory-access (list (Store-Stack-ARM spill-register (get-spill-stack-offset target cfg) 'sp)))
                        spill-register]
                       [else (Register (I-Node-color (hash-ref graph target)))])]
                [else (error 'rewrite-operands "Unsupported behavior - target ~e not in interference-graph ~e" target graph)]))
        targets)
   (map (lambda ([source : Operand])
          (define cur-loads (Memory-Access-loads memory-access))
          (cond [(or (literal? source) (Register? source)) source]
                [(and (Parameter? source) (>= (Parameter-num source) 4))
                 (define spill-register (if (empty? cur-loads) (first-spill-register cfg) (second-spill-register cfg)))
                 (set-Memory-Access-loads! memory-access (append cur-loads (list (Load-Stack-ARM spill-register (get-arg-stack-offset (Parameter-num source)) 'fp))))
                 spill-register]
                [(hash-has-key? graph source)
                 (define node (hash-ref graph source))
                 (cond [(I-Node-spilled node)
                        (define spill-register (if (empty? cur-loads) (first-spill-register cfg) (second-spill-register cfg)))
                        (set-Memory-Access-loads! memory-access (append cur-loads (list (Load-Stack-ARM spill-register (get-spill-stack-offset source cfg) 'sp))))
                        spill-register]
                       [else (Register (I-Node-color (hash-ref graph source)))])]
                [else (error 'rewrite-operands "Unsupported behavior - source ~e not in interference-graph ~e" source graph)]))
        sources)))

;; the first designated spill register
(define (first-spill-register [cfg : CFG]) : Operand
  (define saved-registers (CFG-saved-registers cfg))
  (cond [(not (member 9 saved-registers)) (set-CFG-saved-registers! cfg (append saved-registers (list 9)))])
  (Register 9))

;; the second designated spill register
(define (second-spill-register [cfg : CFG]) : Operand
  (define saved-registers (CFG-saved-registers cfg))
  (cond [(not (member 10 saved-registers)) (set-CFG-saved-registers! cfg (append saved-registers (list 10)))])
  (Register 10))

;; add a list of operands to a set of operands
(define (set-adds [set : (Setof Operand)] [to-add : (Listof Operand)]) : (Setof Operand)
  (define new-set set)
  (for-each (lambda ([operand : Operand])
              (set! new-set (set-add new-set operand)))
            to-add)
  new-set)

;; get the source operands of an ARM instruction
(define (get-ARM-sources [ARM : ARM-Instr]) : (Listof Operand)
  (match ARM
    [(Arith-ARM operator target left right) (list left right)]
    [(Bool-ARM operator target left right) (list left right)]
    [(Move-ARM target source) (list source)]
    [(Branch-Label-ARM label args targets store-stack-ARMs)
     (append (non-constant-args args)
             (map Store-Stack-ARM-source store-stack-ARMs))]
    [(Comp-ARM left right) (list left right)]
    [(Move-Cond-ARM operator target source) (list source)]
    [(Move-Word-ARM target source) (list source)]
    [(Move-Word-Lower-ARM target source) (list source)]
    [(Move-Word-Constant-ARM target constant) '()]
    [(Move-Top-ARM target source) (list source)]
    [(Move-Top-Constant-ARM target constant) '()]
    [(Load-Reg-ARM target source) (list source)]
    [(Load-Stack-ARM target stack-offset base-pointer) '()]
    [(Store-Reg-ARM source target) (list source target)] 
    [(Store-Stack-ARM source stack-offset base-pointer) (list source)]
    [(Branch-Equal-ARM block-label) '()]
    [(Branch-Not-Equal-ARM block-label) '()]
    [(Branch-ARM block-label) '()]
    [(Push-ARM registers) '()]
    [(Pop-ARM registers) '()]))

;; get the target operands of an ARM instruction
(define (get-ARM-targets [ARM : ARM-Instr]) : (Listof Operand)
  (match ARM
    [(Arith-ARM operator target left right) (list target)]
    [(Bool-ARM operator target left right) (list target)]
    [(Move-ARM target source) (list target)]
    [(Branch-Label-ARM label args targets store-stack-ARMs)
     (append targets (list (Register 0) (Register 1) (Register 2) (Register 3)))]
    [(Comp-ARM left right) '()]
    [(Move-Cond-ARM operator target source) (list target)]
    [(Move-Word-ARM target source) (list target)]
    [(Move-Word-Lower-ARM target source) (list target)]
    [(Move-Word-Constant-ARM target source) (list target)]
    [(Move-Top-ARM target source) (list target)]
    [(Move-Top-Constant-ARM target source) (list target)]
    [(Load-Reg-ARM target source) (list target)]
    [(Load-Stack-ARM target stack-offset base-pointer) (list target)]
    [(Store-Reg-ARM source target) '()]
    [(Store-Stack-ARM source stack-offset base-pointer) '()]
    [(Branch-Equal-ARM block-label) '()]
    [(Branch-Not-Equal-ARM block-label) '()]
    [(Branch-ARM block-label) '()]
    [(Push-ARM registers) '()]
    [(Pop-ARM registers) '()]))

;; set register allocation flag
(define (set-register-allocation! [register-allocation : String]) : Void
  (cond
    [(equal? register-allocation "true") (set! REGISTER_ALLOCATION? #t)]
    [(equal? register-allocation "false") (set! REGISTER_ALLOCATION? #f)]))

;; set register allocation flag
(define (set-useless-code-elimination! [useless-code-elimination : String]) : Void
  (cond
    [(equal? useless-code-elimination "true") (set! USELESS_CODE_ELIMINATION? #t)]
    [(equal? useless-code-elimination "false") (set! USELESS_CODE_ELIMINATION? #f)]))

;; get the non-constant arguments to a function call
(define (non-constant-args [args : (Listof (U Operand Symbol))]) : (Listof Operand)
  (define non-constants : (Listof Operand) '())
  (for-each (lambda ([arg : (U Operand Symbol)])
              (cond [(not (symbol? arg))
                     (set! non-constants (cons arg non-constants))]))
            (reverse args))
  non-constants)

;; get the operands that are relevant to graph coloring
(define (relevant-coloring-operands [operands : (Listof Operand)]) : (Listof Operand)
  (filter (lambda ([operand : Operand])
            (or (Intermediate? operand) (Parameter? operand) (Register? operand)))
          operands))

;; get the parameter moves to temporary registers at the beginning of each function (if necessary)
(define (get-param-moves [graph : Interference-Graph] [cfg : CFG]) : (Listof ARM-Instr)
  (define moves : (Listof ARM-Instr) '())
  (for-each (lambda ([node : I-Node])
              (define operand (I-Node-operand node))
              (cond [(and (Parameter? operand)
                          (I-Node-insert-move node))
                     (if (I-Node-spilled node)
                         (set! moves (append moves (list (Store-Stack-ARM (Register (Parameter-num operand)) (get-spill-stack-offset operand cfg) 'sp))))
                         (set! moves (append moves (list (Move-ARM (Register (I-Node-color node)) (Register (Parameter-num operand)))))))]))
            (hash-values graph))
  moves)

;; MILESTONE 6 FUNCTIONS

(define USELESS_CODE_ELIMINATION? : Boolean #t)
(define UNDEFINED_BLOCK -1)

;; run transformation passes to eliminate useless code (SSA-based unused result algorithm)
(define (useless-code-elimination! [blocks : Block-Table]) : Void
  (define sorted-blocks (filter Block-reachable (sort (hash-values blocks) block<)))
  (define use-defs (get-defs-and-uses sorted-blocks))
  (eliminate-useless-code! blocks use-defs))

;; keep iterating to remove useless code until nothing gets removed
(define (eliminate-useless-code! [blocks : Block-Table] [use-defs : Use-Def-Table]) : Void
  (define code-removed? : Boolean #f)
  (for-each (lambda ([info : Use-Def-Info])
              (cond [(not (empty? (Use-Def-Info-def-LLVM info)))
                     (define LLVM-to-remove (first (Use-Def-Info-def-LLVM info)))
                     (cond [(and (empty? (Use-Def-Info-use-LLVMs info)) (not (has-side-effects? LLVM-to-remove)))
                            (define def-block (if (hash-has-key? blocks (Use-Def-Info-def-block-label info))
                                                  (hash-ref blocks (Use-Def-Info-def-block-label info))
                                                  (if (equal? (Block-label (hash-ref blocks 'entry-block)) (Use-Def-Info-def-block-label info))
                                                      (hash-ref blocks 'entry-block)
                                                      (error 'eliminate-useless-code "Unsupported behavior - block ~e not in block table"
                                                             (Use-Def-Info-def-block-label info)))))
                            (set-Block-LLVM! def-block (remove LLVM-to-remove (Block-LLVM def-block)))
                            (for-each (lambda ([source : Operand])
                                        (define source-info (hash-ref use-defs source))
                                        (set-Use-Def-Info-use-LLVMs! source-info (remove LLVM-to-remove (Use-Def-Info-use-LLVMs source-info))))
                                      (filter relevant-useless-code-operand (get-LLVM-sources LLVM-to-remove)))
                            (hash-remove! use-defs (Use-Def-Info-operand info))
                            (set! code-removed? #t)])]))
            (hash-values use-defs))
  (cond [code-removed? (eliminate-useless-code! blocks use-defs)]))

;; does this instruction have side effects (ie. it should NOT be removed)
(define (has-side-effects? [LLVM : LLVM-Instr]) : Boolean
    (match LLVM
    [(Alloca-LLVM type target) #f]
    [(Arith-LLVM operator left right target) #f]
    [(Bool-LLVM operator left right target) #f]
    [(Comp-LLVM operator type left right target) #f]
    [(Size-LLVM operator operand from to target) #f]
    [(Get-Elm-Ptr-LLVM type pointer field-offset target) #f]
    [(Call-LLVM func-name return-type arg-types args target) #t]
    [(Call-Void-LLVM func-name arg-types args) #t]
    [(Malloc-LLVM target size) #f]
    [(Free-LLVM source) #f]
    [(Load-LLVM type source target indir) #f]
    [(Store-LLVM type source target indir) #t]
    [(Print-LLVM operand endl target) #t]
    [(Scan-LLVM var result) #t]
    [(Branch-LLVM block) #f]
    [(Cond-Branch-LLVM cond true false) #f]
    [(Return-Expr-LLVM type expr) #f]
    [(Return-Void-LLVM) #f]
    [(Phi-LLVM _ type values target _) #f]
    [(Move-LLVM target source) #f]))

;; get the definitions and uses of all operands in a function
(define (get-defs-and-uses [sorted-blocks : (Listof Block)]) : Use-Def-Table
  (define use-defs (ann (make-hash '()) Use-Def-Table))
  (for-each (lambda ([cur-block : Block])
              (map (lambda ([LLVM : LLVM-Instr])
                     (map (lambda ([target : Operand])
                            (cond [(not (hash-has-key? use-defs target))
                                   (hash-set! use-defs target (Use-Def-Info target (Block-label cur-block) (list LLVM) '()))]
                                  [else (define info (hash-ref use-defs target))
                                        (cond [(equal? (Use-Def-Info-def-block-label info) UNDEFINED_BLOCK)
                                               (set-Use-Def-Info-def-block-label! info (Block-label cur-block))
                                               (set-Use-Def-Info-def-LLVM! info (list LLVM))]
                                              [else (error "Redefinition of target ~e not allowed in SSA form" target)])]))
                          (filter relevant-useless-code-operand (get-LLVM-targets LLVM)))
                     (map (lambda ([source : Operand])
                            (cond [(hash-has-key? use-defs source)
                                   (define use-def-info (hash-ref use-defs source))
                                   (set-Use-Def-Info-use-LLVMs! use-def-info (cons LLVM (Use-Def-Info-use-LLVMs use-def-info)))]
                                  [else (hash-set! use-defs source (Use-Def-Info source UNDEFINED_BLOCK '() (list LLVM)))]))
                          (filter relevant-useless-code-operand (get-LLVM-sources LLVM))))
                   (Block-LLVM cur-block)))
            sorted-blocks)
  use-defs)

;; get the targets of an LLVM instruction
(define (get-LLVM-targets [LLVM : LLVM-Instr]) : (Listof Operand)
  (match LLVM
    [(Alloca-LLVM type target) (list target)]
    [(Arith-LLVM operator left right target) (list target)]
    [(Bool-LLVM operator left right target) (list target)]
    [(Comp-LLVM operator type left right target) (list target)]
    [(Size-LLVM operator operand from to target) (list target)]
    [(Get-Elm-Ptr-LLVM type pointer field-offset target) (list target)]
    [(Call-LLVM func-name return-type arg-types args target) (list target)]
    [(Call-Void-LLVM func-name arg-types args) '()]
    [(Malloc-LLVM target size) (list target)]
    [(Free-LLVM source) '()]
    [(Load-LLVM type source target indir) (list target)]
    [(Store-LLVM type source target indir) '()]
    [(Print-LLVM operand endl target) '()]
    [(Scan-LLVM var result) '()]
    [(Branch-LLVM block) '()]
    [(Cond-Branch-LLVM cond true false) '()]
    [(Return-Expr-LLVM type expr) '()]
    [(Return-Void-LLVM) '()]
    [(Phi-LLVM _ type values target _) (list target)]
    [(Move-LLVM target source) (list target)]))

;; get the sources of an LLVM instruction
(define (get-LLVM-sources [LLVM : LLVM-Instr]) : (Listof Operand)
  (match LLVM
    [(Alloca-LLVM type target) '()]
    [(Arith-LLVM operator left right target) (list left right)]
    [(Bool-LLVM operator left right target) (list left right)]
    [(Comp-LLVM operator type left right target) (list left right)]
    [(Size-LLVM operator operand from to target) (list operand)]
    [(Get-Elm-Ptr-LLVM type pointer field-offset target) (list pointer)]
    [(Call-LLVM func-name return-type arg-types args target) args]
    [(Call-Void-LLVM func-name arg-types args) args]
    [(Malloc-LLVM target size) '()]
    [(Free-LLVM source) (list source)]
    [(Load-LLVM type source target indir) (list source)]
    [(Store-LLVM type source target indir) (list source target)]
    [(Print-LLVM operand endl target) (list operand)]
    [(Scan-LLVM var result) (list var)]
    [(Branch-LLVM block) '()]
    [(Cond-Branch-LLVM cond true false) (list cond)]
    [(Return-Expr-LLVM type expr) (list expr)]
    [(Return-Void-LLVM) '()]
    [(Phi-LLVM _ type values target _) (map Phi-Value-operand values)]
    [(Move-LLVM target source) (list source)]))

;; these are the only operands we want to consider when doing useless code elimination
(define (relevant-useless-code-operand [operand : Operand]) : Boolean
  (or (Local? operand) (Intermediate? operand)))

;; is this edge a back edge? I've performed block numbering such that this is apparent
(define (back-edge? [cur-block : Block] [next-block : Block]) : Boolean
  (>= (Block-label cur-block) (Block-label next-block)))