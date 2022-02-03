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
  (LLVM-Program->string LLVM-Program))

;; the compiler (takes in the json representation of the program)
(define (compile-json [json-str : String]) : String
  (define AST (parse-str json-str))
  (define environment (type-check-program AST))
  (define LLVM-Program (create-LLVM (Program-functions AST) environment))
  (LLVM-Program->string LLVM-Program))

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
     (define structs (bind-structs types '()))
     (define global-variables (bind-declarations declarations (map Struct-Binding-name structs)))
     (define environment (type-check-functions functions (Environment structs global-variables '())))
     (check-func-names-unique (map Function-Binding-name (Environment-functions environment)))
     (check-valid-main (Environment-functions environment))
     environment]))

;; checks for redeclarations of function names
(define (check-func-names-unique [ids : (Listof Symbol)]) : Void
  (cond [(not (empty? ids))
         (cond [(first-id-unique? (first ids) (rest ids))
                (check-func-names-unique (rest ids))]
               [else (error 'check-func-names-unique "Redeclaration of function with identifier ~e is not allowed" (first ids))])]))

;; checks for a valid main function
(define (check-valid-main [functions : Function-Type-Env]) : Void
  (if (empty? functions)
      (error 'check-valid-main "No main function defined; every valid program must have a function named 'main that takes no arguments and returns an 'int")
      (cond [(equal? (Function-Binding-name (first functions)) 'main)
             (cond [(not (and (equal? (Function-Binding-args (first functions)) '())
                              (equal? (Function-Binding-return-type (first functions)) 'int)))
                    (error 'check-valid-main "'main has an invalid signature, expected '() -> 'int, given ~e -> ~e"
                           (Function-Binding-args (first functions)) (Function-Binding-return-type (first functions)))])]
             [else (check-valid-main (rest functions))])))

;; type checks the program's functions
(define (type-check-functions [functions : (Listof Function)] [env : Environment]) : Environment
  (cond
    [(empty? functions) env]
    [else (let ([first-function-binding : Function-Binding (bind-function (first functions) (Environment-structs env))])
            (set-Environment-functions! env (cons first-function-binding (Environment-functions env)))
            (type-check-function (first functions) env)
            (cond [(not (equal? (Function-return-type (first functions)) 'void))
                   (cond [(not (all-paths-return? (Function-statements (first functions))))
                          (error 'type-check-functions "All paths through function ~e must return with type ~e"
                                 (Function-id (first functions)) (Function-return-type (first functions)))])])
            (type-check-functions (rest functions) env)
            env)]))

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
(define (bind-function [function : Function] [structs : Struct-Type-Env]) : Function-Binding
  (match function
    [(Function name parameters return-type declarations statements)
     (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'bind-function "Function ~e's parameters failed type check : ~e"
                                                             name (exn-message exn)))])
       (Function-Binding name (map Type-Binding-type (bind-declarations parameters (map Struct-Binding-name structs))) return-type '()))]))

;; type checks a single function 
(define (type-check-function [function : Function] [env : Environment]) : Void
  (match function
    [(Function name parameters return-type declarations statements)
     (let ([local-env : Variable-Type-Env (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'type-check-function "Function ~e's local variables failed type check : ~e"
                                                                                                  name (exn-message exn)))])
                                            (bind-declarations (append declarations parameters) (map Struct-Binding-name (Environment-structs env))))])
       (let ([cur-func-binding : Function-Binding (first (Environment-functions env))])
         (set-Function-Binding-locals! cur-func-binding (append base-local-env local-env))
         (type-check-statements statements env return-type)))]))

;; type checks a list of statements
(define (type-check-statements [statements : (Listof Statement)] [environment : Environment] [expected-return-type : Return-Type]) : Void
  (cond [(not (empty? statements))
         (type-check-statement (first statements) environment expected-return-type)
         (type-check-statements (rest statements) environment expected-return-type)]))

;; type checks a single statement
(define (type-check-statement [statement : Statement] [environment : Environment] [expected-return-type : Return-Type]) : Void
  (match statement
    [(Block-Expr statements)
     (type-check-statements statements environment expected-return-type)]
    [(Assignment target source)
     (let ([source-type : Type (type-check-expression source environment)]
           [target-type : Type (type-check-expression target environment)])
       (cond [(not (or (equal? source-type target-type)
                       (and (equal? source-type 'null) (structure-type? target-type))))
              (error 'type-check-statement "Assignment ~e = ~e failed type checking: cannot assign type ~e to a variable of type ~e"
                     target source source-type target-type)]))]
    [(Print expression endl) 
     (let ([expression-type : Type (type-check-expression expression environment)])
       (cond [(not (equal? expression-type 'int))
              (error 'type-check-statement "Print requires an integer argument; given type ~e" expression-type)]))]
    [(If guard then-block)
     (let ([guard-type : Type (type-check-expression guard environment)])
       (cond [(not (equal? guard-type 'bool))
              (error 'type-check-statement "If statements require boolean guards; given expression ~e of type ~e" guard guard-type)])
       (type-check-statements (Block-Expr-statements then-block) environment expected-return-type))]
    [(If-Else guard then-block else-block)
     (let ([guard-type : Type (type-check-expression guard environment)])
       (cond [(not (equal? guard-type 'bool))
              (error 'type-check-statement "If statements require boolean guards; given expression ~e of type ~e" guard guard-type)])
       (type-check-statements (Block-Expr-statements then-block) environment expected-return-type)
       (type-check-statements (Block-Expr-statements else-block) environment expected-return-type))]
    [(Loop guard body)
     (let ([guard-type : Type (type-check-expression guard environment)])
       (cond [(not (equal? guard-type 'bool))
              (error 'type-check-statement "While loops require boolean guards; given expression ~e of type ~e" guard guard-type)])
       (type-check-statements (Block-Expr-statements body) environment expected-return-type))]
    [(Delete expression)
     (let ([expression-type : Type (type-check-expression expression environment)])
       (cond [(not (structure-type? expression-type))
              (error 'type-check-statement "Delete requires a structure type; given expression ~e of type ~e" expression expression-type)]))]
    [(Return expression) 
     (let ([actual-return-type : Type (type-check-expression expression environment)])
       (cond [(not (equal? actual-return-type expected-return-type))
              (error 'type-check-statement "Return expression ~e failed type check; given type ~e, expected type ~e"
                     expression actual-return-type expected-return-type)]))]
    [(Return-Void) (cond [(not (equal? expected-return-type 'void))
                          (error 'type-check-statement "Return statement failed type check; given type 'void, expected type ~e" expected-return-type)])]
    [(Invocation id arguments)
     (let ([function-binding : Function-Binding (lookup-function id (Environment-functions environment))]
           [actual-arg-types : (Listof Expression-Type) (map (lambda ([arg : Expression]) (type-check-expression arg environment)) arguments)])
       (cond [(not (valid-arguments? (Function-Binding-args function-binding) actual-arg-types))
              (error 'type-check-statement "Invocation of function ~e failed type check; given expressions ~e of types ~e, expected types ~e"
                     id arguments actual-arg-types (Function-Binding-args function-binding))]))]))

;; type checks an expression
(define (type-check-expression [expression : Expression] [env : Environment]) : Expression-Type
  (match expression
    [(Binary operator left right)
     (let ([left-type : Type (type-check-expression left env)]
           [right-type : Type (type-check-expression right env)])
       (cond
         [(or (equal? operator '==) (equal? operator '!=))
          (if (and (equal? left-type right-type) (not (equal? left-type 'bool)))
              'bool
              (error 'type-check-expression "Equality operator ~e requires operands of matching types (either 'int or structure); given expressions ~e of type ~e and ~e of type ~e"
                     operator left left-type right right-type))]
         [else (let ([function-binding : Function-Binding (lookup-function operator base-function-env)])
                 (cond [(equal? (list left-type right-type) (Function-Binding-args function-binding))
                        (Function-Binding-return-type function-binding)]
                       [else (error 'type-check-expression "Binary operator ~e requires operands of types ~e; given expressions ~e of type ~e and ~e of type ~e"
                                    operator (Function-Binding-args function-binding) left left-type right right-type)]))]))]
    [(Unary operator operand)
     (let ([operand-type : Type (type-check-expression operand env)])
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
     (let ([left-type : Type (type-check-expression left env)])
       (if (structure-type? left-type)
           (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'type-check-expression "Structure ~e field access failed: ~e"
                                                                   left-type (exn-message exn)))])
             (let ([fields : Variable-Type-Env (lookup-struct left-type (Environment-structs env))])
               (lookup-id symbol fields)))
           (error 'type-check-expression "Field access requires a structure type; given expression ~e of type ~e" left left-type)))]
    [(Invocation id arguments)
     (let ([function-binding : Function-Binding (lookup-function id (Environment-functions env))]
           [actual-arg-types : (Listof Expression-Type) (map (lambda ([arg : Expression]) (type-check-expression arg env)) arguments)])
       (if (valid-arguments? (Function-Binding-args function-binding) actual-arg-types)
           (Function-Binding-return-type function-binding)
           (error 'type-check-expression "Invocation of function ~e failed type check; given expressions ~e of types ~e, expected types ~e"
                     id arguments actual-arg-types (Function-Binding-args function-binding))))]
    [(Allocation id)
     (lookup-struct id (Environment-structs env))
     id]
    [value
     (cond
       [(equal? 'null value) 'null]
       [(symbol? value)
        (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'type-check-expression "Variable ~e is unbound: ~e" value (exn-message exn)))])
          (lookup-id value (append (Function-Binding-locals (first (Environment-functions env)))
                                   (Environment-globals env))))]
       [(integer? value) 'int]
       [(boolean? value) 'bool]
       [else (error 'type-check-expression "Invalid expression type ~e" value)])]))

;; binds a list of structs to their corresponding type environments
(define (bind-structs [new-structs : (Listof Struct)] [bound-struct-names : (Listof Symbol)]) : Struct-Type-Env
  (cond
    [(empty? new-structs) '()]
    [else (let ([first-struct-binding : Struct-Binding (bind-struct (first new-structs) bound-struct-names)])
            (let ([first-struct-fields : Variable-Type-Env (Struct-Binding-fields first-struct-binding)]
                  [bound-struct-names : (Listof Symbol) (cons (Struct-Binding-name first-struct-binding) bound-struct-names)])
              (let ([rest-struct-bindings : Struct-Type-Env (bind-structs (rest new-structs) bound-struct-names)])
                (if (first-id-unique? (Struct-Binding-name first-struct-binding) (map Struct-Binding-name rest-struct-bindings))
                    (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'bind-structs "Structure ~e's fields failed type check: ~e"
                                                                            (Struct-Binding-name first-struct-binding) (exn-message exn)))])
                      (verify-types (map Type-Binding-type first-struct-fields) bound-struct-names)
                      (cons first-struct-binding rest-struct-bindings))
                    (error 'bind-structs "Redeclaration of structure with identifier ~e is not allowed" (Struct-Binding-name first-struct-binding))))))]))
          
;; binds a single struct to a list of type bindings representing its fields
(define (bind-struct [struct : Struct] [bound-struct-names : (Listof Symbol)]) : Struct-Binding
  (match struct
    [(Struct id fields)
     (Struct-Binding id (bind-declarations fields (cons id bound-struct-names)))]))

;; binds a list of declarations to their corresponding types
(define (bind-declarations [declarations : (Listof Declaration)] [bound-struct-names : (Listof Symbol)]) : Variable-Type-Env
  (cond
    [(empty? declarations) '()]
    [else (define first-binding (bind-declaration (first declarations)))
          (define rest-bindings (bind-declarations (rest declarations) bound-struct-names))
          (if (first-id-unique? (Type-Binding-name first-binding) (map Type-Binding-name rest-bindings))
              (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'bind-declarations (exn-message exn)))])
                (verify-type (Type-Binding-type first-binding) bound-struct-names)
                (cons first-binding rest-bindings))
              (error 'bind-declarations "Redeclaration of variable with identifier ~e is not allowed" (Type-Binding-name first-binding)))]))

;; binds an id to a type
(define (bind-declaration [declaration : Declaration]) : Type-Binding
  (match declaration
    [(Declaration type id)
     (Type-Binding id type)]))

;; is the first ID unique among a list of IDs?
(define (first-id-unique? [first-id : Symbol] [rest-ids : (Listof Symbol)]) : Boolean
  (cond
    [(empty? rest-ids) #t]
    [else (if (equal? first-id (first rest-ids))
              #f
              (first-id-unique? first-id (rest rest-ids)))]))

;; verifies that the given types are all defined in the environment
(define (verify-types [new-types : (Listof Type)] [bound-struct-names : (Listof Symbol)]) : Void
  (cond [(not (empty? new-types))
         (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'verify-types (exn-message exn)))])
           (verify-type (first new-types) bound-struct-names)
           (verify-types (rest new-types) bound-struct-names))]))

;; verifies that a single type is defined in the environment
(define (verify-type [new-type : Type] [bound-struct-names : (Listof Symbol)]) : Void
  (cond [(structure-type? new-type)
         (with-handlers ([exn:fail? (lambda ([exn : exn]) (error 'verify-type (exn-message exn)))])
             (verify-struct-id new-type bound-struct-names))]))

;; verifies that a single struct id is defined in the environment
(define (verify-struct-id [id : Symbol] [bound-struct-names : (Listof Symbol)]) : Void
  (cond 
    [(empty? bound-struct-names) (error 'verify-struct-id "Identifier ~e is not defined or is out of scope" id)]
    [else (cond [(not (equal? id (first bound-struct-names)))
                 (verify-struct-id id (rest bound-struct-names))])]))

;; gets the function binding for a function with a given id
(define (lookup-function [id : Symbol] [functions : Function-Type-Env]) : Function-Binding
  (cond
    [(empty? functions) (error 'lookup-function "Type check failed: function ~e has not been defined" id)]
    [else (if (equal? id (Function-Binding-name (first functions)))
               (first functions)
               (lookup-function id (rest functions)))]))

;; gets the field declarations for the struct with the given id
(define (lookup-struct [id : Symbol] [structs : Struct-Type-Env]) : Variable-Type-Env
  (cond
    [(empty? structs) (error 'lookup-struct "Type check failed: struct ~e has not been defined" id)]
    [else (if (equal? id (Struct-Binding-name (first structs)))
              (Struct-Binding-fields (first structs))
              (lookup-struct id (rest structs)))]))

;; gets the type of the given identifier
(define (lookup-id [id : Symbol] [vars : Variable-Type-Env]) : Type
  (cond
    [(empty? vars) (error 'lookup-id "Type check failed: id ~e has not been defined" id)]
    [else (if (equal? id (Type-Binding-name (first vars)))
              (Type-Binding-type (first vars))
              (lookup-id id (rest vars)))]))

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
(define mode : Compiler-Mode 'stack)
(define c-lib-funcs (C-Lib-Funcs #f #f #f #f #f))

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
     (define exit-block (Block -1 '() '() '() '() #f #f))
     (define blocks 
       (ann (make-hash
             (list (cons 0 entry-block)
                   (cons 'exit-block exit-block)))
            Block-Table))
     (add-statements-LLVM! entry-block blocks statements (get-cur-env env name))
     (cleanup-exit-block exit-block return-type)
     (CFG name parameters return-type blocks)]))

;; adds the LLVM for the given list of statements to the supplied block
;; updates the block hash table accordingly
(define (add-statements-LLVM! [cur-block : Block] [blocks : Block-Table] [statements : (Listof Statement)] [env : Environment]) : Void
  (cond
    [(not (empty? statements))
     (match (first statements)
       [(Block-Expr statement-list)
        (add-statements-LLVM! cur-block blocks (append statement-list (rest statements)) env)]
       [(Assignment target source)
        (if (equal? source 'read)
            (add-read-LLVM! target cur-block env)
            (add-assignment-LLVM! target source cur-block env))
        (add-statements-LLVM! cur-block blocks (rest statements) env)]
       [(Print expression endl)
        (define exp-operand (add-expression-LLVM! expression cur-block env))
        (define target (Intermediate counter))
        (set! counter (+ 1 counter))
        (add-LLVM! cur-block (Print-Instr exp-operand endl target))
        (if endl
            (set-C-Lib-Funcs-printf-newline! c-lib-funcs #t)
            (set-C-Lib-Funcs-printf-space! c-lib-funcs #t))
        (add-statements-LLVM! cur-block blocks (rest statements) env)]
       [(If guard then)
        (define guard-operand (add-expression-LLVM! guard cur-block env))
        (define then-block (create-block (list cur-block) #t blocks))
        (add-statements-LLVM! then-block blocks (list then) env)
        (define after-block (create-block (list cur-block then-block) #t blocks))
        (add-statements-LLVM! after-block blocks (rest statements) env)
        (add-LLVM! cur-block (Cond-Branch-Instr guard-operand then-block after-block))
        (set-Block-successors! cur-block (list then-block after-block))
        (set-successors! then-block after-block)]
       [(If-Else guard then else)
        (define guard-operand (add-expression-LLVM! guard cur-block env))
        (define then-block (create-block (list cur-block) #t blocks))
        (add-statements-LLVM! then-block blocks (list then) env)
        (define else-block (create-block (list cur-block) #t blocks))
        (add-statements-LLVM! else-block blocks (list else) env)
        (define after-block (hash-ref blocks 'exit-block))
        (cond [(empty? (rest statements))
               (set-Block-reachable! after-block #t)
               (set-Block-predecessors! after-block (list then-block else-block))
               (set-Block-sealed! after-block #t)]
              [else (set! after-block (create-block (list then-block else-block) #t blocks))
                    (add-statements-LLVM! after-block blocks (rest statements) env)])
        (add-LLVM! cur-block (Cond-Branch-Instr guard-operand then-block else-block))
        (set-Block-successors! cur-block (list then-block else-block))
        (set-successors! then-block after-block)
        (set-successors! else-block after-block)]
       [(Loop guard body)
        (define guard-block cur-block)
        (cond [(not (empty? (Block-LLVM cur-block)))
               (set! guard-block (create-block (list cur-block) #f blocks))
               (add-LLVM! cur-block (Branch-Instr guard-block))]
              [else (set-Block-sealed! cur-block #f)])
        (define guard-operand (add-expression-LLVM! guard guard-block env))
        (define body-block (create-block (list guard-block) #t blocks))
        (add-statements-LLVM! body-block blocks (list body) env)
        (add-LLVM! body-block (Branch-Instr guard-block))
        (set-Block-predecessors! guard-block (append (Block-predecessors guard-block) (list body-block)))
        (set-Block-sealed! guard-block #t)
        (define after-block (hash-ref blocks 'exit-block))
        (cond [(empty? (rest statements))
               (set-Block-reachable! after-block #t)
               (set-Block-predecessors! after-block (list guard-block))
               (set-Block-sealed! after-block #t)]
              [else (set! after-block (create-block (list guard-block) #t blocks))
                    (add-statements-LLVM! after-block blocks (rest statements) env)])
        (add-LLVM! guard-block (Cond-Branch-Instr guard-operand body-block after-block))
        (set-Block-successors! cur-block (list guard-block))
        (set-Block-successors! guard-block (list body-block after-block))
        (set-Block-successors! body-block (list guard-block))]
       [(Delete expression)
        (define exp-operand (add-expression-LLVM! expression cur-block env))
        (add-LLVMs! cur-block (list (Size-Instr 'bitcast exp-operand (string-append "%struct." (symbol->string (type-check-expression expression env)) "*") "i8*" (Intermediate counter))
                                   (Free-Instr (Intermediate counter))))
        (set! counter (+ 1 counter))
        (add-statements-LLVM! cur-block blocks (rest statements) env)
        (set-C-Lib-Funcs-free! c-lib-funcs #t)]
       [(Return expression)
        (define result (add-expression-LLVM! expression cur-block env))
        (define return-type (Function-Binding-return-type (first (Environment-functions env))))
        (define exit-block (hash-ref blocks 'exit-block))
        (if (equal? cur-block 0)
            (add-LLVM! cur-block (Return-Expr-Instr return-type result))
            (block
             (add-LLVMs! cur-block (list (Store-Instr return-type result (Local '.return) (if (structure-type? return-type) 1 0))
                                         (Branch-Instr exit-block)))
             (set-Block-successors! cur-block (list exit-block))
             (set-Block-reachable! exit-block #t)))]
       [(Return-Void)
        (define exit-block (hash-ref blocks 'exit-block))
        (if (equal? cur-block 0)
            (add-LLVM! cur-block (Return-Void-Instr))
            (block
             (add-LLVM! cur-block (Branch-Instr exit-block))
             (set-Block-successors! cur-block (list exit-block))
             (set-Block-reachable! exit-block #t)))]
       [(Invocation id arguments)
        (define args (map (lambda ([arg : Expression]) (add-expression-LLVM! arg cur-block env)) arguments))
        (define function (lookup-function id (Environment-functions env)))
        (add-LLVM! cur-block (Call-Instr (Global id) (Function-Binding-return-type function) (Function-Binding-args function) args (Intermediate counter)))
        (set! counter (+ counter 1))
        (add-statements-LLVM! cur-block blocks (rest statements) env)])]))

;; adds the LLVM for the given expression to the supplied block
;; returns the operand that holds the result of this expression
(define (add-expression-LLVM! [expression : Expression] [block : Block] [env : Environment]) : Operand
  (match expression
    [(Binary operator left right)
     (define left-result (add-expression-LLVM! left block env))
     (define right-result (add-expression-LLVM! right block env))
     (define target (Intermediate counter))
     (cond [(member operator (list '+ '- '* '/))
            (add-LLVM! block (Arith-Instr operator left-result right-result target))]
           [(member operator (list '|| '&& '^^))
            (add-LLVM! block (Bool-Instr operator left-result right-result target))]
           [(member operator (list '== '!= '< '> '<= '>=))
            (define type (if (or (equal? operator '==) (equal? operator '!=))
                             (type-check-expression left env)
                             'int))
            (add-LLVM! block (Comp-Instr operator type left-result right-result target))])
     (set! counter (+ counter 1))
     target]
    [(Unary operator expression)
     (define result (add-expression-LLVM! expression block env))
     (define target (Intermediate counter))
     (cond [(equal? operator '-) (add-LLVM! block (Arith-Instr '- 0 result target))]
           [(equal? operator '!) (add-LLVM! block (Bool-Instr '^^ result #t target))])
     (set! counter (+ counter 1))
     target]
    [(Selector left id)
     (define pointer (if (symbol? left)
                         (LLVM-Declaration-operand (get-declaration left
                                                                    (Function-Binding-locals (first (Environment-functions env)))
                                                                    (Environment-globals env)))
                         (add-expression-LLVM! left block env)))
     (define type (type-check-expression left env))
     (define target (Intermediate counter))
     (define next-target (Intermediate (+ 1 counter)))
     (define field (get-field type id (Environment-structs env)))
     (add-LLVMs! block (list (Get-Elm-Ptr-Instr type pointer (Field-offset field) target) 
                            (Load-Instr (Field-type field) target next-target (if (structure-type? (Field-type field)) 1 0))))
     (set! counter (+ counter 2))
     next-target]
    [(Invocation id arguments)
     (define args (map (lambda ([arg : Expression]) (add-expression-LLVM! arg block env)) arguments))
     (define target (Intermediate counter))
     (define function (lookup-function id (Environment-functions env)))
     (add-LLVM! block (Call-Instr (Global id) (Function-Binding-return-type function) (Function-Binding-args function) args target))
     (set! counter (+ counter 1))
     target]
    [(Allocation id)
     (define first-target (Intermediate counter))
     (define second-target (Intermediate (+ counter 1)))
     (set-C-Lib-Funcs-malloc! c-lib-funcs #t)
     (add-LLVM! block (Malloc-Instr first-target))
     (add-LLVM! block (Size-Instr 'bitcast first-target "i8*" (string-append "%struct." (symbol->string id) "*") second-target))
     (set! counter (+ counter 2))
     second-target]
    [value
     (cond
       [(equal? 'null value) 'null]
       [(symbol? value)
        (define target (Intermediate counter))
        (define declaration (get-declaration value (Function-Binding-locals (first (Environment-functions env))) (Environment-globals env)))
        (define type (LLVM-Declaration-type declaration))
        (define source (LLVM-Declaration-operand declaration))
        (add-LLVM! block (Load-Instr type source target (if (structure-type? type) 1 0)))
        (set! counter (+ counter 1))
        target]
       [(integer? value) value]
       [(boolean? value) value]
       [else (error 'add-expression-LLVM! "Invalid expression type ~e" value)])]))

;; adds the LLVM for a read instruction to the current block
(define (add-read-LLVM! [target : Expression] [block : Block] [env : Environment]) : Void
  (set-C-Lib-Funcs-scanf! c-lib-funcs #t)
  (cond
    [(equal? mode 'stack)
     (define target-operand
       (if (symbol? target)
           (if (global-variable? target (Environment-globals env))
               (Global target)
               (Local target))
           (add-expression-LLVM! target block env)))
     (Scan-Instr target-operand (Intermediate counter))
     (set! counter (+ 1 counter))]
    [(equal? mode 'registers)
     (cond
       [(symbol? target)
        (cond
          [(global-variable? target (Environment-globals env))
           (Scan-Instr (Global target) (Intermediate counter))
           (set! counter (+ 1 counter))]
          [else (Alloca-Instr 'int (Intermediate counter))
                (Scan-Instr (Intermediate counter) (Intermediate (+ 1 counter)))
                (Load-Instr 'int (Intermediate counter) (Register (+ 2 counter)) 0)
                (write-variable! target (Register (+ 2 counter)) block (Block-definitions block))
                (set! counter (+ 3 counter))])]
       [else (Scan-Instr (add-expression-LLVM! target block env) (Intermediate counter))
             (set! counter (+ 1 counter))])]
    [else (error 'add-read-LLVM! "Unsupported compiler mode: ~e" mode)]))

;; adds the LLVM for an assignment instruction to the current block
(define (add-assignment-LLVM! [target : Expression] [source : Expression] [cur-block : Block] [env : Environment]) : Void
  (cond
    [(equal? mode 'stack)
     (define target-operand
       (if (symbol? target)
           (if (global-variable? target (Environment-globals env))
               (Global target)
               (Local target))
           (add-expression-LLVM! target cur-block env)))
     (define source-operand (add-expression-LLVM! source cur-block env))
     (add-LLVM! cur-block (Store-Instr (type-check-expression target env) source-operand target-operand 0))]
    [(equal? mode 'register)
     (define source-operand (add-expression-LLVM! source cur-block env))
     (if (symbol? target)
         (if (global-variable? target (Environment-globals env))
             (add-LLVM! cur-block (Store-Instr (type-check-expression target env) source-operand (Global target) 0))
             (write-variable! target source-operand cur-block (Block-definitions cur-block)))
         (block
          (define target-operand (add-expression-LLVM! target cur-block env))
          (add-LLVM! cur-block (Store-Instr (type-check-expression target env) source-operand target-operand 0))))]
    [else (error 'add-assignment-LLVM! "Unsupported compiler mode: ~e" mode)]))

;; add the variable to the list of definitions - if it is already defined, redefine it
(define (write-variable! [var : Symbol] [value : Definition-Value] [block : Block] [definitions : (Listof LLVM-Definition)]) : Void
  (if (empty? definitions)
      (set-Block-definitions! block (cons (LLVM-Definition var value) (Block-definitions block)))
      (if (equal? var (LLVM-Definition-var (first definitions)))
          (set-LLVM-Definition-value! (first definitions) value)
          (write-variable! var value block (rest definitions)))))
  
;; is the variable in the global environment? (if not, it must be local)
(define (global-variable? [var : Symbol] [globals : Variable-Type-Env]) : Boolean
  (if (empty? globals)
      #f
      (or (equal? var (Type-Binding-name (first globals))) (global-variable? var (rest globals)))))
            
;; creates the struct instructions (struct definitions)
(define (create-struct-instrs [structs : Struct-Type-Env]) : (Listof Struct-Instr)
  (if (empty? structs)
      '()
      (cons (Struct-Instr (Struct-Binding-name (first structs))
                          (map Type-Binding-type (Struct-Binding-fields (first structs))))
            (create-struct-instrs (rest structs)))))

;; creates the global instructions (declarations of global variables)
(define (create-global-instrs [globals : Variable-Type-Env]) : (Listof Global-Instr)
  (if (empty? globals)
      '()
      (cons (Global-Instr (Type-Binding-name (first globals))
                          (Type-Binding-type (first globals)))
            (create-global-instrs (rest globals)))))

;; adds a list of LLVM instructions to a block
(define (add-LLVMs! [block : Block] [LLVMs : (Listof LLVM-Instr)]) : Void
  (set-Block-LLVM! block (append (Block-LLVM block) LLVMs)))

;; adds an LLVM instruction to a block
(define (add-LLVM! [block : Block] [LLVM : LLVM-Instr]) : Void
  (set-Block-LLVM! block (append (Block-LLVM block) (list LLVM))))

;; gets the environment as defined while within the given function
;; (implements the correct scope upon generating LLVM to ensure there is no access to functions created after this one in the file)
(define (get-cur-env [env : Environment] [func-name : Symbol]) : Environment
  (if (equal? func-name (Function-Binding-name (first (Environment-functions env))))
      env
      (get-cur-env (Environment (Environment-structs env) (Environment-globals env) (rest (Environment-functions env))) func-name)))

;; gets the field information for a field in a struct
(define (get-field [type : Type] [id : Symbol] [structs : Struct-Type-Env]) : Field
  (if (equal? type (Struct-Binding-name (first structs)))
      (Field (get-field-type id (Struct-Binding-fields (first structs)))
             (get-field-offset id (Struct-Binding-fields (first structs))))
      (get-field type id (rest structs))))

;; gets the field type for a field in a struct
(define (get-field-type [id : Symbol] [fields : Variable-Type-Env]) : Type
  (if (equal? id (Type-Binding-name (first fields)))
      (Type-Binding-type (first fields))
      (get-field-type id (rest fields))))

;; gets the field offset for a field in a struct
(define (get-field-offset [id : Symbol] [fields : Variable-Type-Env]) : Integer
  (if (equal? id (Type-Binding-name (first fields)))
      0
      (+ 1 (get-field-offset id (rest fields)))))

;; gets the declaration for a symbol from the environment
;; checks locals first (since local declarations can hide global ones) and then globals
(define (get-declaration [id : Symbol] [locals : Variable-Type-Env] [globals : Variable-Type-Env]) : LLVM-Declaration
  (cond
    [(and (empty? locals) (empty? globals)) (error 'get-declaration "undefined behavior: ~e is not defined" id)]
    [(empty? locals) (if (equal? id (Type-Binding-name (first globals)))
                         (LLVM-Declaration (Type-Binding-type (first globals)) (Global id))
                         (get-declaration id '() (rest globals)))]
    [else (if (equal? id (Type-Binding-name (first locals)))
              (LLVM-Declaration (Type-Binding-type (first locals)) (Local id))
              (get-declaration id (rest locals) globals))]))

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
    [(CFG func-name parameters return-type blocks)
     (define dot-code (string-append "digraph \"" (symbol->string func-name) "\" {\n"))
     (define entry-block (hash-ref blocks 0))
     (set! dot-code (string-append dot-code (apply string-append (map generate-label (hash-values blocks)))))
     (set! dot-code (string-append dot-code (apply string-append (map generate-edge (hash-values blocks)))))
     (string-append dot-code "}\n\n")]))

;; generate dot code for a label
(define (generate-label [block : Block]) : String
  (match block
    [(Block label LLVM definitions _ _ _ _)
     (string-append (number->string label) "[ label = \"" (number->string label) "\n" (LLVMs->string LLVM)
                    "\", xlabel = \"" (definitions->string definitions) "\"]")]))

;; generate dot code for an edge
(define (generate-edge [block : Block]) : String
  (match block
    [(Block label _ _ _ successors _ _)
     (if (empty? successors)
         ""
         (apply string-append (map (lambda ([successor-label : Integer])
                                     (string-append (number->string label) "->" (number->string successor-label) ";\n"))
                                   (map Block-label successors))))]))

;; converts an LLVM-Program object into its string representation!
(define (LLVM-Program->string [LLVM : LLVM-Program]) : String
  (match LLVM
    [(LLVM-Program structs globals CFGs c-lib-funcs)
     (string-append (structs->string structs) "\n"
                    (c-lib-func-globals->string c-lib-funcs)
                    (globals->string globals) "\n"
                    (CFGs->string CFGs)
                    (c-lib-funcs->string c-lib-funcs))]))

;; converts a list of struct instructions to their string representations
(define (structs->string [structs : (Listof Struct-Instr)]) : String
  (if (empty? structs)
      ""
      (string-append "%struct."
                     (symbol->string (Struct-Instr-name (first structs)))
                     " = type { "
                     (type->string (first (Struct-Instr-fields (first structs))) (Levels-Indirection 0 1))
                     (apply string-append (map (lambda ([type : Type]) (string-append ", " (type->string type (Levels-Indirection 0 1))))
                                               (rest (Struct-Instr-fields (first structs)))))
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
(define (globals->string [globals : (Listof Global-Instr)]) : String
  (if (empty? globals)
      ""
      (string-append "@"
                     (symbol->string (Global-Instr-name (first globals)))
                     " = common global "
                     (type->string (Global-Instr-type (first globals)) (Levels-Indirection 0 0))
                     " "
                     (default-global-value (Global-Instr-type (first globals)))
                     ", align "
                     (align-value (Global-Instr-type (first globals)))
                     "\n"
                     (globals->string (rest globals)))))

;; converts a list of Control Flow Graphs (CFGs) to their string representations
(define (CFGs->string [CFGs : (Listof CFG)]) : String
  (if (empty? CFGs)
      ""
      (string-append (CFG->string (first CFGs))
                     (CFGs->string (rest CFGs)))))

;; converts a Control Flow Graph (CFG) to its string representation
(define (CFG->string [cfg : CFG]) : String
  (match cfg
    [(CFG func-name parameters return-type blocks)
     (string-append "define " (type->string return-type (Levels-Indirection 0 1))
                    " @" (symbol->string func-name) "(" (get-parameters-string parameters 0)
                    ") {\n" (block-table->string blocks))]))

;; converts the hashtable of blocks to their string representations
(define (block-table->string [blocks : Block-Table]) : String
  (apply string-append (map block->string (sort (hash-values blocks) block<))))

;; converts a block to its string representation
(define (block->string [block : Block]) : String
  (match block
    [(Block label LLVM _ _ _ reachable _)
     (if reachable (string-append (if (equal? label 0) "" (string-append (number->string label) ":\n"))
                                  (LLVMs->string LLVM) "\n")
         "")]))

;; converts a list of LLVM instructions to their string representations
(define (LLVMs->string [LLVMs : (Listof LLVM-Instr)]) : String
  (if (empty? LLVMs)
      ""
      (string-append (LLVM->string (first LLVMs)) (LLVMs->string (rest LLVMs)))))

;; converts an LLVM instruction to its string representation
(define (LLVM->string [LLVM : LLVM-Instr]) : String
  (match LLVM
    [(Alloca-Instr type target)
     (string-append "  " (operand->string target) " = alloca " (type->string type (Levels-Indirection 0 1))
                    ", align " (align-value type) "\n")]
    [(Arith-Instr operator left right target)
     (string-append "  " (operand->string target) " = " (operator->string operator)
                    " i32 " (operand->string left) ", " (operand->string right) "\n")]
    [(Bool-Instr operator left right target)
     (string-append "  " (operand->string target) " = " (operator->string operator)
                    " i32 " (operand->string left) ", " (operand->string right) "\n")]
    [(Comp-Instr operator type left right target)
     (string-append "  " (operand->string target) " = icmp " (operator->string operator)
                    " " (type->string type (Levels-Indirection 0 1)) " " (operand->string left) ", "
                    (operand->string right) "\n")]
    [(Size-Instr operator operand from to target)
     (string-append "  " (operand->string target) " = " (operator->string operator) " "
                    from " " (operand->string operand) " to " to "\n")]
    [(Get-Elm-Ptr-Instr type pointer field-offset target)
     (string-append "  " (operand->string target) " = getelementptr " (type->string type (Levels-Indirection 0 0))
                    ", " (type->string type (Levels-Indirection 0 1)) " " (operand->string pointer) ", i1 0, i32 "
                    (number->string field-offset) "\n")]
    [(Call-Instr func-name return-type arg-types args target)
     (string-append "  " (operand->string target) " = call " (type->string return-type (Levels-Indirection 0 1))
                    " " (operand->string func-name) "(" (get-arguments-string arg-types args)
                    ")\n")]
    [(Malloc-Instr target)
     (string-append "  " (operand->string target) " = call i8* @malloc (i64 8)\n")]
    [(Free-Instr target)
     (string-append "  call void @free(i8* " (operand->string target) ")\n")]
    [(Load-Instr type source target indir)
     (string-append "  " (operand->string target) " = load "
                    (type->string type (Levels-Indirection indir indir)) ", "
                    (type->string type (Levels-Indirection (+ 1 indir) (+ 1 indir))) " "
                    (operand->string source) ", align "
                    (align-value type) "\n")]
    [(Store-Instr type source target indir)
     (string-append "  store " (type->string type (Levels-Indirection indir indir)) " " (operand->string source) ", "
                    (type->string type (Levels-Indirection (+ 1 indir) (+ 1 indir))) " " (operand->string target) ", align "
                    (align-value type) "\n")]
    [(Print-Instr operand endl target)
     (if endl
         (string-append "  " (operand->string target) " = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.print.newline, i64 0, i64 0), i32 " (operand->string operand) ")\n")
         (string-append "  " (operand->string target) " = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.print.space, i64 0, i64 0), i32 " (operand->string operand) ")\n"))]
    [(Scan-Instr var result)
     (string-append "  " (operand->string result) " = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.read, i64 0, i64 0), i32* " (operand->string var) ")\n")]
    [(Branch-Instr block)
     (string-append "  br label " (operand->string (Block-label block)) "\n")]
    [(Cond-Branch-Instr cond true false)
     (string-append "  br i1 " (operand->string cond) ", label " (operand->string (Block-label true))
                    ", label " (operand->string (Block-label false)) "\n")]
    [(Return-Expr-Instr type expr)
     (string-append "  ret " (type->string type (Levels-Indirection 0 1)) " " (operand->string expr) "\n}\n")]
    [(Return-Void-Instr) "ret void\n}\n"]))

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
    [(Block label _ _ _ _ _ _) (string-append "%" (number->string label))]
    [(Register num) (string-append "%r" (number->string num))]
    [Literal (literal->string operand)]))

;; converts operators to their string representation
(define (operator->string [operator : Operator]) : String
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

(define (definitions->string [definitions : (Listof LLVM-Definition)]) : String
  (if (empty? definitions)
      ""
      (string-append (symbol->string (LLVM-Definition-var (first definitions))) ": "
                     (definition-value->string (LLVM-Definition-value (first definitions))) "\n"
                     (definitions->string (rest definitions)))))

(define (definition-value->string [value : Definition-Value]) : String
  (cond
    [(Register? value) (string-append "r" (number->string (Register-num value)))]
    [else (literal->string value)]))

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
  (define entry-block (Block -1 '() '() '() '() #t #t))
  (cond
    [(equal? mode 'stack)
     (cond [(not (equal? return-type 'void))
            (add-LLVM! entry-block (Alloca-Instr return-type (Local '.return)))])
     (allocate-declarations entry-block parameters declarations)
     (store-parameters entry-block parameters)]
    [(equal? mode 'registers)
     (map (lambda ([var : Symbol])
            (write-variable! var (Register counter) entry-block (Block-definitions entry-block))
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
              (map (lambda ([dec : Declaration]) (Alloca-Instr (Declaration-type dec) (Local (Declaration-id dec))))
                   (append non-struct-params declarations))))

;; store the values of all non-struct parameters into local variables on the stack
;; we don't allocate or store structure parameters (as explained above)
(define (store-parameters [entry-block : Block] [parameters : (Listof Declaration)]) : Void
  (define non-struct-params (filter (lambda ([param : Declaration]) (not (structure-type? (Declaration-type param)))) parameters))
  (add-LLVMs! entry-block (map (lambda ([param : Declaration])
                                (define target (Intermediate counter))
                                (set! counter (+ 1 counter))
                                (Store-Instr (Declaration-type param) target (Local (Declaration-id param)) 0))
                              non-struct-params)))

;; clean up the exit block
;; this entails adding return instructions and setting the label (we couldn't set it before since block labels must be sequential)
(define (cleanup-exit-block [exit-block : Block] [return-type : Return-Type]) : Void
  (if (equal? return-type 'void)
      (add-LLVM! exit-block (Return-Void-Instr))
      (add-LLVMs! exit-block (list (Load-Instr return-type (Local '.return) (Intermediate (+ 1 counter)) (if (structure-type? return-type) 1 0))
                                   (Return-Expr-Instr return-type (Intermediate (+ 1 counter))))))
  (set-Block-label! exit-block counter))

;; creates a block and puts it in the hash table
(define (create-block [predecessors : (Listof Block)] [sealed : Boolean] [blocks : Block-Table]) : Block
  (define block (Block counter '() '() predecessors '() #t sealed))
  (hash-set! blocks counter block)
  (set! counter (+ 1 counter))
  block)

(define (set-successors! [cur-block : Block] [default-next-block : Block]) : Void
  (define last-instr (last (Block-LLVM cur-block)))
  (cond [(or (Branch-Instr? last-instr) (Cond-Branch-Instr? last-instr))
         (set-Block-successors! cur-block (get-successors last-instr))]
        [else (add-LLVM! cur-block (Branch-Instr default-next-block))
              (set-Block-successors! cur-block (list default-next-block))]))

(define (get-successors [branch : Branch]) : (Listof Block)
  (match branch
    [(Branch-Instr block) (list block)]
    [(Cond-Branch-Instr cond true false) (list true false)]))

;; defines the inequality used to sort blocks in the block hash table (integer comparison on the block labels)
(define (block< [block1 : Block] [block2 : Block]) : Boolean
  (< (Block-label block1) (Block-label block2)))

;(test-compiler "../mile1/sample_json/simple.json")