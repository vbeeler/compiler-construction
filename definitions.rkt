#lang typed/racket

(provide (all-defined-out))

;; DEFINITIONS/TYPES

;; ABSTRACT SYNTAX TREE (AST)

(struct Program ([types : (Listof Struct)] [declarations : (Listof Declaration)] [functions : (Listof Function)]) #:transparent)
(struct Declaration ([type : Type] [id : Symbol]) #:transparent)
(define-type Type (U 'int 'bool Symbol))
(struct Struct ([id : Symbol] [fields : (Listof Declaration)]) #:transparent)
(struct Function ([id : Symbol] [parameters : (Listof Declaration)] [return-type : Return-Type] [declarations : (Listof Declaration)] [statements : (Listof Statement)]) #:transparent)
(define-type Return-Type (U Type 'void))
(define-type Statement (U Block-Expr Assignment Print Conditional Loop Delete Ret Invocation))
(struct Block-Expr ([statements : (Listof Statement)]) #:transparent)
(struct Assignment ([target : Lvalue] [source : Expression]) #:transparent)
(define-type Lvalue (U Selector Factor))
(define-type Expression (U Binary Unary Selector Factor))
(define-type Expression-Type (U Type 'null))
(struct Binary ([operator : BinOp] [left : Expression] [right : Expression]) #:transparent)
(define-type BinOp (U '|| '&& '== '!= '< '> '<= '>= '+ '- '* '/))
(struct Unary ([operator : UnOp] [operand : Expression]) #:transparent)
(define-type UnOp (U '! '-))
(struct Selector ([left : Expression] [id : Symbol]) #:transparent)
(define-type Factor (U Symbol Invocation Integer Boolean Allocation 'null))
(struct Invocation ([id : Symbol] [arguments : (Listof Expression)]) #:transparent)
(struct Allocation ([id : Symbol]) #:transparent)
(struct Print ([expression : Expression] [endl : Boolean]) #:transparent)
(define-type Conditional (U If If-Else))
(struct If ([guard : Expression] [then : Block-Expr]) #:transparent)
(struct If-Else ([guard : Expression] [then : Block-Expr] [else : Block-Expr]) #:transparent)
(struct Loop ([guard : Expression] [body : Block-Expr]) #:transparent)
(struct Delete ([expression : Expression]) #:transparent)
(define-type Ret (U Return Return-Void))
(struct Return ([expression : Expression]) #:transparent)
(struct Return-Void [] #:transparent)

;; TYPE ENVIRONMENT

(struct Field ([type : Type] [index : Integer]) #:transparent)
(struct Struct-Type ([fields : Field-Environment] [index : Integer]) #:transparent)
(struct Function-Type ([args : (Listof Type)] [return-type : Return-Type] [locals : Variable-Environment]) #:transparent #:mutable)

(define-type Variable-Environment (Mutable-HashTable Symbol Type))
(define-type Field-Environment (Mutable-HashTable Symbol Field))
(define-type Struct-Environment (Mutable-HashTable Symbol Struct-Type))
(define-type Function-Environment (Mutable-HashTable Symbol Function-Type))

(struct Environment ([structs : Struct-Environment]
                     [globals : Variable-Environment]
                     [functions : Function-Environment])
  #:transparent #:mutable)

;; base local variable environment
(define (base-local-env)
  (ann (make-hash
        (list (cons 'read 'int)))
       Variable-Environment))

;; base function environment - function bindings for binary functions
(define (base-function-env)
  (ann (make-hash
        (list (cons '+ (Function-Type (list 'int 'int) 'int (base-local-env)))
              (cons '- (Function-Type (list 'int 'int) 'int (base-local-env)))
              (cons '* (Function-Type (list 'int 'int) 'int (base-local-env)))
              (cons '/ (Function-Type (list 'int 'int) 'int (base-local-env)))
              (cons '< (Function-Type (list 'int 'int) 'bool (base-local-env)))
              (cons '> (Function-Type (list 'int 'int) 'bool (base-local-env)))
              (cons '<= (Function-Type (list 'int 'int) 'bool (base-local-env)))
              (cons '>= (Function-Type (list 'int 'int) 'bool (base-local-env)))
              (cons '|| (Function-Type (list 'bool 'bool) 'bool (base-local-env)))
              (cons '&& (Function-Type (list 'bool 'bool) 'bool (base-local-env)))))
       Function-Environment))

;; Control Flow Graph (CFG)

(struct CFG ([func-name : Symbol] [parameters : (Listof Declaration)] [return-type : Return-Type] [blocks : Block-Table]) #:transparent) 
(struct Block ([label : Integer]
               [LLVM : (Listof LLVM-Instr)]
               [definitions : Definition-Table]
               [predecessors : (Listof Block)]
               [successors : (Listof Block)]
               [reachable : Boolean]
               [sealed : Boolean])
  #:transparent #:mutable)
(define-type Block-Label (U Integer 'entry-block 'exit-block))
(define-type Block-Table (Mutable-HashTable Block-Label Block))
(define-type Definition-Table (Mutable-HashTable Symbol Operand))

;; LLVM INSTRUCTIONS

(define-type LLVM-Instr (U Alloca-Instr Arith-Instr Bool-Instr Comp-Instr Size-Instr Get-Elm-Ptr-Instr
                           Call-Instr Malloc-Instr Free-Instr Load-Instr Store-Instr Print-Instr Scan-Instr
                           Branch-Instr Cond-Branch-Instr Return-Expr-Instr Return-Void-Instr Phi-Instr Move-Instr))
(struct Alloca-Instr ([type : Type] [target : Target]) #:transparent)
(struct Arith-Instr ([operator : Arith-Operator] [left : Operand] [right : Operand] [target : Target]) #:transparent)
(struct Bool-Instr ([operator : Bool-Operator] [left : Operand] [right : Operand] [target : Target]) #:transparent)
(struct Comp-Instr ([operator : Comp-Operator] [type : Type] [left : Operand] [right : Operand] [target : Target]) #:transparent)
(struct Size-Instr ([operator : Size-Operator] [operand : Operand] [from-type : String] [to-type : String] [target : Target]) #:transparent)
(struct Get-Elm-Ptr-Instr ([type : Symbol] [pointer : Operand] [field-offset : Integer] [target : Target]) #:transparent)
(struct Call-Instr ([func-name : Operand] [return-type : Type] [arg-types : (Listof Type)] [args : (Listof Operand)] [target : Target]) #:transparent)
(struct Malloc-Instr ([target : Target]) #:transparent)
(struct Free-Instr ([target : Target]) #:transparent)
(struct Load-Instr ([type : Type] [source : Operand] [target : Target] [indirection : Integer]) #:transparent)
(struct Store-Instr ([type : Type] [source : Operand] [target : Operand] [indirection : Integer]) #:transparent)
(struct Print-Instr ([operand : Operand] [endl : Boolean] [target : Target]) #:transparent)
(struct Scan-Instr ([var : Operand] [result : Target]) #:transparent)
(define-type Branch (U Branch-Instr Cond-Branch-Instr))
(struct Branch-Instr ([block : Block]) #:transparent)
(struct Cond-Branch-Instr ([cond : Operand] [true : Block] [false : Block]) #:transparent)
(struct Return-Expr-Instr ([type : Type] [expr : Operand]) #:transparent)
(struct Return-Void-Instr [] #:transparent)
(struct Phi-Instr ([var : Symbol] [type : Type] [values : (Listof Phi-Value)] [target : Operand] [complete : Boolean] [removed : Boolean]) #:transparent #:mutable)
(struct Move-Instr ([target : Operand] [source : Operand]) #:transparent)

;; LLVM operators
(define-type Operator (U Arith-Operator Bool-Operator Comp-Operator Size-Operator))
(define-type Arith-Operator (U '+ '- '* '/))
(define-type Bool-Operator (U '|| '&& '^^))
(define-type Comp-Operator (U '== '!= '< '> '<= '>=))
(define-type Size-Operator (U 'trunc 'zext 'bitcast))

;; LLVM useful types
(struct Expr-LLVM ([LLVM : (Listof LLVM-Instr)] [result : Intermediate]) #:transparent)
(define-type Literal (U Integer Boolean 'null))
(struct Local ([id : Symbol]) #:transparent)
(struct Global ([id : Symbol]) #:transparent)
(struct Intermediate ([num : Integer]) #:transparent)
(struct Register ([num : Integer]) #:transparent)
(define-type Operand (U Literal Local Global Intermediate Register Block))
(define-type Target (U Local Global Intermediate Register))
(struct C-Lib-Funcs ([malloc : Boolean]
                     [free : Boolean]
                     [printf-newline : Boolean]
                     [printf-space : Boolean]
                     [scanf : Boolean])
  #:transparent #:mutable)
(struct Levels-Indirection ([primitive : Integer] [struct : Integer]) #:transparent)
(struct LLVM-Declaration ([type : Type] [operand : Operand]) #:transparent)
(struct Phi-Value ([operand : Operand] [block-label : Integer]) #:transparent)

;; LLVM Program
(struct Struct-Instr ([name : Symbol] [fields : (Listof Type)]) #:transparent)
(struct Global-Instr ([name : Symbol] [type : Type]) #:transparent)
(struct LLVM-Program ([structs : (Listof Struct-Instr)]
                      [globals : (Listof Global-Instr)]
                      [func-CFGs : (Listof CFG)]
                      [c-lib-funcs : C-Lib-Funcs])
  #:transparent)

;; Compiler Specifications/Information
(define-type Compiler-Mode (U 'stack 'registers))