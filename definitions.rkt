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

(struct CFG ([func-name : Symbol]
             [parameters : (Listof Declaration)]
             [return-type : Return-Type]
             [blocks : Block-Table]
             [stack : Stack-Table]
             [alloc-size : Integer]
             [arg-alloc-size : Integer]
             [saved-registers : (Listof Integer)]
             [intermediate-counter : Counter])
  #:transparent #:mutable) 
(struct Block ([label : Integer]
               [LLVM : (Listof LLVM-Instr)]
               [ARM : (Listof ARM-Instr)]
               [allocated-ARM : (Listof ARM-Instr)]
               [definitions : Definition-Table]
               [predecessors : (Listof Block)]
               [successors : (Listof Block)]
               [reachable : Boolean]
               [sealed : Boolean]
               [add-temp-moves : (Listof Move-ARM)]
               [live : Live-Analysis]
               [execution-frequency : Real])
  #:transparent #:mutable)
(define-type Block-Label (U Integer 'entry-block 'exit-block))
(define-type Block-Table (Mutable-HashTable Block-Label Block))
(define-type Definition-Table (Mutable-HashTable Symbol Operand))
(define-type Stack-Table (Mutable-HashTable Operand Integer))
(struct Live-Analysis ([gen : Bitset]
                       [kill : Bitset]
                       [out : Bitset])
  #:transparent #:mutable)
(struct Bitset ([bits : Integer]) #:transparent #:mutable)
(struct Counter ([num : Integer]) #:transparent #:mutable)

;; LLVM INSTRUCTIONS

(define-type LLVM-Instr (U Alloca-LLVM Arith-LLVM Bool-LLVM Comp-LLVM Size-LLVM Get-Elm-Ptr-LLVM
                           Call-LLVM Call-Void-LLVM Malloc-LLVM Free-LLVM Load-LLVM Store-LLVM Print-LLVM Scan-LLVM
                           Branch-LLVM Cond-Branch-LLVM Return-Expr-LLVM Return-Void-LLVM Phi-LLVM Move-LLVM))
(struct Alloca-LLVM ([type : Type] [local : Local]) #:transparent)
(struct Arith-LLVM ([operator : Arith-Operator] [left : Operand] [right : Operand] [target : Operand]) #:transparent)
(struct Bool-LLVM ([operator : Bool-Operator] [left : Operand] [right : Operand] [target : Operand]) #:transparent)
(struct Comp-LLVM ([operator : Comp-Operator] [type : Type] [left : Operand] [right : Operand] [target : Operand]) #:transparent)
(struct Size-LLVM ([operator : Size-Operator] [operand : Operand] [from-type : String] [to-type : String] [target : Operand]) #:transparent)
(struct Get-Elm-Ptr-LLVM ([type : Symbol] [pointer : Operand] [field-offset : Integer] [target : Operand]) #:transparent)
(struct Call-LLVM ([func-name : Global] [return-type : Type] [arg-types : (Listof Type)] [args : (Listof Operand)] [target : Operand]) #:transparent)
(struct Call-Void-LLVM ([func-name : Global] [arg-types : (Listof Type)] [args : (Listof Operand)]) #:transparent)
(struct Malloc-LLVM ([target : Operand] [size : Integer]) #:transparent)
(struct Free-LLVM ([target : Operand]) #:transparent)
(struct Load-LLVM ([type : Type] [source : Operand] [target : Operand] [indirection : Integer]) #:transparent)
(struct Store-LLVM ([type : Type] [source : Operand] [target : Operand] [indirection : Integer]) #:transparent)
(struct Print-LLVM ([operand : Operand] [endl : Boolean] [target : Operand]) #:transparent)
(struct Scan-LLVM ([var : Operand] [result : Operand]) #:transparent)
(define-type Branch (U Branch-LLVM Cond-Branch-LLVM))
(struct Branch-LLVM ([block : Block]) #:transparent)
(struct Cond-Branch-LLVM ([cond : Operand] [true : Block] [false : Block]) #:transparent)
(struct Return-Expr-LLVM ([type : Type] [expr : Operand]) #:transparent)
(struct Return-Void-LLVM [] #:transparent)
(struct Phi-LLVM ([var : Symbol] [type : Type] [values : (Listof Phi-Value)] [target : Operand] [complete : Boolean]) #:transparent #:mutable)
(struct Move-LLVM ([target : Operand] [source : Operand]) #:transparent)

;; LLVM operators
(define-type LLVM-Operator (U Arith-Operator Bool-Operator Comp-Operator Size-Operator))
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
(struct Parameter ([num : Integer]) #:transparent)
(struct Special-Register ([name : Register-Name]) #:transparent)
(define-type Register-Name (U 'sp))
(define-type Operand (U Literal Local Global Intermediate Register Block Special-Register Parameter))
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
(struct Struct-LLVM ([name : Symbol] [fields : (Listof Type)]) #:transparent)
(struct Global-LLVM ([name : Symbol] [type : Type]) #:transparent)
(struct LLVM-Program ([structs : (Listof Struct-LLVM)]
                      [globals : (Listof Global-LLVM)]
                      [func-CFGs : (Listof CFG)]
                      [c-lib-funcs : C-Lib-Funcs])
  #:transparent)

;; Compiler Specifications/Information
(define-type Compiler-Mode (U 'stack 'registers))
(define-type Output-Type (U 'LLVM 'asssembly))

;; ARM INSTRUCTIONS

(define-type ARM-Instr (U Arith-ARM Bool-ARM Move-ARM Branch-Label-ARM Comp-ARM Move-Cond-ARM Move-Word-ARM
                          Move-Word-Lower-ARM Move-Word-Constant-ARM Move-Top-ARM Move-Top-Constant-ARM Load-Reg-ARM
                          Load-Stack-ARM Store-Reg-ARM Store-Stack-ARM Branch-Equal-ARM Branch-Not-Equal-ARM Branch-ARM
                          Push-ARM Pop-ARM))
(struct Arith-ARM ([operator : Arith-Operator-ARM] [target : Operand] [left : Operand] [right : Operand]) #:transparent #:mutable)
(struct Bool-ARM ([operator : Bool-Operator] [target : Operand] [left : Operand] [right : Operand]) #:transparent)
(struct Move-ARM ([target : Operand] [source : Operand]) #:transparent)
(struct Branch-Label-ARM ([label : Symbol] [args : (Listof (U Operand Symbol))] [targets : (Listof Operand)] [store-stack-ARMs : (Listof Store-Stack-ARM)]) #:transparent)
(struct Comp-ARM ([left : Operand] [right : Operand]) #:transparent)
(struct Move-Cond-ARM ([operator : Comp-Operator] [target : Operand] [source : Operand]) #:transparent)
(struct Move-Word-ARM ([target : Operand] [source : Operand]) #:transparent)
(struct Move-Word-Lower-ARM ([target : Operand] [source : Integer]) #:transparent)
(struct Move-Word-Constant-ARM ([target : Operand] [constant : Symbol]) #:transparent)
(struct Move-Top-ARM ([target : Operand] [source : Integer]) #:transparent)
(struct Move-Top-Constant-ARM ([target : Operand] [constant : Symbol]) #:transparent)
(struct Load-Reg-ARM ([target : Operand] [source : Operand]) #:transparent)
(struct Load-Stack-ARM ([target : Operand] [stack-offset : Integer] [base-pointer : Stack-Pointer-Type]) #:transparent)
(struct Store-Reg-ARM ([source : Operand] [target : Operand]) #:transparent)
(struct Store-Stack-ARM ([source : Operand] [stack-offset : Integer] [base-pointer : Stack-Pointer-Type]) #:transparent)
(struct Branch-Equal-ARM ([block-label : Integer]) #:transparent)
(struct Branch-Not-Equal-ARM ([block-label : Integer]) #:transparent)
(struct Branch-ARM ([block-label : Integer]) #:transparent)
(struct Push-ARM ([registers : (Listof Integer)]) #:transparent)
(struct Pop-ARM ([registers : (Listof Integer)]) #:transparent)

(define-type ARM-Operator (U Arith-Operator-ARM Bool-Operator Comp-Operator))
(define-type Arith-Operator-ARM (U '+ '- '*))
(struct Lib-Funcs ([c-lib-funcs : C-Lib-Funcs] [division : Boolean]) #:transparent #:mutable)
(struct Global-ARM ([id : Symbol] [size : Integer]) #:transparent)
(struct ARM-Program ([global-decs : (Listof Global-ARM)]
                     [func-CFGs : (Listof CFG)]
                     [lib-funcs : Lib-Funcs])
  #:transparent)
(define-type Stack-Pointer-Type (U 'fp 'sp))
(struct Edge ([first-block : Block] [next-block : Block]) #:transparent)

;; REGISTER ALLOCATION
(struct I-Node ([operand : Operand]
                [interferences : (Setof Operand)]
                [degree : Integer]
                [color : Integer]
                [spilled : Boolean]
                [spill-cost : Real]
                [insert-move : Boolean])
  #:transparent #:mutable)
(define-type Interference-Graph (Mutable-HashTable Operand I-Node))
(struct Memory-Access ([loads : (Listof ARM-Instr)] [stores : (Listof ARM-Instr)]) #:transparent #:mutable)

;; USELESS CODE ELIMINATION
(struct Use-Def-Info ([operand : Operand] [def-block-label : Integer] [def-LLVM : (Listof LLVM-Instr)] [use-LLVMs : (Listof LLVM-Instr)]) #:transparent #:mutable)
(define-type Use-Def-Table (Mutable-HashTable Operand Use-Def-Info))