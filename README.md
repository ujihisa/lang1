# lang1

Program

    main args: (print 123)

Instruction set (type 1: stackmachine VM)

    ISetLocal "args"
    IPush 123
    ICall "print"

Instruction set (type 1: stackmachine VM)

    IRegMovVal 123 r1
    IRegCall1 "print" r1
    IRegMov r1 r0

Result (no matter which VM you used)

    123

* Written in Haskell
* Written by Tatsuhiro Ujihisa
    * <http://twitter.com/ujm>
* Some optimizations included
    * Tail call optimization

## ISeq (Stack Machine)

    data Inst = IPlus | IMult | ICall String | IPush Int |
      ILt | INeg | IZeroJump Int | IJump Int | ILabel Int |
      ISetLocal String | IGetLocal String deriving (Show, Eq)

* IPlus
* IMult
* ILt
    * Consumes 2 values and stores the result of plus/mult/less-than by the two values
* ICall {function name}
    * Consumes 1 value and calls a function with the value as the argument
    * The callee has to push the argument manually
* ITailCall {function name}
    * For optimization
    * Ends the current function immediately after calling the given function
    * Other behaviours are same to ICall
* IPush {value}
    * Constant
* INeg
    * Negates the top value
* IJump {label number}
    * Just jumps to the label
    * You can only go forward
* IZeroJump
    * Consumes 1 value and jumps to the label only when the value is 0
    * You can only go forward
* ILabel
    * This doesn't do anything by itself
* ISetLocal {variable name}
    * Consumes 1 value and binds the name and the value. This lasts until (1) another ISetLocal or (2) the function ends
* IGetLocal {variable name}
    * Gets the value of the variable and stores it

## ISeq (Register Machine)

    data InstReg =
      IRegMov Register Register |
      IRegAdd Register Register |
      IRegNeg Register |
      IRegLt Register Register |
      IRegMult Register Register |
      IRegMovVal Int Register |
      IRegCall1 String Register |
      IRegTailCall1 String Register |
      IRegZeroJump Register Int |
      IRegJump Int |
      IRegLabel Int |
      IRegVar_ String Register
      deriving (Show, Eq)

* IRegMov {register 1} {register 2}
    * copys the data in {register 1} to {register 2}
* IRegAdd {register 1} {register 2}
    * adds the data in {register 1} and {register 2} and save it to {register 2}
* IRegNeg {register}
    * nagates {register}'s value and update it
* IRegLt {register 1} {register 2}
    * compares {register 1} and {register 2}. If the left is smaller than right, the right becomes 1. otherwise 0.
* IRegMult {register 1} {register 2}
    * adds the data in {register 1} and {register 2} and save it to {register 2}
* IRegMovVal {int} {register}
    * copys immediate value {int} to {register}
* IRegCall1 {name} {register}
    * calls a function with argument {register}, and saves the return value there.
* IRegTailCall1 {name} {register}
    * For optimization
    * Ends the current function immediately after calling the given function
    * Other behaviours are same to IRegCall1
    * This implicitly movs from {register} to r0
* IRegZeroJump {register} {label}
    * if the value of {register} is 0, jumps to the label.
* IRegJump {label}
    * just jumpsts to the label
* IRegLabel {label}
    * the goal of the jumps
* IRegVar\_ {name} {register}
    * internal use only: refers a variable name and saves it in {register}.
    * the compiler temporary uses this instruction and replaces this with IRegMov.

r0 is always for the function argument and the return value.

Every time a function is called, the runtime system creates new set of registers, saving old registers into the register-stack.

## ISeq comparison

    f n: (+ (let x (+ n 1) (print x)) (print n))

stackmachine:

    ISetLocal "n"
    IGetLocal "n"
    IPush 1
    IPlus
    ISetLocal "x"
    IGetLocal "x"
    ICall "print"
    IGetLocal "n"
    ICall "print"
    IPlus

registermachine:

    IRegMov r0 r1
    IRegMovVal 1 r2
    IRegAdd r1 r2
    IRegMov r2 r1
    IRegCall1 "print" r1
    IRegMov r0 r2
    IRegCall1 "print" r2
    IRegAdd r1 r2
    IRegMov r2 r0

## Tail Call Optimization

Both stackmachine VM and registermachine VM optimize tail calls to jumps.

    main args: (print (deep 1000000))
    deep n: (if (< n 1) (print 1) (deep (- n 1)))

run

    $ ghc -O3 lang1 -o lang1 -rtsopts
    $ ./lang1 +RTS -K1K -RTS

This will cause stack overflow unless you apply optimizers. Otherwise even though it will take time, it succeeds.
