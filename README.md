## ISeq

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
* IPush {value}
    * Constant
* INeg
    * Negates the top value
* IJump {label number}
    * Just jumps to the label
* IZeroJump
    * Consumes 1 value and jumps to the label only when the value is 0
* ILabel
    * This doesn't do anything by itself
* ISetLocal {variable name}
    * Consumes 1 value and binds the name and the value. This lasts until (1) another ISetLocal or (2) the function ends
* IGetLocal {variable name}
    * Gets the value of the variable and stores it
