.data
    num1 10
    num2 20
    num3 30

.code
    load %num1 $r0
    ifeq :P2 %num1 $r0
:P1
    load %num2 $r1  ; this operation is not executed
:P2
    hw
    load %num3 $r2
    hw
    ifeq :P3 %num3 $r2
    hw  ; this operation is not executed
:P3
    exit
