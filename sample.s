;; this is a sample code for svm-as

.data
    testnum 42
    teststr "fourty two"

.code
    load %testnum $r0  ; load from memory
    load 43 $r1        ; load from immediate
    add $r1 $r2 $r3    ; add 42 + 43
    store $r3 &400     ; store into addr (400)

    load 20 $r4
    load 21 $r5
    ifeq $r4 $r5 :END   ; junp to :END if 20 ($4) is equal to 21 ($r5)
    hw                 ; hello assembly world!
:END
    exit               ; exit program
