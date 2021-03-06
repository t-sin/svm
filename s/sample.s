;; this is a sample code for svm-as

.data
    testnum 42
    teststr "fourty two"

.code
    load %testnum $r0  ; load from memory
    load 43 $r1        ; load from immediate
    add $r0 $r1 $r2    ; add 42 + 43
    store $r2 &100     ; store into addr (100)

    load 20 $r4
    load 21 $r5
    ifeq :END $r4 $r5  ; junp to :END if 20 ($4) is equal to 21 ($r5)
    hw                 ; hello assembly world!
:END
    exit               ; exit program
