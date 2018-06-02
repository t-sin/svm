;; this is a sample code for svm-as

.data
    testnum 42
    teststr "fourty two"

.code
    load %testnum $r0  ; load from memory
    load 43 $r1        ; load from immediate
    add $r1 $2 $r3     ; add 42 + 43
    store $r3 &400     ; store into addr (400)
    hw                 ; hello assembly world!

