# Space VM

> I wanna die...
> 
> --- "I can't be the only one, baby", Space Dandy

Space VM, it is a virtual machine in Space. Space VM is characterised with these features:

- register based
- codes and datum are stored in the same memory


## Goals

- Understand programming languages
- Understand basics of computer architecture


## Tools

- assembler (Common Lisp)
- virtual machine (Common Lisp)

## Simple usage

```
$ ros run
* (ql:quickload :svm)
* (multiple-value-bind (vm p)
      (svm:init-vm #P"s/sample.s")
    (declare (ignore p))
    (let ((svm-vm/vm/virtual-machine::*print-op* t)
          (svm-vm/vm/virtual-machine::*print-register* t)
          (svm-vm/vm/virtual-machine::*print-memory* nil))
      (svm:run-program vm)))
```

## About VM

- memory format -> see [vm/README.md](vm/README.md)
- instructions -> see [instruction.lisp](instruction.lisp)

### Registers

- `pc`: program counter
- `r0` ~ `r6`: general purpus register

### Data types

- byte
- bytes
- UTF-8 characters
- UTF-8 character strings
