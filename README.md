# String VM

> I wanna die...
> 
> --- "I can't be a only one, baby", Space Dandy

String VM is a toy virtual machine. String VM is characterised with these features:

- register based
- codes and datum are stored in the same memory


## Goals

- Understand programming languages
- Run my Lisp!


## Tools

- assembler (Common Lisp)
- virtual machine (Common Lisp? C? Nim?)


## About VM

### Registers

- `pc`: program counter
- `r0` ~ `r6`: general purpus register

### Data types

- bytes
- integers
- UTF-8 character strings

### Instructions

#### Arithmetic instructions

- `add`
- `mul`
- `div`

#### Memory access instructions

- `load`
- `store`

#### Flow controlling instructions

- `ifeq`
- `ifneq`
- `jump`

#### Miscellaneous instructions

- `nop`
- `hw`
