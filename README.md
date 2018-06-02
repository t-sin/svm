# String VM


String VM is a toy virtual machine. String VM is characterised with these features:

- register based
- codes and datum are stored in the same memory

## Registers

- `pc`: program counter
- `r0` ~ `r6`: general purpus register

## Data types

- bytes
- integers
- UTF-8 character strings

## Instructions

### Arithmetic instructions

- `add`
- `mul`
- `div`

### Memory access instructions

- `load`
- `store`

### Flow controlling instructions

- `ifeq`
- `ifneq`
- `jump`

### Miscellaneous instructions

- `nop`
- `hw`
