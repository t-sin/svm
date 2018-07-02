# String Virtual Machine


## Memory

SVM has a memory segmented as bytes. The memory holds data as a single or multiple bytes.

Any things can be a memory satisfies some requirements. These requriments are belows:

- can be created as single data structure
- can read/write a byte through accessor function
- can dump through stringify the memory (to debugging or to seriarizing)

## Data representation

All data in SVM have each representation, *based on bytes*.

The smallest data is a byte, but some data types are needed multiple bytes because of single
byte is too small to express. Multiple-bytes data are coded with ordering **little endian**
-- least significant byte are placed as left, and most significant byte are placed at left.

### Program

The program which stores in the memory consists of two parts:

1. data area
2. code area

*Data area* stores intermediate values in assembly code. *Code area* stores program code
itsself as binary instruction format.

*Data* are represented as and consist of, a byte or multiple bytes.

*Code* is a program run by VM. This is described at chapter below.

#### Data representation format

Basically, single/multiple byte data are represented with its length, it means, max
length of multiple-byte data is 255.

```
+-------+------+
| 8-bit | ...  |
+-------+------+
| type  | data |
+-------+------+
```

##### Types

SVM processes four kind of data types: byte, byte array, characters and character strings.

```
| type ID | type name  | description             | data                             |
|---------|------------+-------------------------+----------------------------------|
| 0       | byte       | single byte             | type, byte                       |
| 1       | bytes      | multiple byte           | type, length, byte1, byte2, ...  |
| 2       | UTF-8 char | character: multi-bytes  | type, byte1, byte2, byte3, byte4 |
| 3       | UTF-8 str  | multi-char              | type, length, byte1, byte2, ...  |
```

#### Binary instruction format

One operation is represented as two bytes (16 bits), and this representation is called
the *word*. Detailed format as follows:

```
+--------+--------+---------+---------+----------+--------+
| 2-bit  | 6-bit  | 4-bit   | 4-bit   | 4-bit    | 4-bit  |
+--------+--------+---------+---------+----------+--------+
| type   | opcode | oprand1 | oprand2 | operand3 | unused |
+--------+--------+---------+---------+----------+--------+
```

##### Operands

Operand of each operation consists of *intermediate flag* (first bit) and resister number.

```
+-------+----------+
| 1-bit | 3-bit    |
+-------+----------+
| addr? | register |
+-------+----------+
```
