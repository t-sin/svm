# Textual Memory

Virtual machine of *SVM* has a memory that stores binary data but can be seen as a string directly.


## physical memory representation

SVM's *physical memory representation (physical representation)* is just a string that sequence of hex representations of bytes. PR example is below:

```
00aabbccff112233440000000000000000aabb1122aabb1122aabb1122......
```

## low-level structured representation

SVM's *low-level structured representation* is a multi-line string that are separated by segement name lines like `.text`.

```
.vm

00aabbccff11223344

.bss

0000000000000000

.data

aabb1122aabb1122aabb1122

.text

...

.stack

...
```

## 