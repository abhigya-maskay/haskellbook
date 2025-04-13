2)
```haskell
f :: zed -> Zed -> Blah
```
a. zed: fully polymorphic
b. Zed: concrete
c. Blah: concrete

3)
```haskell
f :: Enum b => a -> b -> c
```
a. a: fully polymorphic
b. b: constrained polymorphic 
c. c: fully polymorphic

4)
```haskell
f :: f -> g -> C
```
a. f: fully polymorphic
b. g: fully polymorphic
c. C: concrete
