### Dynamic Tail

```
-- prelude
Nat : Type
A : Type
tail : (n : Nat) -> Vec A (1 + n) -> Vec A n
----
dTail : (Dynamic {n : Nat} (Vec A n)) -> (Dynamic {n : Nat} (Vec A n))
dTail = DBind (\v => force {
  let m in
    tail m (coe (unify (Vec A n) (Vec A (1 + m))) v)
})
```

1. meta normalization
`(unify (Vec A n) (Vec A (1 + m)))`  =>  `VecInj refl (unify n (1 + m))`
2. eval `dTail (Ok (n = 1) [a])`
  - 
  ```
  (n = 1, v = [a]) |- force {
      let m in
        tail m (coe (VecInj refl (unify n (1 + m))) v)
    }
  ```
  - 
  ```
    (n = 1, v = [a]) | m = unknown |- 
        tail m (coe (VecInj refl (unify n (1 + m))) [a])
  ```
  - 
  ```
    (n = 1, v = [a]) | m = unknown |- 
        tail m (coe (VecInj refl (SucInj (unify 0 m))) [a])
  ```
  - 
  ```
    (n = 1, v = [a]) | m = 0 |- 
        tail m (coe refl [a])
  ```
  - 
  ```
    (n = 1, v = [a]) | m = 0 |- a
  ```