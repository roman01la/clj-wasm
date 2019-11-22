_Clojure-flavored WASM's text format_

Turns this

```clojure
(module
  (defn add [^i32 a ^i32 b]
    (let [x 1]
      (if (= a x)
        (+ a x)
        (+ a b))))

  (defn main []
    (add 9 8)))
```

into this

```lisp
(module
  (type $return_add (func (param $a i32) (param $b i32) (result i32)))

  (func $add (param $a i32) (param $b i32) (result i32) (local $x i32)
    (block (result i32)
      (local.set $x (i32.const 1))
      (if (result i32)
        (i32.eq
          (local.get $x)
          (local.get $a))
        (then
          (i32.add
            (local.get $x)
            (local.get $a)))
        (else
          (i32.add
            (local.get $b)
            (local.get $a))))))

  (export "add" (func $add))

  (type $return_main (func (result i32)))

  (func $main (result i32)
    (call $add
      (i32.const 9)
      (i32.const 8)))

  (export "main" (func $main)))
```
