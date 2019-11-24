(module (type $return_true? (func (param $x i32) (result i32)))
(func $true_QMARK_ (param $x i32) (result i32)
(i32.eq 
(i32.const 1) 
(local.get $x)))
(export "$true_QMARK_" (func $true_QMARK_))(type $return_false? (func (param $x i32) (result i32)))
(func $false_QMARK_ (param $x i32) (result i32)
(i32.eq 
(i32.const 0) 
(local.get $x)))
(export "$false_QMARK_" (func $false_QMARK_))(type $return_add (func (param $a i32) (param $b i32) (result i32)))
(func $add (param $a i32) (param $b i32) (result i32)(local $x i32) 

(block (result i32)

(local.set $x 
(i32.const 1))

(if (result i32) (i32.eq 
(local.get $x) 
(local.get $a))
  (then (i32.add 
(local.get $x) 
(local.get $a)))
  (else (i32.add 
(local.get $b) 
(local.get $a))))))
(export "$add" (func $add))(type $return_main (func (result i32)))
(func $main (result i32)

(call $add  
(i32.const 9) 
(i32.const 8)))
(export "main" (func $main)))