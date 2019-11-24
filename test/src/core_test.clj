(ns src.core-test
  (:require [clojure.test :refer :all]
            [wasm.core :as wasm]
            [clojure.java.shell :as sh]))

(do
  (->> (wasm/compile-wasm
         '(module

            (defn true? [^i32 x]
              (= x true))

            (defn false? [^i32 x]
              (= x false))

            (defn add [^i32 a ^i32 b]
              (let [x 1]
                (if (= a x)
                  (+ a x)
                  (+ a b))))

            (defn ^:export main []
              (add 9 8))))
       (spit "sample.wat"))

  (->> (sh/sh "wasmtime" "sample.wat" "--invoke=main")))
