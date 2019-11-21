(ns src.core-test
  (:require [clojure.test :refer :all]
            [wasm.core :as wasm]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(do
  (->> (wasm/compile-wasm
         '(module

            (defn add [^i32 a ^i32 b]
              (let [x 1]
                (if (= a x)
                  (+ a x)
                  (+ a b))))

            (defn main []
              (add 9 8))))
       (spit "sample.wat"))

  (->> (sh/sh "wasmtime" "sample.wat" "--invoke=main")
       :out str/trim println))
