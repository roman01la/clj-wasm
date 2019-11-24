(ns wasm.core
  #?(:cljs (:import [goog.string StringBuffer]))
  (:require [clojure.string :as str]))

(def ^:dynamic ^StringBuilder out)
(def ^:dynamic compiler-env)
(def ^:dynamic *fn-locals*)

(defn default-compiler-env []
  (atom {:defs {}
         :defs-order []}))

(def specials
  '#{module ns
     def fn defn do
     if and let
     = + - / *})

(def types '#{i32 i64 f32 f64})

(defn emits [& strs]
  (doseq [s strs]
    (.append out (str s))))

(defn emitln
  ([]
   (emits "\n"))
  ([s]
   (emits "\n" s)))

(defn munge-name [s]
  (str "$"
    (-> s
        (str/replace "?" "_QMARK_")
        (str/replace "!" "_BANG_")
        (str/replace "*" "_STAR_")
        (str/replace "-" "_")
        (str/replace "+" "_PLUS_")
        (str/replace ">" "_GT_")
        (str/replace "<" "_LT_")
        (str/replace "=" "_EQ_"))))

(defmulti emit (fn [ast] (:op ast)))

(defmethod emit 'module [{:keys [body]}]
  (emits "(module ")
  (doseq [expr body]
    (emit expr))
  (emits ")"))

(defmethod emit 'ns [{:keys [name]}]
  (prn @compiler-env)
  (emitln "(table 0 funcref)"))

(defmethod emit 'id [{:keys [name]}]
  (emits (munge-name name)))

(defmethod emit 'param [{:keys [tag id]}]
  (emits "(param ")
  (emit id)
  (emits " " tag ") "))

(defmethod emit 'def [{:keys [name tag]}]
  (emits "(global ")
  (emit name)
  (emits " (import \"js\" \"global\") " tag ")"))

(defn emit-fn-ret-type [{:keys [name ret-tag args]}]
  (emits "(type $return_" (:name name) " (func ")
  (doseq [arg args]
    (emit arg))
  (emits "(result " ret-tag ")")
  (emits "))"))

(defn emit-local [id]
  (emits "(local ")
  (emit id)
  (emits " " (:tag id))
  (emits ") "))

(defmethod emit 'fn [{:keys [ret-tag name locals args body] :as ast}]
  #_#_#_(emits "(elem (i32.const "
               (dec (count (:defs-order @compiler-env)))
               ") ")
      (emit name)
      (emits ")")
  (emit-fn-ret-type ast)

  (emitln "(func ")
  (emit name)
  (emits " ")
  (doseq [arg args]
    (emit arg))
  (when ret-tag
    (emits "(result " ret-tag ")"))
  (doseq [local (filter #(not= 'param (:op %)) locals)]
    (emit-local local))
  (doseq [expr body]
    (emitln)
    (emit expr))
  (emits ")")
  (emitln "(export ")
  (let [var-name (cond-> (:name name)
                         (-> ast :meta :export true? not) munge-name)]
    (emits "\"" var-name "\" (func "))
  (emit name)
  (emits "))"))

(defmethod emit '+ [{:keys [tag left right]}]
  (emits "(" tag ".add ")
  (emit right)
  (emits " ")
  (emit left)
  (emits ")"))

(defmethod emit '= [{:keys [tag left right]}]
  (emits "(" tag ".eq ")
  (emit right)
  (emits " ")
  (emit left)
  (emits ")"))

(defmethod emit 'and [{:keys [tag left right]}]
  (emits "(" tag ".and ")
  (emit right)
  (emits " ")
  (emit left)
  (emits ")"))

(defmethod emit 'block [{:keys [tag body]}]
  (emitln "(block (result ")
  (emits tag ")")
  (doseq [expr body]
    (emitln)
    (emit expr))
  (emits ")"))

(defmethod emit 'local-get [{:keys [id]}]
  (emitln "(local.get ")
  (emit id)
  (emits ")"))

(defmethod emit 'local-set [{:keys [id value]}]
  (emitln "(local.set ")
  (emit id)
  (emits " ")
  (emit value)
  (emits ")"))

(defmethod emit 'const [{:keys [tag form]}]
  (emitln "(")
  (emits tag ".const " form ")"))

(defn emit-type [type]
  (emits "(type $return_" type ")"))

(defmethod emit 'invoke [{:keys [name args]}]
  (emitln "(call ")
  #_#_(emitln "(call_indirect ")
      (emit-type (:name name))
  (emit name)
  (emits " ")
  (doseq [expr args]
    (emits " ")
    (emit expr))
  (emits ")")
  #_(emits " (i32.const "
           (.indexOf ^PersistentVector (:defs-order @compiler-env) (:name name))
           "))"))

(defmethod emit 'if [{:keys [tag test then else]}]
  (emitln "(if ") (emits "(result " tag ") ") (emit test)
  (emitln "  ") (emits "(then ")
  (emit then) (emits ")")
  (emitln "  ") (emits "(else ")
  (emit else)
  (emits "))"))

(declare analyze)


(defmulti parse (fn [form env] (first form)))

(defn parse-symbol [v]
  {:op 'id
   :name v})

(defn parse-argument [id]
  (let [tag (-> id :name meta :tag)
        _ (assert (some? tag) "function argument should declare type")
        _ (assert (contains? types tag) "invalid function argument type")]
    {:op 'param
     :tag tag
     :id id
     :children [:id]}))

(defn parse-local [id]
  {:op 'local-get
   :id id
   :children [:id]})

(defn index-by [f coll]
  (->> coll
       (reduce (fn [ret v]
                 (assoc! ret (f v) v)
                 ret)
               (transient {}))
       persistent!))

(defmethod parse 'module [[_ & body] env]
  {:op 'module
   :body (map #(analyze % env) body)
   :children [:body]})

(defmethod parse 'def [[_ name] env]
  (let [tag (:tag (meta name))]
    {:op 'def
     :name (parse-symbol name)
     :tag tag}))

(defmethod parse 'fn [[_ args & body] env]
  (let [{ret-tag :tag name :name} (meta args)
        _ (when (some? ret-tag)
            (assert (contains? types ret-tag) "invalid function return type"))
        args (map (comp parse-argument #(analyze % env)) args)
        [body fn-locals] (binding [*fn-locals* (atom (index-by (comp :name :id) args))]
                           [(doall
                              (for [expr body]
                                (let [ast (analyze expr nil)]
                                  (if (= 'id (:op ast))
                                    (parse-local ast)
                                    ast))))
                            @*fn-locals*])
        inferred-ret-tag (-> body last :tag)
        _ (assert (or (some? inferred-ret-tag) (some? ret-tag))
                  (str "Couldn't infer return type of " name
                       ", add manual return type hint"))
        _ (when (and (some? inferred-ret-tag) (some? ret-tag))
            (assert (= inferred-ret-tag ret-tag)
                    (str "Return type " ret-tag " doesn't match inferred type "
                         inferred-ret-tag " in " name)))
        ast {:op 'fn
             :meta (meta name)
             :ret-tag (or inferred-ret-tag ret-tag)
             :name (parse-symbol name)
             :args args
             :locals (vals fn-locals)
             :body body
             :children [:name :args :locals :body]}]
    (swap! compiler-env assoc-in [:defs name] ast)
    (swap! compiler-env update :defs-order conj name)
    ast))

(defmethod parse 'defn [[_ name args & body] env]
  (parse `(~'fn ~(vary-meta args #(assoc % :name name)) ~@body)
         env))

(defmethod parse '+ [[_ left right] env]
  (let [left (let [e (analyze left env)]
               (if (= 'id (:op e))
                 (parse-local e)
                 e))
        right (let [e (analyze right env)]
                (if (= 'id (:op e))
                  (parse-local e)
                  e))
        ltag (if (= 'const (:op left))
               (:tag left)
               (get-in @*fn-locals* [(-> left :id :name) :tag]))
        rtag (if (= 'const (:op right))
               (:tag right)
               (get-in @*fn-locals* [(-> right :id :name) :tag]))
        _ (assert (= ltag rtag) (str "Can't + values of different types " ltag " and " rtag))]
    {:op '+
     :tag ltag
     :left left
     :right right
     :children [:left :right]}))

(defmethod parse '= [[_ left right] env]
  (let [left (let [e (analyze left env)]
               (if (= 'id (:op e))
                 (parse-local e)
                 e))
        right (let [e (analyze right env)]
                (if (= 'id (:op e))
                  (parse-local e)
                  e))
        ltag (if (= 'const (:op left))
               (:tag left)
               (get-in @*fn-locals* [(-> left :id :name) :tag]))
        rtag (if (= 'const (:op right))
               (:tag right)
               (get-in @*fn-locals* [(-> right :id :name) :tag]))
        _ (assert (= ltag rtag) (str "Can't = values of different types " ltag " and " rtag))]
    {:op '=
     :tag ltag
     :left left
     :right right
     :children [:left :right]}))

(defn add-types [& types]
  (let [tags (into #{} types)]
    (if (== 1 (count tags))
      (first tags)
      tags)))

(defmethod parse 'if [[_ test then else] env]
  (let [then (analyze then env)
        else (analyze else env)
        then-tag (:tag then)
        else-tag (:tag else)
        _ (assert (= then-tag else-tag)
                  (str "Can't return different types from if expression "
                       (or then-tag "nil") " and " (or else-tag "nil")))]
    {:op 'if
     :tag then-tag
     :test (analyze test env)
     :then then
     :else else
     :children [:test :then :else]}))

(defmethod parse 'and [[_ left right] env]
  (let [left (analyze left env)
        right (analyze right env)
        ltag (:tag left)
        rtag (:tag right)
        _ (assert (= ltag rtag) (str "Can't `and` values of different types " ltag " and " rtag))]
    {:op 'and
     :tag ltag
     :left left
     :right right
     :children [:left :right]}))

(defmethod parse 'do [[_ & body] env]
  (let [body (map #(analyze % env) body)
        tag (:tag (last body))]
    {:op 'block
     :tag tag
     :body body}))

(defn parse-local-binding [[sym value] env]
  (let [id (parse-symbol sym)
        value (analyze value env)
        _ (->> (:tag value)
               (assoc id :tag)
               (swap! *fn-locals* assoc (:name id)))]
    {:op 'local-set
     :id id
     :value value}))

(defmethod parse 'let [[_ bindings & let-body] env]
  (let [bindings (partition-all 2 bindings)
        _ (assert (== 2 (count (last bindings))) "let takes even number of forms")
        body (doall (map #(parse-local-binding % env) bindings))
        let-body (map #(analyze % env) let-body)
        tag (-> let-body last :tag)]
    {:op 'block
     :tag tag
     :body (concat body let-body)}))

(defmethod parse 'ns [[_ ns-name] env]
  {:op 'ns
   :name ns-name})

(defn parse-invoke [[s & args] env]
  (let [{tag :ret-tag fargs :args} (get-in @compiler-env [:defs s])
        _ (assert (== (count args) (count fargs))
                  (str "Wrong number of args (" (count args) ") "
                       "passed to: " s))]
    {:op 'invoke
     :name (parse-symbol s)
     :tag tag
     :args (map #(analyze % env) args)
     :children [:name :args]}))

(defn parse-number [form]
  (let [tag (cond
              (int? form) 'i32
              (float? form) 'f64)]
    {:op 'const
     :tag tag
     :form form}))

(defn parse-boolean [form]
  (if (true? form)
    (parse-number 1)
    (parse-number 0)))

(defn analyze-seq [form env]
  (cond
    (contains? specials (first form)) (parse form env)
    :else (parse-invoke form env)))

(defn analyze [form env]
  (cond
    (list? form) (analyze-seq form env)
    (symbol? form) (parse-symbol form)
    (number? form) (parse-number form)
    (boolean? form) (parse-boolean form)
    :else form))

(defn compile-wasm [form]
  (binding [out #?(:clj (StringBuilder.)
                   :cljs (StringBuffer.))
            compiler-env (default-compiler-env)]
    (emit (analyze form nil))
    (.toString out)))
