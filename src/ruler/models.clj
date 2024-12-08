(ns ^:no-doc ruler.models)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Const definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce keys-req #{:req :req-depends :req-fn})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- type->types [i]
  (cond
    (= Integer i)
    #{Integer Long Short Byte}
    :else
    #{i}))

(defn- ->err [err k p]
  (when err
    {:key k :pred p}))

(defn- type? [t v]
  (some #(instance? % v) (type->types t)))

(defn- req-key? [k]
  (contains? keys-req k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- map->coll [model]
  (letfn [(add-k [ac k v]
            (conj ac (assoc v :key k)))]
    (reduce-kv add-k [] model)))

(defn parse-structure [model]
  (if (vector? model)
    model
    (map->coll model)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys models validations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn key-dispatcher
  "Receive a key-rule to validate the rule and data.
   If the key-rule is one of the 'required' rules, dispatch the key;
   Else, if there is a value for the field defined in the rule, dispatch the key;
   Otherwise, dispatch for the :default where no error is returned.
   This behavior is designed to avoid validating more than 'required' rules on fields that don't have a value."
  [k {:keys [key]} data]
  (let [k-req? (req-key? k)
        val? (some? (get data key))
        dispatch? (or k-req? val?)]
    (if dispatch? k
        :default)))

(defmulti key-validation
  "Validates each rule key defined in the rule map.
   Get the field :key from the data map and check if it is required;
   Check for types and other existing rules."
  key-dispatcher)

(defmethod key-validation :default [_ _ _]
  nil)

(defmethod key-validation :type
  [k {:keys [key type]} data]
  (let [val (get data key)
        val? (some? val)
        err (and val? (not (type? type val)))]
    (->err err key k)))

(defmethod key-validation :req
  [k {:keys [key req]} data]
  (let [err (and req (nil? (get data key)))]
    (->err err key k)))

(defmethod key-validation :req-depends
  [k {:keys [key req-depends]} data]
  (let [values (vals (select-keys data req-depends))
        every-deps? (every? some? values)
        err (and every-deps? (nil? (get data key)))]
    (->err err key k)))

(defmethod key-validation :req-fn
  [k {:keys [key req-fn]} data]
  (let [req? (req-fn data)
        err (and req? (nil? (get data key)))]
    (->err err key k)))

(defmethod key-validation :excluded
  [k {:keys [key excluded]} data]
  (let [val (get data key)
        excluded-vals (vals (select-keys data excluded))
        excluded? (some some? excluded-vals)
        err (and val excluded?)]
    (->err err key k)))

(defmethod key-validation :excluded-fn
  [k {:keys [key excluded-fn]} data]
  (let [val (get data key)
        excluded? (excluded-fn data)
        err (and val excluded?)]
    (->err err key k)))

(defn- valid-limits?
  "Verify if 'val' is within the range of 'min' to 'max' (min <= val <= max)."
  [min max val]
  (>= max val min))

(defmethod key-validation :min
  [k {:keys [key min]} data]
  (when-let [val (get data key)]
    (when (number? val)
      (let [max Double/POSITIVE_INFINITY
            err (not (valid-limits? min max val))]
        (->err err key k)))))

(defmethod key-validation :max
  [k {:keys [key max]} data]
  (when-let [val (get data key)]
    (when (number? val)
      (let [min Double/NEGATIVE_INFINITY
            err (not (valid-limits? min max val))]
        (->err err key k)))))

(defmethod key-validation :min-length
  [k {:keys [key min-length]} data]
  (when-let [val (get data key)]
    (when (string? val)
      (let [max Integer/MAX_VALUE
            length (count val)
            err (not (valid-limits? min-length max length))]
        (->err err key k)))))

(defmethod key-validation :max-length
  [k {:keys [key max-length]} data]
  (when-let [val (get data key)]
    (when (string? val)
      (let [min -1
            length (count val)
            err (not (valid-limits? min max-length length))]
        (->err err key k)))))

(defmethod key-validation :length
  [k {:keys [key length]} data]
  (let [val  (get data key)
        val' (if (string? val) (count val) val)
        err  (not (valid-limits? length length val'))]
    (->err err key k)))

(defmethod key-validation :contains
  [k {:keys [key contains]} data]
  (let [val (get data key)
        err (not (contains? (set contains) val))]
    (->err err key k)))

(defmethod key-validation :format
  [k {:keys [key format]} data]
  (let [val (get data key)
        matches? (or (not (string? val)) (re-matches format val))
        err (not matches?)]
    (->err err key k)))

(defmethod key-validation :format-fn
  [k {:keys [key format-fn]} data]
  (let [val (get data key)
        err (not (format-fn val))]
    (->err err key k)))

(defmethod key-validation :of
  [k {:keys [key type of]} data]
  (let [val (get data key)
        err (or (not= clojure.lang.IPersistentVector type)
                (not-every? #(type? of %) val))]
    (->err err key k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Models opts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti opt-key-validation
  (fn [opt-kw _model _opts _data]
    opt-kw))

(defmethod opt-key-validation :default
  [_ _ _ _]
  nil)

(defmethod opt-key-validation :extra-keys?
  [k model opts data]
  (when (false? (k opts))
    (let [model-keys (map :key model)
          rest-map (apply dissoc data model-keys)
          rest-keys (keys rest-map)]
      (when (seq rest-keys)
        {:ruler/extra-keys? (vec rest-keys)}))))
