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

(defn- req-key? [k]
  (contains? keys-req k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Models.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn key-dispatcher [k {:keys [key]} data]
  (let [k-req? (req-key? k)
        val? (some? (get data key))]
    (cond
      k-req? k
      val?   k
      :else  :default)))

(defmulti key-validation key-dispatcher)

(defmethod key-validation :default [_ _ _]
  nil)

(defmethod key-validation :type
  [k {:keys [key type]} data]
  (let [val (get data key)
        val? (some? val)
        types (type->types type)
        no-instance? (not (some #(instance? % val) types))
        err (and val? no-instance?)]
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

(defn- valid-limits?
  "Verify if val is between max and min (max >= val >= min)."
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
        matches? (re-matches format val)
        err (not matches?)]
    (->err err key k)))

(defmethod key-validation :format-fn
  [k {:keys [key format-fn]} data]
  (let [val (get data key)
        err (not (format-fn val))]
    (->err err key k)))
