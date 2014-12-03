(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [base exp acc]
                  (cond
                    (zero? exp) acc
                    :else (recur base (dec exp) (* acc base))))]

  (power-helper base exp 1)))

(defn last-element [a-seq]
  (cond
    (empty? (rest a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (empty? a-seq) false
    (empty? b-seq) false
    (not (= (first a-seq) (first b-seq))) false 
    :else (recur (rest a-seq) (rest b-seq))))

(defn find-first-index [pred a-seq]
  (loop [index 0 
         b-seq a-seq]
    (cond
      (empty? b-seq) nil
      (pred (first b-seq)) index
      :else (recur (inc index) (rest b-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         n   0
         b-seq a-seq]
    (cond
      (empty? b-seq) (if (zero? n)
                        0
                        (/ acc n))
      :else (recur (+ acc (first b-seq)) (inc n) (rest b-seq)))))

(defn parity [a-seq]
  (loop [result '#{}
         b-seq  a-seq]

    (let [elem (first b-seq)]
      (cond
        (empty? b-seq) result
        (contains? result elem) (recur (disj result elem) (rest b-seq))
        :else (recur (conj result elem) (rest b-seq))))))
    

(defn fast-fibo [n]
  (loop [f-n-1 0
         f-n   1
         nn    n]

    (cond
      (<= nn 0) f-n-1
      :else (recur f-n (+ f-n-1 f-n) (dec nn)))))
    

(defn cut-at-repetition [a-seq]
  (loop [result '[]
         result-set '#{}
         b-seq a-seq]

    (let [elem (first b-seq)]
      (cond
        (empty? b-seq) result
        (contains? result-set elem) result
        :else (recur (conj result elem) (conj result-set elem) (rest b-seq))))))
    





