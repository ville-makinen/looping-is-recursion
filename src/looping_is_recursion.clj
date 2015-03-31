; recursion-PROJEKTISSA ON 3 TEHTÄVÄÄ RÄSTISSÄ..

(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [luku acc n]
                 (if (zero? n)
                   acc
                   (recur luku (* acc luku) (dec n))))]
    (helper base 1 exp)))

(power 2 2)


(first [])

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (empty? (rest a-seq))
                   (first a-seq)
                   (recur (rest a-seq))))]
    (helper a-seq)))

(last-element [])
(last-element [1 2 3]) ;=> 3
(last-element [2 5])   ;=> 5


(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                  (and (empty? seq1) (empty? seq2))
                    true
                  (or (and (empty? seq1) (not (empty? seq2))) (and (empty? seq2) (not (empty? seq1))))
                    false
                  (not (= (first seq1) (first seq2)))
                    false
                  :else
                    (recur (rest seq1) (rest seq2))))]
    (helper seq1 seq2)))


(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [1 3 5])   ;=> false



(defn find-first-index [pred a-seq]
  (loop [hakuseq a-seq
         n 0]
    (cond
     (empty? hakuseq)
       nil
     (pred (first hakuseq))
       n
     :else
       (recur (rest hakuseq) (inc n)))))

(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
(find-first-index zero? [1 1 3 7 2])                          ;=> nil
(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
(find-first-index nil? [])                                    ;=> nil

(defn avg [a-seq]
  (loop [hakuseq a-seq
         summa 0
         maara 0]
    (cond
     (empty? hakuseq)
       (/ summa maara)
     :else
       (recur (rest hakuseq) (+ summa (first hakuseq)) (inc maara)))))

(avg [1 2 3])   ;=> 2
(avg [0 0 0 4]) ;=> 1
(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5




(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))]
    (loop [hakuseq a-seq
           returnset #{}]
      (if (empty? hakuseq)
        returnset
        (recur (rest hakuseq) (toggle returnset (first hakuseq)))))))

(parity [:a :b :c])           ;=> #{:a :b :c}
(parity [:a :b :c :a])        ;=> #{:b :c}
(parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}

(defn fast-fibo [n]
  (if (= n 0)
     0
     (loop [iter 1
            current 1
            preceeding 0]
       (if (= n iter)
         current
         (recur (inc iter) (+ current preceeding) current)))))

(fast-fibo 0) ;=> 0
(fast-fibo 1) ;=> 1
(fast-fibo 2) ;=> 1
(fast-fibo 3) ;=> 2
(fast-fibo 4) ;=> 3
(fast-fibo 5) ;=> 5
(fast-fibo 6) ;=> 8


(concat [1 2 3 4] [1])

(defn cut-at-repetition [a-seq]
  (loop [haku-seq a-seq
         nahty-set #{}
         palautus-seq '()]
    (if (or (empty? haku-seq) (not (= (get nahty-set (first haku-seq)) nil)))
      palautus-seq
      (recur (rest haku-seq) (conj nahty-set (first haku-seq)) (concat palautus-seq [(first haku-seq)])))))


(cut-at-repetition [1 1 1 1 1])
;=> [1] doesn't have to be a vector, a sequence is fine too
(cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;=> [:cat :dog :house :milk 1]
(cut-at-repetition [0 1 2 3 4 5])
;=> [0 1 2 3 4 5]
