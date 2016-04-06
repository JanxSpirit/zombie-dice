(ns zombie-dice.core
  (:gen-class))

(def dice-values
  {:green [:run :run :shotgun :brain :brain :brain]
   :yellow [:run :run :shotgun :shotgun :brain :brain]
   :red [:run :run :shotgun :shotgun :shotgun :brain]
   })

(defn dice-color-count [die-triples] (reduce
                                      (fn [coll die]
                                        (let [[color _] die]
                                          (assoc coll color (inc (color coll 0))))) {} die-triples))

(defn dice-values-count [die-triples] (reduce
                                       (fn [coll die]
                                         (let [[_ value] die]
                                           (assoc coll value (inc (value coll 0))))) {} die-triples))

(defn dice-values-map [die-triples]
  (reduce (fn [coll die]
            (let [[color value] die]
              (assoc coll value (conj (value coll []) color)))) {} die-triples))

(defn dice [count used] (take count (shuffle (flatten (map #(replicate (- 5 (% used 0)) %) (keys dice-values))))))

;;this needs the complete set of die-triples for the entire game as arg so that we can pass complete set of used dice to (dice)
;;take the :run triples and roll those plus enough others to make 3
(defn roll [die-triples] (let [run-dice (:run (dice-values-map die-triples))]
                           (map (fn [die]
                                  [die (first (shuffle (die dice-values)))])
                           (concat run-dice (dice (- 3 (count run-dice)) (dice-color-count die-triples))))))

#_(defn get-dice [die-triples value]
  (reduce
   (fn [coll die]
     (let [[k v] die]
       (if (= value v)
         (conj coll k)
         coll)))
   []
   die-triples))

(defn pretty-print [die-triples]
  (let [dice-values (dice-values-map die-triples)]
    (println)
    (println "------------------------------")
    (println "| Brains(" (count (:brain dice-values))
             ") | Run(" (count (:run dice-values))
             ") | Shotguns(" (count (:shotgun dice-values)) ") |" )
    (println "------------------------------")
    (println "| " (:brain dice-values) " | " (:run dice-values) " | " (:shotgun dice-values) " |")
    (println "------------------------------")
    (println)))

(defn roll-dice [running-score] (let [roll-outcome (roll running-score)
                                      current-running-score (concat (filter
                                                                     (fn [pair] (let [[k v] pair] (not= v :run)))
                                                                     running-score)
                                                                    roll-outcome)
                                      shotgun-dice (:shotgun (dice-values-map current-running-score) [])
                                      brain-dice (:brain (dice-values-map current-running-score) [])]
                                  (pretty-print current-running-score)
                                  (if (> (count shotgun-dice) 2)
                                    (println "You got shotgunned " (count shotgun-dice) " times!!!")
                                    (do (println "roll again?")
                                      (if (= "y" (read-line))
                                        (roll-dice current-running-score)
                                        (println "Congratulations! You ate "
                                                 (count brain-dice)
                                                 " BRAAAAAAIIIIIINNNSSS!"))))))

(defn -main
  "I play zombie dice."
  [& args]
  (do (println "Welcome to ZOMBIE DICE!")
    (roll-dice [])))
