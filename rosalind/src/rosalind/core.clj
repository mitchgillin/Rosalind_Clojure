(ns rosalind.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn -bpfreq
  "Returns the frequencies of a given text.txt file in the order A C G T"
  []
  (let [text (slurp "bpfreq.txt")]
    (print ((frequencies text) \A)((frequencies text) \C)((frequencies text) \G)((frequencies text) \T))
    )
  )

(defn transcribe
  "Transcribes DNA to RNA"
  []
  (let [text (slurp "transcribe.txt")]
    (str/replace text #"T" "U")
    ))

(defn reversecomp
  "Returns the Reverse Compliment of the inputted text"
  []()
  (let [text (slurp "r3.txt")]
    ( str/reverse (clojure.string/replace text #"A|T|C|G" {"A" "T" "T" "A" "C" "G" "G" "C"}))))

(defn threadComp
  "Returns the Reverse Compliment of the inputted text using thread macro"
  []
  (let [text (slurp "r3.txt")]
    (-> text (str/reverse) (clojure.string/replace #"A|T|C|G" {"A" "T" "T" "A" "C" "G" "G" "C"}))))

(defn calcRabbits
  "Starting with 1 pair of newborn rabbits that take 2 periods to produce k pairs of newborn rabbits, who also take 2 periods to produce k pairs of newborn rabbits ... return the total number of rabbits after n months."
  [n k]

  (let [n n k k myvals [[0 0 1 1]]]
    (loop [month 1 myvals myvals]
      (if (= month n)
        (last (last myvals))
        (do
          (let [r ((myvals (- month 1) )0) g ((myvals (- month 1)) 1) n ((myvals (- month 1)) 2) t ((myvals (- month 1)) 3) ]
            (recur (inc month)  (vec (concat myvals [[ (+ r g) n (* (+ println g) k) (+ (+ r g) n (* (+ r g) k) )]]))  )
            )
        )
      )
     )
  )
 )

(defn pointMutations
  []
  (let [text (slurp "mlt.txt" ) nl-index (str/index-of (slurp "mlt.txt")"\n") str1 (subs (slurp "mlt.txt") 0 nl-index) str2 (subs (slurp "mlt.txt") (+ 1 nl-index)  ) ]
    (loop [differences 0 str1 str1 str2 str2]
      (cond
        (= (count str1) 0) (- (count (slurp "mlt.txt")) differences)
        (= 1 1)(if (= (first str1) (first str2))
          (recur (inc differences) (subs str1 1) (subs str2 1))
          (recur differences (subs str1 1) (subs str2 1))
          )
        )
      )
    )
  )

(def codonTable {:UUU "F" :CUU "L"    :AUU "I"      :GUU "V"
  :UUC "F"      :CUC "L"      :AUC "I"      :GUC "V"
  :UUA "L"      :CUA "L"      :AUA "I"      :GUA "V"
  :UUG "L"      :CUG "L"      :AUG "M"      :GUG "V"
  :UCU "S"      :CCU "P"      :ACU "T"      :GCU "A"
  :UCC "S"      :CCC "P"      :ACC "T"      :GCC "A"
  :UCA "S"      :CCA "P"      :ACA "T"      :GCA "A"
  :UCG "S"      :CCG "P"      :ACG "T"      :GCG "A"
  :UAU "Y"      :CAU "H"      :AAU "N"      :GAU "D"
  :UAC "Y"      :CAC "H"      :AAC "N"      :GAC "D"
  :UAA "Stop"   :CAA "Q"      :AAA "K"      :GAA "E"
  :UAG "Stop"   :CAG "Q"      :AAG "K"      :GAG "E"
  :UGU "C"      :CGU "R"      :AGU "S"      :GGU "G"
  :UGC "C"      :CGC "R"      :AGC "S"      :GGC "G"
  :UGA "Stop"   :CGA "R"      :AGA "R"      :GGA "G"
  :UGG "W"      :CGG "R"      :AGG "R"      :GGG "G" })

(defn getCodon
  [triplet]
  (codonTable (keyword triplet))
  )

(defn convertToProteins
  [rawBP]
  (loop [rawBP rawBP protein ""]
    (if (= (count rawBP) 0)
      protein
      (recur (subs rawBP 3) (str protein (getCodon (subs rawBP 0 3))))
      )

    )
  )


(defn calcGCPercent
  [text]

  (float(/ (+ ((frequencies text) \G ) ((frequencies text) \C)) (- (count text) ((frequencies text) \newline)  )))

  )


(defn calcGCPercentNoNewLines
  [text]
  (float(/ (+ ((frequencies text) \G ) ((frequencies text) \C) ) (count text))))



(defn gcContent
  []
  (let [text (slurp "rosalind_gc.txt")]
    (loop [text text startIndex 0 endIndex (+ startIndex 2) dataSets {}]
      (if (< (count text) 200)
        dataSets
        (do
          (println (count text))
                    (recur (subs text (str/index-of text ">" 2)) (str/index-of text ">" 2) (+ (str/index-of text ">" 2) 3) (into dataSets {(keyword (subs text startIndex (+ startIndex 14))) (+ (frequencies ))}))
          )
        )
      )
    )
  )


(defn gcContentV2
  []
  "Get the GC content of a passed in string. May contain newline charachters "
  (let [text (str/replace (slurp "rosalind_gc.txt") "\n" "")]  ;;Removes the newlines from the text
    (loop [text text dataSet {}] ;;Sets up the loop structure; Ultimately we want the text to be subs'd so the string loses a >rosalind each time
      (if (= (count text) 0)
        (println (key (apply max-key val dataSet)) "\n" (dataSet (key (apply max-key val dataSet))) )
        (recur (subs text 0 (str/last-index-of text ">"))  (into dataSet {(keyword (subs text (str/last-index-of text ">") (+ (str/last-index-of text ">") 14 )))  (calcGCPercentNoNewLines (subs text (+(str/last-index-of text ">")14)))  })
            )
        )
      )
    )

)
