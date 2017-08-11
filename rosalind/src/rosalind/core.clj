(ns rosalind.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn -bpfreq
  []
  "Problem 1: DNA"
  "Returns the frequencies of a given text.txt file in the order A C G T"

  (let [text (slurp "bpfreq.txt")]
    (print ((frequencies text) \A)((frequencies text) \C)((frequencies text) \G)((frequencies text) \T))
    )
  )

(defn transcribe
  []
  "Problem 2: RNA"
  "Transcribes DNA to RNA"

  (let [text (slurp "transcribe.txt")]
    (str/replace text #"T" "U")
    ))

(defn reversecomp

  []
  "Problem 3: RevC"
  "Returns the Reverse Compliment of the inputted text"
  (let [text (slurp "r3.txt")]
    ( str/reverse (clojure.string/replace text #"A|T|C|G" {"A" "T" "T" "A" "C" "G" "G" "C"}))))

(defn threadComp
  []
  "Problem 3: RevC"
  "Returns the Reverse Compliment of the inputted text using thread macro"

  (let [text (slurp "r3.txt")]
    (-> text (str/reverse) (clojure.string/replace #"A|T|C|G" {"A" "T" "T" "A" "C" "G" "G" "C"}))))

(defn calcRabbits

  [n k]

  "Problem 4: Fib"
  "Starting with 1 pair of newborn rabbits that take 2 periods to produce k pairs of newborn rabbits, who also take 2 periods to produce k pairs of newborn rabbits ... return the total number of rabbits after n months."

  (let [n n k k myvals [[0 0 1 1]]]
    (loop [month 1 myvals myvals]
      (if (= month n)
        (last (last myvals))
        (do
          (let [r ((myvals (- month 1) )0) g ((myvals (- month 1)) 1) n ((myvals (- month 1)) 2) t ((myvals (- month 1)) 3) ]
            (recur (inc month)  (vec (concat myvals [[ (+ r g) n (* (+ println g) k) (+ (+ r g) n (* (+ r g) k) )]])))))))))

(defn pointMutations

  []

  "Problem 5: HAMM"
  "Reads a file and determines the number of point mutations"
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

"Codon hash table for use in later problems."
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
  :UAA ""   :CAA "Q"      :AAA "K"      :GAA "E"
  :UAG ""   :CAG "Q"      :AAG "K"      :GAG "E"
  :UGU "C"      :CGU "R"      :AGU "S"      :GGU "G"
  :UGC "C"      :CGC "R"      :AGC "S"      :GGC "G"
  :UGA ""   :CGA "R"      :AGA "R"      :GGA "G"
  :UGG "W"      :CGG "R"      :AGG "R"      :GGG "G" })

(defn getCodon
  [triplet]
  "Simple way to query the table to retrive the corresponding Protein letter"

  (codonTable (keyword triplet))
  )

(defn convertToProteins

  [rawBP]

  "Problem 6: PROT"
  "Takes in a raw Base Pair sequence and converts it to the protein sequence."
  (loop [rawBP rawBP protein ""]
    (if (= (count rawBP) 0)
      protein
      (recur (subs rawBP 3) (str protein (getCodon (subs rawBP 0 3))))
      )

    )
  )

"Would love to be able to combine calcGCPercent and calcGCPercentNoNewLines into one function"

(defn calcGCPercent
 
  [text]
  "Calculates the GC percentage of a string assuming it has \newlines in it"

  (float(/ (+ ((frequencies text) \G ) ((frequencies text) \C)) (- (count text) ((frequencies text) \newline)  )))

  )


(defn calcGCPercentNoNewLines
  [text]
  "Calculates the GC percentage of a string assuming it doesn't contain any new lines"

  (float(/ (+ ((frequencies text) \G ) ((frequencies text) \C) ) (count text))))






(defn gcContent
  []
  "Get the GC content of multiple strings of DNA in the Rosalind format. Returnt the sequence ID and percent GC content (decimal format) "

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


