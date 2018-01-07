(ns .lexer)
; *********************************************
; *  341 Programming Languages                *
; *  Fall 2017                                *
; *  Student: Burak Furkan Akşahin            *
; *********************************************

;; utility functions
(use 'clojure.java.io)
(use '[clojure.string :only (split triml)])

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***)(



(defn wtL [word]
  "Just a fancy function for translating word to list of characters"
  (seq word)
  )

(defn ltL [doc]
  "Takes a document and return list of words which list of characters"
  (loop [x 0 n (count doc) result ()]




    (if (== x n)
      (reverse result)
      (recur (inc x) n (conj result (wtL (nth doc x))))
      )
    )
  )

(defn dtL
  [lines]
  "Takes list of lines and make these lines list of words and these words to list of characters"
  (loop [x 0 n (count lines) result ()]
    (if (== x n)
      (reverse result)
      (recur (inc x) n (conj result (ltL (seq (clojure.string/split (nth lines x) #" ")))))
      )
    )

  )

(defn readDocument
  [filename]
  "Reads document and returns list of lines which list of words which list of characters"
  (let [doc (slurp (clojure.java.io/resource filename))]
    (println "Reading document " filename)
    (def whole (clojure.string/split-lines doc ))
    (def lines (wtL whole))
    (dtL lines))
  )

(defn parse-int [v]
  (try
    (Integer/parseInt (re-find #"^\d+" (.toString v)))
    (catch NumberFormatException e "a")))

(defn isId
  [word]
  (def alphabeth #{"a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                   "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"})
  (def bools #{"true" "false"})
  (def index 1)
  (def size (count word))
  (def returnValue 0)
  (while (< index size)
    (def check (contains? alphabeth (str (nth word index))))
    (cond
      (and (= returnValue 0) (= check true)) (def returnValue 0)
      (and (= returnValue 0) (= check false)) (def returnValue -1)
      :else (def returnValue -1)
    )
    (def index (inc index))
  )
  (if (contains? bools word)
    2
    returnValue
  )
)

(defn isInteger
  [word]
  (def intValue (parse-int (nth word 0)))
  (def index 1)
  (def size (count word))
  (def returnValue 1)
  (if (and (= intValue 0) (> size 1))
    (def returnValue -1)
  )
  (while (< index size)
    (def check (parse-int (nth word index)))
    (cond
      (and (= returnValue 1) (integer? check)) (def returnValue 1)
      (and (= returnValue 1) (not (integer? check) )) (def returnValue -1)
      :else (def returnValue -1)
      )
    (def index (inc index))
  )
  returnValue
)

(defn terminals
  [word]
  (def alphabeth #{"a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                   "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"})

  (def idOrValue -1)

  (cond
    (contains? alphabeth (str (nth word 0)))   (def idOrValue (isId word))
    (integer? (parse-int (nth word 0))) (def idOrValue (isInteger word))
    (= (str (nth word 0)) "-") (def idOrValue (isInteger (subs word 1)))
    :else (def idOrValue -1)
  )
  (cond
    (== idOrValue 0) (list "ID" word)
    (== idOrValue 1) (list "INT_VALUE" word)
    (== idOrValue 2) (list "BOOL_VALUE" word)
    :else (list "ERROR!!!" word)
  )
)

(defn tokenizeOther
  [word]
  (def returnList nil)
  (def newWord (clojure.string/join word))
  (def newWord (.replaceAll newWord "\\(" ""))
  (def newWord (.replaceAll newWord "\\)" ""))
  (def keywords #{"and" "or" "not" "equal"  "append" "concat" "set" "deffun" "for" "while" "if" "then" "else"})
  (def operators #{"+" "-" "/" "*"})
  (cond
    (contains? keywords newWord) (def returnList (list (str "KEY_" newWord) newWord))
    (contains? operators newWord) (def returnList (list (str "OPER_" newWord) newWord))
    :else  (def returnList (terminals newWord))
  )
  returnList
)
(defn tokenizer
  [word]
  (def tokenList nil)
  (def parantesis nil)
  (if (= word nil)
    (def tokenList (conj tokenList (list  "INDENT" " ")))
  )
  (def charIndex 0)
  (def numberOfChars (count word))
  (def cont 0)

  (while (< charIndex numberOfChars)
      (cond
        (= (nth word charIndex) \( ) (def tokenList (conj tokenList (list "L_PAREN" "("))  )
        (= (nth word charIndex) \) ) (def tokenList (conj tokenList (list "R_PAREN" ")"))  )
        :else (def cont (inc cont))
      )
      (if (or (= (nth word charIndex) \( )(= (nth word charIndex) \) ))
          (def cont 0)
        )
      (if (= cont 1)
        (def tokenList (conj tokenList (tokenizeOther word)))
      )
      (def charIndex (inc charIndex))
  )
  (reverse tokenList)
)

(defn lexer
  [filename]
  (let [doc (readDocument filename)]
    (def tokenL nil)
    (def currentLineIndex 0)
    (def numberOfLines (count doc))
    (while (< currentLineIndex numberOfLines)
      (def currentLine (nth doc currentLineIndex))
      ;;(println currentLine)
      (def currentIndex 0)
      (def numberOfWords (count currentLine))
      (def tokenPosition 0)
      (while (< currentIndex numberOfWords)
        (def currentWord (nth currentLine currentIndex))
        (def token (tokenizer currentWord))
        (def tokenIndex 0)
        (def numberOfTokens (count token))
        (while (< tokenIndex numberOfTokens)
          (def tokenL (conj tokenL (list  (nth token tokenIndex) currentLineIndex tokenPosition)))
          (def tokenPosition (inc tokenPosition))
          (def tokenIndex (inc tokenIndex))
        )
        (def currentIndex (inc currentIndex))
      )
      (def currentLineIndex (inc currentLineIndex))
    )
    (reverse tokenL)
  )
)




(defn print-tokens
  [tokens]
  (def i 0)
  (def j (count tokens))
  (while (< i j)
    (println "TOKEN TYPE=" (first (first (nth tokens i))) ",  LINE=" (nth (nth tokens i) 1)
             ", TOKENPOSITION=" (nth (nth tokens i)2) ",  TEXT=" (second (first (nth tokens i))))
    (def i (inc i))
  )
)

;; -----------------------------------------------------
;; Test code...

(defn test_on_test_data
  []
  (let [tokens (lexer "CoffeeSample.coffee")]
    (print-tokens tokens)
  )

)


;; test code...
(test_on_test_data)

