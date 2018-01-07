; *********************************************
; *  341 Programming Languages                *
; *  Fall 2017                                *
; *  Author: Yakup Genc
; *  Student: Burak Furkan Akşahin            *
; *********************************************

;; utility functions 
(load-file "include.clj") ;; "c2i and "i2c"
(use 'clojure.java.io)
(use '[clojure.string :only (split triml)])

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***)(

(defn fact [x]
	"Never used"
	(loop [n x f 1]
		(if (= n 1)
			f
			(recur (dec n) (* f n)))))

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

(defn linearSearch
	[word dic]
	"Linearsearch in given sorted dic. Returns how many elements exist in this dictionary"
	(def counter 0)
	(loop [x 0]
		(when (< x (count dic))
			(if (= word (apply str (nth dic x)))
				(def counter (inc counter))
				(def counter (+ counter 0))
			)
			(recur (inc x))
		)
	)
	counter
)

(defn binarySearch [word dic]
	"Binary search in given sorted dictionary, returns index of element or -1"
	(loop [lower 0
				 upper (dec (count dic))]

		(if (> lower upper) -1
												(let [mid (quot (+ lower upper) 2)
															midvalue (apply str (nth dic mid)) ]
													(cond
														(> (compare midvalue word) 0) (recur lower (dec mid))
														(< (compare midvalue word) 0) (recur (inc mid) upper)
														(= (compare midvalue word) 0) mid)))))

(defn findFrequencys
	[doc]
	"Find frequencies of letters in the given doc. and returns list of letter-freq entries"
	(def freqs nil)
	(def characters '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
										 "k" "l" "m" "n" "o" "p" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
	(def x 0)
	(def n (count characters))
	(while (< x n)
		(do
			(if (= (first(select-keys (frequencies (apply str doc)) (nth characters x))) nil)
				()
				(def freqs (conj freqs (select-keys (frequencies (apply str doc)) (nth characters x))))
				)

			(def x (inc x))
			)
		)
	 (reverse freqs)
)

(defn findCharacters
	"Find letters from freq list created by findFrequencies and returns list of letters"
	[freqs]
	(def x 0)
	(def n (count freqs))
	(def temp nil)
	(while (< x n)
		(do
			(def temp (conj temp (nth (str (nth freqs x)) 2)))
			(def x (inc x))
			)
		)
	(reverse temp)
)

"Next two functions are from web"
(defn rotations [a-seq]
	(let [a-vec (vec a-seq)]
		(for [i (range (count a-vec))]
			(concat (subvec a-vec i) (subvec a-vec 0 i)))))

(defn permutations [a-set]
	(if (empty? a-set)
		(list ())
		(apply concat (map (fn [x] (map cons (repeat (first x))
																		(permutations (rest x))))
											 (rotations a-set)))))

(defn encoder
	"Encode a document with only one line and creates a new file with name encoded_filename
	This encoding procedure is different from homework.pdf Encoding will be handled like that:
	Line: hello this is a test, for encoding only the existing letters can be used for each other.
	So encoded line can be: atool iahe he s itei,only using existing letters "
	[file]
	(def text (slurp file))
	(def encodedFile (str "encoded_" file))

	(def doc (readDocument file ))
	(def characters (findCharacters (findFrequencys (first doc))))
	(def mappings (permutations characters))
	(def randomMapping (nth mappings (rand-int (count mappings))))
	(println "Encoding file " file " -> " encodedFile)
	(def x 0)
	(def n (count text))
	(def encodedText nil)
	(while (< x n)
		(do
			(if (< -1 (.indexOf characters (nth text x)))
				(def encodedText (str encodedText (nth randomMapping (.indexOf characters (nth text x)))))
				(def encodedText (str encodedText (nth text x)))
			)
			(def x (inc x))
		)
	)
	(println "This message: " text)
	(println "Encoded to this: " encodedText)
	(spit encodedFile	encodedText)
)

(defn read-as-list
	"Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
	[filename]
	; Implement this function... ;
	(println "Reading dictionary" filename)
	(with-open [r (reader filename)]
		(def doc (doall (line-seq r)))
		(if (<(count doc) 1500)
			(ltL doc )
			(ltL doc)
			)
		)
	)

(defn spell-checker-0
	"Uses linear search for spell checking"
	[word,dic]
	(>= (linearSearch (apply str word) dic) 0)
)

(defn spell-checker-1
	"Uses binarySearch for spell checker"
	[word,dic]
	(>= (binarySearch (apply str word) dic) 0)
)



;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defn Gen-Decoder-A
	"This decoder only words with one line documents or the file must be encoded line by line
	so it can decode line by line correctly. The line must contains max 8-9 different letters
	Or it will crash :)
	And please read the comment in the encoder function"
	[paragraph]
	(let [dic (read-as-list "dictionary3.txt")]
		(def freqs (findFrequencys paragraph))
		(def characters (findCharacters freqs))
		(def mappings (permutations characters))
		(println "Decoding..")
		(println "Total possible permumations: " (count mappings))
		(def x 0)
		(def i 0)
		(def n (count mappings))
		(def j (count paragraph))
		(def trueWords 0)
		(def best (list (int 0) (int 0)))
		(def encoded nil)
		(def encodedParagraph nil)
		(while (< x n)
			(do
				(println "Trying permutation :" x "/" n)
				(def i 0)
				(def trueWords 0)
				(def encoded nil)
				(while (< i j)
					(do
						(def currentWord (nth paragraph i))
						(def currentMapping (nth mappings x))
						(def tryWord nil)
						(def k 0)
						(def l (count currentWord))
						(while (< k l)
							(do
								(def tryWord (conj tryWord (nth currentMapping (.indexOf characters (nth currentWord k)))) )
								(def k (inc k))
							)
						)

						(def foundWord (reverse tryWord))
						(if (spell-checker-1 (apply str foundWord) dic)
							(do
								(def trueWords (inc trueWords))
								(def encoded (conj encoded foundWord))
								)
							()
						)

						(def i (inc i)))
				)

				(if (< (first best) trueWords)
					(do
						(def best (list trueWords x) )
						(def encodedParagraph (reverse encoded))
						(if (= trueWords (count paragraph))
							(def x (dec n))
							()
						)
					)
					()
				)
				(def x (inc x))
			)

		)

		(println "Max Found:" (first best) "MappingIndex" (second best))
		(def correctMapping (nth mappings (second best)))
		encodedParagraph
	)
)

(defn Gen-Decoder-B-0
	[paragraph]
  	;you should implement this function
)

(defn Gen-Decoder-B-1
	[paragraph]
  	;you should implement this function
)

(defn Code-Breaker
	[document decoder]
	(println "Decoded message: " document)
	(println "Encoded to this: " decoder)
)


;; -----------------------------------------------------
;; Test code...

(defn test_on_test_data
	[]
	(let [doc (readDocument "encoded_encoded.txt")]
		;;(println (time (spell-checker-0 (first (first doc))  dic)))
		;;(println (time (spell-checker-1 (first (first doc))  dic)))
		(Code-Breaker doc (Gen-Decoder-A (first doc)))
		)

)


;; test code...
(test_on_test_data)

