(ns hs-robotwar.core)

(use '[clojure.set :only (union)])
(use '[clojure.string :only (split join)])

(def operators #{ "-" "+" "*" "/" "=" "#" "<" ">"})

(def registers #{ "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
                  "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                  "AIM" "SHOT" "RADAR" "DAMAGE" "SPEEDX" "SPEEDY" "RANDOM" "INDEX"})

(def commands (union operators 
                     #{"TO" "IF" "GOTO" "GOSUB" "ENDSUB"}))

; (largely stolen from re-seq source)
(defn re-seq-with-pos
  "Returns a lazy sequence of successive matches of pattern in string with position"
  [^java.util.regex.Pattern re s]
  (let [m (re-matcher re s)]
    ((fn step []
       (when (.find m)
         (cons [(re-groups m) (.start m)] (lazy-seq (step))))))))

(def lex-re (let [opstring (join operators)]
              (re-pattern (str "[" opstring "]|[^" opstring "\\s]+"))))

(defn strip-comments
  [line]
  (re-find #"[^;]*" line))

(defn lex-line
  [line]
  (map (fn [[s n]]  {:token-str s, :pos n})
       (re-seq-with-pos lex-re line)))

(defn lex
  [lines]
  (mapcat lex-line (split lines #"\n")))

(defn merge-lexed-tokens
  "helper function for conjoining minus signs to next token
  if they turn out to be unary negative signs"
  [first-token second-token]
  {:token-str (str (:token-str first-token) (:token-str second-token))
   :pos (:pos first-token)})

(defn str->int
  "Like Integer/parseInt, but returns nil on failure"
  [s]
  (and (re-matches #"-?\d+" s)
       (Integer/parseInt s)))

(defn valid-word
   "Capital letters and numbers, starting with a capital letter"
  [s]
  (re-matches #"[A-Z]+\d*" s))

(def return-err (constantly "Invalid word or symbol"))

(defn parse-token
  "takes a single token and adds the appropriate metadata"
  [{token-str :token-str, pos :pos}]
  (some
    (fn [[parser token-type]]
      (when-let [token-val (parser token-str)]
        {:val token-val, :type token-type, :pos pos}))
    [[registers  :register]
     [commands   :command]
     [str->int   :number]
     [valid-word :label]
     [return-err :error]]))

(def value-type? #{:number :register})

(defn parse
  "take the tokens and convert them to structured source code ready for compiling"
  [initial-tokens]
  (loop [parsed []
         [{token-str :token-str :as token} & tail :as tokens] initial-tokens]
    (let [previous-parsed-token (last parsed)]
      (cond
        (or (empty? tokens) (= (:type previous-parsed-token) :error))
          parsed
        ; deal with unary negative signs
        (and (= token-str "-") (not-empty tail) (not (value-type? (:type previous-parsed-token)))) 
          (recur parsed (cons (merge-lexed-tokens token (first tail)) (rest tail)))
        :otherwise
          (recur (conj parsed (parse-token token)) tail)))))


(defn pretty-print-tokens [token-seq]
  (join 
    "\n"
    (map #(format "%2d %s %s" (:pos %) (:type %) (:val %)) 
         token-seq)))

(defn evaluate [token-seq]
  (println (pretty-print-tokens token-seq)))


(defn repl
  "make it so"
  []
  (loop [input (read-line)]
    (when (not= input "exit")
      (-> input
          strip-comments
          lex
          parse
          evaluate)
      (recur (read-line)))))


