(ns hello-world.core
  (:require [reagent.core :as reagent :refer [atom]]
            [markdown-to-hiccup.core :as m]
            [garden.core :refer [css]]
            [clojure.string :as string]
            [cljs.reader :refer [read-string]]
            [cljs.js :refer [empty-state eval js-eval]]
            [goog.events :as events]
            [cljs.core.async :refer [chan dropping-buffer put! <! go]]
            [cljs.pprint :as p])
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:import  [goog.events.EventType]))

(enable-console-print!)

(defonce app-state (atom {:current 0}))

(defn next! [ref coll]
  (let [cyclic-inc #(mod (inc %) (count coll))]
    (swap! ref update-in [:current] cyclic-inc)))

(defn prev! [ref coll]
  (let [cyclic-dec #(mod (dec %) (count coll))]
    (swap! ref update-in [:current] cyclic-dec)))

(defn set-slide! [i]
  (swap! app-state assoc :current i))

(defn markdown [text]
  (->> text
       (m/md->hiccup)
       (m/component)))

(defn bullets [args]
  (loop [args args
         acc ""]
    (let [seperator (if (empty? acc) "* " "\n* ")]
      (if (empty? args)
        acc
        (recur (rest args) (string/join seperator [acc (first args)]))))))

(defn with-style [dom style]
  (let [div [:div {:class (str style)}]]
    (if (= :div (nth dom 0))
      (conj div (nth dom 2))
      (conj div dom))))

(defn empty-slide []
  [:div {:class "slide-container"}])

(defn empty-code-slide []
  [:div {:class "code-container"}])

(defn naked-slide
  ([title body]
   (let [container (empty-slide)
         dom-title (with-style (markdown title) "title")]
     (conj container
           dom-title
           body))))

(defn simple-slide [title body]
  (naked-slide title (with-style body "slide")))

(defn pretty [& args]
  (let [make-pretty #(p/with-pprint-dispatch
                      p/code-dispatch
                       (with-out-str (p/pprint %)))
        joined (string/join "\n" (map make-pretty args))]
    joined))

;-- Code component
(defn count-newlines [a]
  (reagent/track (fn []
                   (count (re-seq #"\n" @a)))))

(defn code-did-mount [input]
  (fn [this]
    (let [count-newlines #(count (re-seq #"\n" %))
          cm (.fromTextArea  js/CodeMirror
                             (reagent/dom-node this)
                             #js {:mode "clojure"
                                  :lineNumbers true})]
      (.setSize cm 1400 (* 60 (count-newlines input)))
      (.on cm "change" #(reset! input (.getValue %))))))

(defn edit-card [initial]
  (reagent/with-let [content (atom initial)
                     counter (count-newlines content)]
    [:textarea
     {:rows (+ 102 @counter)
      :on-change #(do
                    (reset! content (.. % -target -value)))
      :value initial}]))

(defn code-ui [input]
  (reagent/create-class
   {:render (fn []
              [edit-card input])
    :component-did-mount (code-did-mount input)}))

(defn code-slide
  ([title]
   (code-slide title nil))
  ([title initial]
   (let [container (empty-code-slide)
         title (with-style (markdown title) "title")
         code (code-ui initial)
         body (with-style [code] "code-slide")
         slide (conj container
                     title
                     body)]
     slide)))

;; Slides

(defn vector-of-content []
  (let [title "# vector of content"
        text (pretty '(def slides [on-clojure
                                   vector-of-content
                                   why-clojure
                                   on-state-and-complexity
                                   why-functional
                                   functional-programming
                                   warmup
                                   warmup-2
                                   warmup-3
                                   why-functional-2
                                   clj-value-model
                                   clj-state-and-identity-model
                                   why-not-oop
                                   why-not-oop-2
                                   clojure-working-model
                                   clojure-working-model-2
                                   the-m-word
                                   the-semantics-of-mutation
                                   persistency-and-immutability
                                   the-magic-of-macros
                                   macro-example
                                   macro-example-2
                                   macro-example-3
                                   runtime-polymorphism
                                   multimethod-polymorphism
                                   protocol-polymorphism
                                   questions?]))]
    (code-slide title text)))

(defn on-clojure []
  (let [title "# clojure"
        text (markdown (bullets ["modern Lisp dialect, on the JVM"
                                 "immutable persistent data structures"
                                 "built-in support in concurrency"
                                 "created by Rich Hickey"
                                 ]))]
    (simple-slide title text)))

(defn why-clojure []
  (let [title "#why clojure?"
        text (markdown (bullets ["functional"
                                 "code as data (as code)"
                                 "enables high level of abstraction"
                                 "distinctive approuch to State, Identity and Time"
                                 "(almost) no syntax"
                                 ]))]
    (simple-slide title text)))

(defn on-state-and-complexity []
  (let [title "# state and complexity"
        text (markdown (bullets ["Anyone who has ever telephoned a support desk for a software system and been told to “try it again”, or “reload the document”, or “restart the program”, or “reboot your computer” or “re-install the program” or even “re- install the operating system and then the program” has direct experience of the problems that state causes for writing reliable, understandable software - _Out of the Tar Pit, Moseley and Marks_"
                                 "computers have very large numbers of states. This makes conceiving, describing, and testing them hard. Software systems have orders-of-magnitude more states than computers do - _Brooks_"]))]
    (simple-slide title text))  )

(defn why-functional []
  (let [title "#why functional?"
        text (markdown (bullets ["program as a chain of transformations on values"
                                 "easier to reason about"
                                 "easier to understand"
                                 "less mechanical, more mathematical"
                                 "composable \n * _Design is to take things apart in such a way that they can be put back together_ - R.H"
                                 ]))]
    (simple-slide title text)))


(defn functional-programming []
  (let [title "#functional  programming"
        text (markdown (bullets ["rooted in the theoretical framework of λ-calculus"
                                 "alternative to the imperative approuch of von-Neumann architecture (OOP)"
                                 "can simulate any stateful Turing machine"
                                 "building blocks \n * functions (preferably pure) \n * (mostly) immutable data"]))]
    (simple-slide title text)))



(defn warmup []
  (let [title "#(warmup)"
        text (pretty '(let [a-str "Bothers and Sisters"
                            a-keyword :first-release
                            a-number  20090504
                            a-vec  ["used extensively", 123, ["nested"]]
                            a-list  ("are you list?", true, false)
                            a-set {"Heed", 3.1}
                            a-func (fn [x y]
                                      (if (<= x y)
                                        y
                                        x))
                            a-map {:key "value",
                                    "key" :value}])
                     )]
    (code-slide title text)))

(defn warmup-2 []
  (let [title "#(warmup 2)"
        text (pretty '(let [crts [{:type "dog" :human-friendly [100, 1000]}
                                  {:type "cat" :human-friendly [-32, 9]}
                                  {:type "homosapien" :human-friendly [-1000, 5.7]}]
                            friendliness (fn [[mi ma]] (+ ma mi))
                            cosmological-const 42
                            friendly? (fn [crt] (<= cosmological-const
                                                    (friendliness (:human-friendly crt))))]
                        (->> crts
                             (filter friendly?)
                             (map (fn [crt] (crt :type)))))
                     )]
    (code-slide title text)))

(defn warmup-3 []
  (let [title "#(warmup 3)"
        text (pretty '(((fn [f]
                          (f f))
                        (fn [f]
                          (fn [v]
                            (if (= 1 (count v))
                              (first v)
                              (max (first v) ((f f) (rest v)))))))
                       [12 3 93 8 1 0 -1 2018 4.4])
                     )]
    (code-slide title text)))

(defn why-functional-2 []
  (let [title "#why functional? #2"
        text (markdown (bullets ["given a set of arguments, the program will always return exactly the same result \n * pure functions + immutable data - side-effects = referential transparency"
                                 "testable \n * function acts locally"
                                 "in concurrent environment - non-issue"
                                 ]))]
    (simple-slide title text)))

(defn clj-value-model []
  (let [title "# Value"
        text (markdown (bullets ["a magnitude, quantity, or number"
                                 "observable"
                                 "comparable"
                                 "values aggregate to value (42, [42], {:a 42, :b [42ד]})"
                                 "**never** changes"
                                 ]))]
    (simple-slide title text)))

(defn clj-state-and-identity-model []
  (let [title "# Identity and State"
        text (markdown (bullets ["Identity \n * a logical entity associated with a series of different values over time"
                                 "State \n *  a value associated with some identity at a point in time"
                                 ]))]
    (simple-slide title text)))

(defn why-not-oop []
  (let [title "# why not OOP?"
        text (markdown (bullets ["couples state with identity \n * an object is a pointer to a memory that contains its state"
                                 "couples state with behavior \n * an object is also a set of methods that manipulate its state"
                                 "couples read with write \n * impossible to observe an object without blocking others from changing it \n * _The hidden premise of imperative program: the world is stopped while you look at or change it_ - R.H"
                                 ]))]
    (simple-slide title text)))

(defn why-not-oop-2 []
  (let [title "# why not OOP? #2"
        text (markdown (bullets ["most objects are just fancy maps \n * but with personal language (.getFighterId())"
                                 "over-specified \n * new thing - new class"
                                 "no reuse"
                                 "explosion of code"
                                 "synchronization nightmare"
                                 "power corrupts"
                                 ]))]
    (simple-slide title text)))

(defn clojure-working-model []
  (let [title "# clojure working model"
        text (markdown (bullets ["all data structures are immutable"
                                 "explicit semantics for handling state \n * via Refs and Agents"
                                 "decouples state from identity \n * identity _has_ state \n * identity can be associated with different states at different times \n * but the state _itself_ does not change "
                                 ]))]
    (simple-slide title text)))

(defn clojure-working-model-2 []
  (let [title "# clojure model #2"
        text (markdown (bullets ["changes to references are coordinated by the system \n * enforced \n * consistent view of the world \n * the time never stops (no locks!)"
                                 "the value of a reference (state of identity) is always \n * visible \n *  shareable"
                                 "imperative way \n * direct access to mutable objects"
                                 "clojure way \n * indirect access to immutable persistent data structures"
                                 ]))]
    (simple-slide title text)))

(defn the-m-word []
  (let [title "# the m word"
        text (markdown (bullets ["the only mutable type is Refs"
                                 "mutations are done within a transaction"
                                 "atomic \n * every change made within a transaction occurs or none do"
                                 "isolated \n * transaction is not effected by other transaction while running"
                                 "must avoid side effects"
                                 ]))]
    (simple-slide title text)))

(defn the-semantics-of-mutation []
  (let [title "# the semantics of mutation"
        text (pretty '(defonce app-state (atom {:current 0}))
                     '(defn next! [r coll]
                        (let [cyclic-inc #(mod (inc %) (count coll))]
                          (swap! r update-in [:current] cyclic-inc)))
                     '(defn prev! [r coll]
                        (let [cyclic-dec #(mod (dec %) (count coll))]
                          (swap! r update-in [:current] cyclic-dec)))
                     '(defn slide []
                        (fn []
                          (let [i (get @app-state :current)
                                current (get slides i)]
                            [current]))))]
    (code-slide title text)))

(defn persistency-and-immutability []
  (let [title "# (), #{}, {}, []"
        text (markdown (bullets [
                                 "immutability \n * when change occurs, new version of the data structure is created"
                                 "persistency \n * old version and newer version share some structure"
                                 "structural sharing \n * maintains performance for collections operations (Big O) \n * not expensive \n * safe for threads and iteration"
                                 ]))]
    (simple-slide title text)))

(defn the-magic-of-macros []
  (let [title "# defmacro"
        text (markdown (bullets ["macros are functions called in compile time to perform transformations of code"
                                 "clojure extends the lispy notion of code-as-data to maps and vectors"
                                 "Use cases \n * syntactic abstraction (DSL) \n * control flow \n * reduce boilerplate \n * create binding forms (let) \n * . \n * .. \n * ... \n * the power is in your hands"
                                 ]))]
    (simple-slide title text)))


(defn macro-example []
  (let [title "# macro example"
        text "(defmacro and
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))"]
    (code-slide title text)))

(defn macro-example-1 []
  (let [title "# macro example #1"
        text "(defmacro safe [body]
  `(try ~body
        (catch Exception e#
          (str \"caught exception: \" (.getMessage e#)))))

(safe (/ 4 2))

(safe (/ 1 0))

(safe (+ 1 \"some string\"))"]
    (code-slide title text)))

(defn macro-example-2 []
  (let [title "# macro example #2"
        text "(defmacro def-watched [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                :re-bind
                (fn [~'key ~'r old# new#]
                  (println old# \" -> \" new#)))))
(def-watched x 2)

(def x 3)"]
    (code-slide title text)))

(defn macro-example-3 []
  (let [title "# and many more"
        text (markdown (bullets ["Branching \n *  and, or, when, when-not, when-let, when-first, if-not, if-let, cond, condp"
                                 "Looping \n * for, doseq, dotimes, while"
                                 "Dynamic scopes \n *  binding, locking, time, with-open"
                                 ]))]
    (simple-slide title text)))

(defn runtime-polymorphism []
  (let [title "# polymorphism in clojure"
        text (markdown (bullets ["polymorphism is the ability of a function to have different definitions depending on the type of the target object"
                                 "clojure way \n * single function designator dispatches to multiple independently-defined functions based upon some value of the call \n * using protocols to define function signature"
                                 ]))]
    (simple-slide title text)))

(defn polymorphism-via-multimethod []
  (let [title "# defmulti"
        text (pretty '(defmulti handle-op (fn [data]
                                           (:op data)))

                     '(defmethod handle-op :update-aep [{:keys [args]}])

                     '(defmethod handle-op :update-audio [{:keys [args]}])

                     '(defmethod handle-op :update-metadata [{:keys [args]}]))]
    (code-slide title text)))

(defn polymorphism-via-protocol []
  (let [title "# defprotocol (deftype, defrecord)"
        text (pretty '(defprotocol Concatenatable
                        (cat [this other]))

                     '(extend-type String
                        Concatenatable
                        (cat [this other]
                          (.concat this other)))
                     '(cat "House of" "Cards"))]
    (code-slide title text)))


(defn questions? []
  (let [title "# .clj(s)"
        text (markdown (bullets ["[https://clojure.org/index](https://clojure.org/index)"
                                 "Rich Hickey on Youtube\n * Clojure Made Simple \n * The Value of Values \n * Clojure Concurrency \n * and all the others"
                                 "Books \n * The Joy of Clojure \n * Clojure for the Brave and True \n * [Out of the Tar Pit](http://curtclifton.net/papers/MoseleyMarks06a.pdf)"
                                 "Thanks for listening!"]))]
    (simple-slide title text)))

(def slides [on-clojure
             vector-of-content
             why-clojure
             on-state-and-complexity
             why-functional
             functional-programming
             warmup
             warmup-2
             warmup-3
             why-functional-2
             clj-value-model
             clj-state-and-identity-model
             why-not-oop
             why-not-oop-2
             clojure-working-model
             clojure-working-model-2
             the-m-word
             the-semantics-of-mutation
             persistency-and-immutability
             the-magic-of-macros
             macro-example
             macro-example-1
             macro-example-2
             macro-example-3
             runtime-polymorphism
             polymorphism-via-multimethod
             polymorphism-via-protocol
             questions?])


(defn slide []
  (fn []
    (let [i (get @app-state :current)
          current (get slides i)]
      [current])))

(defn main-container []
  [:div {:class "slideshow-container"}
   [slide]
  [:div {:class "dot-container"}
   (for [i (range (count slides))]
     [:span {:class "dot"
             :key (str i)
             :onClick #(set-slide! i)}])]])


(reagent/render-component [main-container]
                          (. js/document (getElementById "app")))
;; navigation support

(def keyboard-events
    (.-KEYDOWN events/EventType))

(def event-source
  (.-body js/document))


(defn extract-key [evt]
  (.-keyCode evt))

(defn listen-to-keyboard []
  (let [event-ch (chan (dropping-buffer 1))]
    (events/listen event-source
                   keyboard-events
                   #(put! event-ch (extract-key %)))
    event-ch))

(let [input (listen-to-keyboard)]
  (go-loop []
    (let [key (<! input)
          right 39
          left 37]
      (cond (= key right) (next! app-state slides)
            (= key left) (prev! app-state slides)))
    (recur)))

(defn on-js-reload [])
