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
      (.setSize cm 1400 (* 55 (count-newlines input)))
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
(comment "slide on complexity, links and resources, macro,functional, presistent data structure immutabillity, concurrency, lisp, jvm, polymorphism")

(defn clojure []
  (let [title "#clojure"
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

(defn model-value []
  (let [title "# Value"
        text (markdown (bullets ["a magnitude, quantity, or number"
                                 "observable"
                                 "comparable"
                                 "values aggregate to value (42, [42])"
                                 "**never** changes"
                                 ]))]
    (simple-slide title text)))

(defn model-state-identity []
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

(defn clojure-model []
  (let [title "# clojure model"
        text (markdown (bullets ["all data structures are immutable and persistent"
                                 "explicit semantics for handling state \n * via Refs and Agents"
                                 "decouples state from identity \n * identity _has_ state \n * identity can be associated with different states at different times \n * but the state _itself_ does not change "
                                 ]))]
    (simple-slide title text)))

(defn clojure-model-2 []
  (let [title "# clojure model #2"
        text (markdown (bullets ["changes to references are coordinated by the system \n * enforced \n * consistent view of the world \n * the time never stops (no locks!)"
                                 "the value of a reference (state of identity) is always \n * visible \n *  shareable"
                                 "imperative way \n * direct access to mutable objects"
                                 "clojure way \n * indirect access to immutable persistent data structures"
                                 ]))]
    (simple-slide title text)))

(defn clojure-mutation []
  (let [title "# clojure mutation"
        text (markdown (bullets ["the only mutable type is Refs"
                                 "mutations are done within a transaction"
                                 "atomic \n * every changppe made within a transaction occurs or none do"
                                 "isolated \n * transaction is not effected by other transaction while running"
                                 "must avoid side effects"
                                 ]))]
    (simple-slide title text)))

(defn mutaion-semantics []
  (let [title "# mutation semantics"
        text (pretty '(defonce app-state (atom {:current 0}))
                     '(defn next! [r coll]
                        (let [cyclic-inc #(mod (inc %) (count coll))]
                          (swap! r update-in [:current] cyclic-inc)))
                     '(defn prev! [r coll]
                        (let [cyclic-dec #(mod (dec %) (count coll))]
                          (swap! r update-in [:current] cyclic-dec))))]
    (code-slide title text)))

(defn persistent-immutable-ds []
  (let [title "# clojure mutation"
        text (markdown (bullets ["the only mutable type is Refs"
                                 "mutations are done within a transaction"
                                 "atomic \n * every change made within a transaction occurs or none do"
                                 "isolated \n * transaction is not effected by other transaction while running"
                                 ]))]
    (simple-slide title text)))



(def slides [clojure
             why-clojure
             why-functional
             functional-programming
             warmup
             warmup-2
             warmup-3
             why-functional-2
             model-value
             model-state-identity
             why-not-oop
             why-not-oop-2
             clojure-model
             clojure-model-2
             clojure-mutation
             mutaion-semantics])

(defn slide []
  (fn []
    (let [i (get @app-state :current)
          current (get slides i)]
      (.log js/console (str "current slide: " i))
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
