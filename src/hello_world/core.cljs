(ns hello-world.core
  (:require [reagent.core :as reagent :refer [atom]]
            [markdown-to-hiccup.core :as m]
            [garden.core :refer [css]]
            [clojure.string :as string]
            [cljs.reader :refer [read-string]]
            [cljs.js :refer [empty-state eval js-eval]]
            [goog.events :as events]
            [cljs.core.async :refer [chan dropping-buffer put! <! go]]
            [cljs.pprint :refer [pprint]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:import  [goog.events.EventType]))

(enable-console-print!)

(def css-path (str "/Users/talwanich/clojurescript/hello-world/resources/public/css/style.css"))

(def styles
  (let [slideshow-container (css [:.slideshow-container {
                                                       :postition "relative"
                                                       :background "#f1f1f1f1"}])
        slide (css [:.slide { :padding "80px"
                             :text-align "center"}])
        next-prev (css [:.prev :.next {:cursor "pointer"
                                       :position "absolute"
                                       :top "50%"
                                       :width "auto"
                                       :margin-top "-30px"
                                       :padding "16px"
                                       :color "#888"
                                       :font-weight "bold"
                                       :font-size "20px"
                                       :border-radius "0 3px 3px 0"
                                       :user-select "none"}])
        next-position (css [:.next {:position "absolute"
                                    :right 0
                                    :border-radius "3px 0 0 3px"}])
        on-hover (css [:.prev:hover :.next:hover
                       {:background-color "rgba(0,0,0,0.8)"
                        :color "white"}])
        dot-container (css [:.dot-container {:text-align "center"
                                             :padding "20px"
                                             :background "#ddd"}])
        dot (css [:.dot {:cursor "pointer"
                         :height "15px"
                         :width "15px"
                         :margin "0 2px"
                         :background-color "#bbb"
                         :broder-radius "50%"
                         :display "inline-block"
                         :transition "background-color 0.6s ease"}])
        active-dot (css [:.active :.dot:hover {:background-color "#717171"}])
        quote (css [:.q {:font-style "italic"}])
        author (css [:.author {:color "cornflowerblue"}])]
    (string/join [slideshow-container
                  slide
                  next-prev
                  next-position
                  on-hover
                  dot-container
                  dot
                  active-dot
                  quote
                  author])))


(defonce app-state (atom {:current 0}))

(comment "macro,functional, presistent data structure immutabillity, concurrency, lisp, jvm, polymorphism")


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

(defn markdown-code [code]
  (let []
    (->> code
         (markdown))))

(defn code-block [code]
  [:pre {}
   [:code {:class "language-clojure"
           :data-lang "clojure"}
    (prn-str code)]])

(defn with-style [dom style]
  (let [div [:div {:class (str style)}]]
    (if (= :div (nth dom 0))
      (conj div (nth dom 2))
      (conj div dom))))

(defn empty-slide []
  [:div {:class "slide-container"}])

(defn naked-slide [title body]
  (let [container (empty-slide)
        dom-title (with-style (markdown title) "title")]
    (conj container
          dom-title
          body)))

(defn simple-slide [title body]
  (naked-slide title (with-style body "slide")))

(defn next! [atom coll]
  (let [cyclic-inc #(mod (inc %) (count coll))]
    (println "next! " atom ", " coll " slides: ", (count coll))
    (swap! atom update-in [:current] cyclic-inc)))

(defn prev! [atom coll]
  (let [cyclic-dec #(mod (dec %) (count coll))]
    (println "prev! "  atom ", " coll, " slides: ", (count coll))
    (swap! atom update-in [:current] cyclic-dec)))

(defn set-slide! [i]
  (swap! app-state assoc :current i))

(defn pretty [s]
  (with-out-str (pprint s)))
; ------ Intro ------------

(defn intro []
  (let [title "#clojure"
        text (markdown (bullets ["modern Lisp dialect, on the JVM"
                                 "immutable persistent data structures"
                                 "built-in support concurrency (no locks)"
                                 ]))]
    (simple-slide title text)))

;---- lisp  -----
; macro, functional style, high order, loops, code as data, lists, Dynamic polymorphism


(defn lisp []
  (let [title "#clojure as Lisp"
        text (markdown (bullets ["functional"
                                 "code as data (as code)"
                                 "dynamically typed"
                                 "(almost) no syntax"
                                 ]))]
    (simple-slide title text)))

(defn count-newlines [a]
  (reagent/track (fn []
             (count (re-seq #"\n" @a)))))

(defn edit-card [initial]
  (reagent/with-let [content (atom initial)
                     counter (count-newlines content)]
    (.log js/console @content)
    [:textarea
     {:rows (+ 3 @counter)
      :class "codesnapshot"
      :style {:resize "none"
              :width "90%"
              :display "block"
              :overflow "auto"}
      :value initial
      :on-change #(do
                    (reset! content (.. % -target -value)))}]))


(defn code-did-mount [input]
  (fn [this]
    (let [cm (.fromTextArea  js/CodeMirror
                             (reagent/dom-node this)
                             #js {:mode "clojure"
                                  :lineNumbers true})]
      (.on cm "change" #(reset! input (.getValue %))))))


(defn code-ui [input]
  (reagent/create-class
   {:render (fn []
              [edit-card input])
    :component-did-mount (code-did-mount input)}))

(defn code-slide
  ([title]
   (code-slide title nil))
  ([title initial]
   (let [code (code-ui initial)
         body [code]]
     (naked-slide title
                   body))))

(defn lisp1 []
  (let [title "#clojure as Lisp - syntax"
        text (pretty '{:string "Hello people" :integer 123
                    :keyboard :keyboar
                    :symbol (quote "symbol")
                    :vector [1989 "great" :year]
                    :list  (1 2 3 5 8)
                    :set {"Heed", 2.0}
                    :map {:key "value"}})]
    (code-slide title text)))
; ------ Examples --------

(defn read [s]
  (let [ss s]
  (read-string ss)))


(defn eval-expr [s]
  (let [res (eval (empty-state)
                  (read s)
                  {:eval       js-eval
                   :source-map true
                   :context    :expr}
                  identity)]
    (println "eval-raw:  " s)
    (println "eval: " res)
    res))

(defn render-code [this]
  (->> this reagent/dom-node (.highlightBlock js/hljs)))

(defn result-ui [output]
  (reagent/create-class
   {:render (fn []
              [:div {:class "result"}
               [:pre>code.clj
                (if-let [output (get @output :value)]
                  (prn-str output)
                  "")]])
    :component-did-mouth render-code}))

(defn editor-did-mount [input]
  (fn [this]
    (let [cm (.fromTextArea  js/CodeMirror
                             (reagent/dom-node this)
                             #js {:mode "clojure"
                                  :lineNumbers true})]
      (.on cm "change" #(reset! input (.getValue %))))))


(defn editor-ui
  ([input]
   (editor-ui input ""))
  ([input initial]
   (reagent/create-class
   {:render (fn []
              [:textarea
               {:value initial
                :auto-complete "off"
                :class "codesnapshot"
                :on-change #(reset! input (.getValue %))}])
    :component-did-mount (editor-did-mount input)})))


(defn repl
  ([]
   (repl "#repl" nil))
  ([title initial]
   (let [input (atom initial)
         output (atom nil)
         editor (editor-ui input initial)
         eval-btn (fn []
                    [:button {:class "eval"
                              :on-click #(reset! output (eval-expr @input))}])
         result (result-ui output)
         columns (fn [& args]
                   (for [[key, arg] (map-indexed vector args)]
                     [:td {:key key}
                      [arg]]))
         body [:table
               [:tbody
                [:tr
                 (columns editor eval-btn result)]]]]
     (simple-slide title
                   body))))




(def slides [intro, lisp, lisp1, #(code-slide "#clojure as Lisp" (prn-str '(->> [{:type "angry dog", :human-friendly 10},
                                                                     {:type "angry hippopotamus", :human-friendly 4},
                                                                     {:type "angry human", :human-friendly -1}]
                                                                    (filter (fn [crt] (< (:human-friendly crt) 5)))
                                                                    (map (fn [crt] (:type crt))))))])

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

(defn listen! []
  (let [input (listen-to-keyboard)]
    (go-loop []
      (let [key (<! input)
            right 39
            left 37]
        (cond (= key right) (next! app-state slides)
              (= key left) (prev! app-state slides)))
      (recur))))



(defn on-js-reload []

  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
