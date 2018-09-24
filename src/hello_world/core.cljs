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

(defn with-style [dom style]
  (println "with-style: " dom)
  (let [div [:div {:class (str style)}]]
    (if (= :div (nth dom 0))
      (conj div (nth dom 2))
      (conj div dom))))

(defn empty-slide []
  [:div {:class "slide-container"}])

(defn simple-slide [title body]
  (let [container (empty-slide)
        dom-title (with-style (markdown title) "title")
        dom-body (with-style body "slide")]
    (conj container
          dom-title
          dom-body)))

(defn bullets [args]
  (loop [args args
         acc ""]
    (let [seperator (if (empty? acc) "* " "\n* ")]
      (if (empty? args)
        acc
        (recur (rest args) (string/join seperator [acc (first args)]))))))

(defn intro []
  (let [title "#clojure"
        text (markdown (bullets ["modern Lisp dialect, on the JVM"
                                 "immutable data structures"
                                 "novel approach to concurrency, no locks"
                                 ]))]
    (simple-slide title text)))

(defn markdown-code [code]
  (let []
    (->> code
         (markdown))))

(defn code-block [code]
  [:pre {}
   [:code {:class "language-clojure"
           :data-lang "clojure"}
    (prn-str code)]])


(comment (defn editor [content]
           [:div.container-fluid
            [:div.row
             [:div.col-sm-6
              [:textarea.form-control {:value @content
                                       :style {:class "editor"}
                                       :on-change #(reset! content (-> % .-target .-value))}]]]]))


(comment (defn code-component [content]
           (fn []
             [:div {:dangerouslySetInnerHTML
                    {:__html (display content)}}])))

(comment (defn preview [content]
           (when (not-empty @content)
             (code-component @content))))

(defn eval-expr [s]
  (println "eval-expr " s)
  (eval (empty-state)
        (read-string s)
        {:eval       js-eval
         :source-map true
         :context    :expr}
        identity))


(defn render-code [this]
  (.log js/console "render-code")
  (->> this reagent/dom-node (.highlightBlock js/hljs)))

(defn result-ui [output]
  (reagent/create-class
   {:render (fn []
              [:pre>code.clj
               (with-out-str (pprint (get @output :value)))])
    :component-did-mouth render-code
    }))


(println (intro))



(defn table-with-cols [& args]
  [:table
     [:tbody
      [:tr
       (for [arg args]
         [:td [arg]])]]])

(println (table-with-cols "2" "4" "5"))

(comment (defn editor-did-mouth [input]
           (fn [this]
             (let [cm (.fromTextArea js/CodeMirror
                                     (reagent/dom-node this)
                                     #js {:mode "clojure"
                                          :lineNumbers true})]
               (.on cm "change" #(reset! input (.getValue %)))))))


(defn editor-did-mount [input]
  (fn [this]
    (let [cm (.fromTextArea  js/CodeMirror
                             (reagent/dom-node this)
                             #js {:mode "clojure"
                                  :lineNumbers true})]
      (.on cm "change" #(reset! input (.getValue %))))))

(defn editor-ui [input]
  (reagent/create-class
   {:render (fn [] [:textarea
                    {:default-value ""
                     :auto-complete "off"}])
    :component-did-mount (editor-did-mount input)}))

(defn debug []
  (let [input (reagent/atom nil)
        output (reagent/atom nil)
        editor (editor-ui input)
        eval-btn (fn []
                   [:button {:class "eval"
                             :on-click #(reset! output (eval-expr @input))}])
        result (result-ui output)
        columns (fn [& args]
                     (for [[key, arg] (map-indexed vector args)]
                       [:td {:key key}
                        (if (vector? arg) arg
                            [arg])]))
        body [:table
              [:tbody
               [:tr
               (columns editor eval-btn result)]]]]
    (simple-slide "#repl-debug"
                  body)))

(println (debug))








(comment [:div.container-fluid {:style {:class "slide"}}
          [:div.row
           [:div.col-sm-6
            [code-ct]]




           [:div.col-sm-6
            [:h3 "Preview"]
            [output-ct]]]])
(comment )
(defn repl []
  (let [input (reagent/atom nil)
        output (reagent/atom nil)
        container (empty-slide)
        code-ct (editor-ui input)
        output-ct (result-ui output)]
    [:div {:class "slide-container"}
     (with-style (markdown "#repl") "title")
     [:div {:class "slide"}
      [:table
       [:tbody
        [:tr
         [:td
          [code-ct]]
         [:td [:button {:class "eval"
                        :on-click #(reset! output (eval-expr @input))}]]
         [:td [output-ct]]]]]]]))

(defn slide3 []
  (let [title "#Clojure Presentation"
        text  (bullets ["Body **tesxt** is really awesome"
                        "more bullet"])]
    (simple-slide title text)))

(defn slide4 []
  (fn []
    [:div {:style {:height "20px"
                   :width "20px"
                   :background-color "#bbb"
                   :border-radius "50%"
                   :display "inline-block"}}]))


(def slides [intro, debug])

(defn slide []
  (fn []
    (let [i (get @app-state :current)
          current (nth slides i)]
      (.log js/console (str "current slide: " i))
      [current])))

(defn prev-slide![]
  (println "prev-slide")
  (let [cyclic-dec #(mod (dec %) (count slides))]
    (swap! app-state update-in [:current] cyclic-dec)))

(defn next-slide! []
  (println "next-slide!")
  (let [cyclic-inc #(mod (inc %) (count slides))]
    (swap! app-state update-in [:current] cyclic-inc)))

(defn set-slide! [i]
  (swap! app-state assoc :current i))


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

(let [input (listen-to-keyboard)]
  (go-loop []
    (let [key (<! input)
          right 39
          left 37]
      (cond (= key right) (next-slide!)
            (= key left) (prev-slide!)))
    (recur)))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
