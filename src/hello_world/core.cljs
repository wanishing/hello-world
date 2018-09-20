(ns hello-world.core
  (:require [reagent.core :as reagent :refer [atom]]
            [markdown-to-hiccup.core :as m]
            [garden.core :refer [css]]
            [clojure.string :as string]
            [goog.events :as events]
            [cljs.core.async :refer [chan dropping-buffer put! <! go]])
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


(defn markdown [text]
  (let [add-style #(conj [:div {:class "slide"}] (nth % 2))]
    (->> text
         (m/md->hiccup)
         (m/component)
         (add-style))))


(defn slide3 []
  (markdown "Old **markdown** test"))

(defn slide1 []
  [:div {:class "slide"}
   [:q "I love you the more in that I believe you had liked me for my own sake and for nothing else"]
   [:p {:class "author"} "John Keats"]])

(defn slide2 []
  [:div {:class "slide"}
   [
    :q "But man is not made for defeat. A man can be destroyed but not defeated."]
   [:p {:class "author"} "Ernest Hemingway"]])

(def slides [slide1, slide2, slide3])

(defn slide []
  (fn []
    (let [current (nth slides (get @app-state :current))]
       [current])))


(defn cyclic-inc [i]
  (mod (inc i) (count slides)))

(defn cyclic-dec [i]
  (mod (dec i) (count slides)))

(defn prev-slide []
  (println "prev-slide")
  (swap! app-state update-in [:current] cyclic-dec))

(defn next-slide []
  (println "next-slide")
  (swap! app-state update-in [:current] cyclic-inc))


(defn prev-button []
  (fn []
    [:a {:class "prev"
         :onClick prev-slide}
     "<"]))

(defn next-button []
  (fn []
    [:a {:class "next"
         :onClick next-slide}
     ">"]))

(defn main-container []
  [:div {:class "slideshow-container"}
   [slide]
   [prev-button]
   [next-button]
  [:div {:class "dot-container"}
   (for [i (range (count slides))]
     [:span {:class "dot"
             :key (str i)
             :onClick #(swap! app-state assoc :current i)}])]])

(reagent/render-component [main-container]
                          (. js/document (getElementById "app")))

(def keyboard-events
    (.-KEYDOWN events/EventType))

(def event-source
  (.-body js/document))

(defn listen-to-keyboard []
  (let [event-ch (chan (dropping-buffer 1))]
    (events/listen event-source
                   keyboard-events
                   #(put! event-ch #(.-keyCode %)))
    event-ch))

(let [input (listen-to-keyboard)]
  (go-loop []
    (let [key (<! input)
          right 39
          left 37]
      (cond (= key right) (next-slide)
            (= key left) (prev-slide)))
    (recur)))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
