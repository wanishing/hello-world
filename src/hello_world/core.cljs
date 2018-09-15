(ns hello-world.core
  (:require [reagent.core :as reagent :refer [atom]]
            ))

(enable-console-print!)

(println "This text is printed from src/hello-world/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

                                        ;
;(defonce app-state (atom 0))
(def app-state (atom {:count 0 }))

(def slide1 (str "Slide 1"))
(def slide2 (str "Slide 2"))
(def slides [slide1, slide2])

(defn bounded-inc [i]
  (if (= i (dec (count slides)))
    i
    (inc i)))

(defn bounded-dec [i]
  (if (zero? i)
    i
    (dec i)))

(defn slide []
  (fn []
    (let [slide (nth slides (get @app-state :count))]
      [:div
       [:p "Current: " slide]
       (.log js/console (str "Count is " (get @app-state :count)))
       [:button {:onClick #(swap! app-state update-in [:count] bounded-dec)} "prev"]
       [:button {:onClick #(swap! app-state update-in [:count] bounded-inc)} "next"]
       ])))

(reagent/render-component [slide]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
