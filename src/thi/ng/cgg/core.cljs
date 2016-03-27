(ns thi.ng.cgg.core
  (:require-macros
   [reagent.ratom :refer [reaction]]
   [cljs-log.core :refer [debug info]])
  (:require
   [reagent.core :as r]
   [thi.ng.color.core :as col]
   [thi.ng.color.gradients :as grad]
   [thi.ng.strf.core :as f]
   [thi.ng.math.core :as m :refer [PI TWO_PI HALF_PI]]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.svg.adapter :as svgadapt]
   [thi.ng.geom.viz.core :as viz]))

(defonce app
  (r/atom
   {:ranges  [[0 1] [0 1] [0 HALF_PI] [0 TWO_PI]]
    :globals [0 1 1 0]
    :viz     {:x-axis (viz/linear-axis
                       {:domain [0 1] :range [50 580] :major 0.5 :minor 0.125 :pos 250})
              :y-axis (viz/linear-axis
                       {:domain [0 1] :range [250 20] :major 0.2 :minor 0.1 :pos 50
                        :label-dist 15 :label-style {:text-anchor "end"}})
              :grid   {:minor-x true
                       :minor-y true}}}))

(def fmt-vec
  (let [ff (f/float 3)]
    #(apply f/format ["[" ff " " ff " " ff "]"] %)))

(defn channel-specs
  [colors]
  (->> ["red" "green" "blue"]
       (map-indexed
        (fn [idx col]
          {:values  (viz/uniform-domain-points [0 1] (map #(nth (deref %) idx) colors))
           :attribs {:fill "none" :stroke col}
           :layout  viz/svg-line-plot}))
       (vec)))

(defn color-bars
  [x1 x2 y w h colors]
  (let [n (dec (count colors))]
    (for [i (m/norm-range n)]
      (svg/rect [(m/mix* x1 x2 i) y] w h {:fill (nth colors (int (* i n)))}))))

(defn update-viz!
  []
  (let [grad (grad/cosine-gradient 100 (:coeffs @app))]
    (swap! app assoc :gradient grad)
    (swap! app assoc-in [:viz :data] (channel-specs grad))))

(defn update-coeff!
  [id idx]
  (fn [e]
    (swap! app assoc :preset :user)
    (swap! app assoc-in [:coeffs id idx] (-> e .-target .-value f/parse-float))
    (swap! app assoc :coeffs-orig (:coeffs @app))
    (update-viz!)))

(defn update-global!
  [id f]
  (fn [e]
    (let [x (-> e .-target .-value f/parse-float)]
      (swap! app assoc :preset :user)
      (swap! app assoc-in [:globals id] x)
      (swap! app assoc-in [:coeffs id] (mapv #(f % x) (get-in @app [:coeffs-orig id])))
      (update-viz!))))

(defn set-preset!
  [id]
  (swap! app assoc :preset id :coeffs (grad/cosine-schemes id) :globals [0 1 1 0])
  (swap! app assoc :coeffs-orig (:coeffs @app))
  (update-viz!))

(defn randomize-coeffs!
  []
  (swap! app assoc :preset :user)
  (swap! app assoc :coeffs
         (mapv
          (fn [i] (vec (repeatedly 3 #(apply m/random (get-in @app [:ranges i])))))
          (range 4)))
  (swap! app assoc :coeffs-orig (:coeffs @app))
  (update-viz!))

(defn slider
  [props]
  [:p
   [:input (merge {:type :range :min (- PI) :max PI :step 0.01} props)]
   [:input (merge {:type :number :step 0.01} props)]])

(defn preset-chooser
  []
  (let [presets (->> grad/cosine-schemes keys (map name) sort (cons "Choose a preset:"))
        sel     (reaction (name (:preset @app)))]
    (fn []
      [:select {:on-change #(-> % .-target .-value keyword set-preset!)
                :value @sel}
       (for [id presets] [:option {:key id :value id} id])])))

(defn gradient-graph
  []
  (let [colors (reaction (:gradient @app))
        spec   (reaction (:viz @app))]
    (fn []
      (let [body (viz/svg-plot2d-cartesian @spec)
            bars (color-bars 50 570 280 10 20 @colors)]
        [:div#viz
         (svgadapt/inject-element-attribs
          (svg/svg {:width 650 :height 300} body bars))]))))

(defn channel-controls
  [id [r g b]]
  [:div
   [slider {:value r :on-change (update-coeff! id 0)}]
   [slider {:value g :on-change (update-coeff! id 1)}]
   [slider {:value b :on-change (update-coeff! id 2)}]])

(defn global-controls
  [id params f min max]
  [slider {:value (params id) :min min :max max :on-change (update-global! id f)}])

(defn gradient-controls
  []
  (let [coeffs  (reaction (:coeffs @app))
        globals (reaction (:globals @app))]
    (fn []
      [:div
       [:table
        [:tbody
         [:tr [:th "DC Offset"] [:th "Amp"] [:th "Freq"] [:th "Phase"]]
         [:tr
          [:td [channel-controls 0 (nth @coeffs 0)]]
          [:td [channel-controls 1 (nth @coeffs 1)]]
          [:td [channel-controls 2 (nth @coeffs 2)]]
          [:td [channel-controls 3 (nth @coeffs 3)]]]
         [:tr [:th "Global controls"]]
         [:tr
          [:td [global-controls 0 @globals + -1 1]]
          [:td [global-controls 1 @globals * 0 2]]
          [:td [global-controls 2 @globals * 0 2]]
          [:td [global-controls 3 @globals + (- PI) PI]]
          ]]]])))

(defn app-component
  []
  (let [coeffs (reaction (:coeffs @app))]
    (fn []
      (let [coeffs' (interpose " " (map fmt-vec @coeffs))]
        [:div
         [:div
          [preset-chooser]
          [:button {:on-click randomize-coeffs!} "Randomize"]]
         [gradient-graph]
         [gradient-controls]
         [:p "Vector of coefficients for the above shown gradient:" [:br]
          [:pre "[" coeffs' "]"]]
         [:p "To sample the gradient and produce a seq of N RGBA colors:"
          [:pre "(require '[thi.ng.color.gradients :as grad])

(grad/cosine-gradient
  100 [" coeffs' "]))"]]]))))

(defn main
  []
  (set-preset! :rainbow1)
  (r/render-component [app-component] (.getElementById js/document "app")))

(main)
