(ns mdt.app
  (:require [reagent.core :as reagent]
            [goog.dom :as gdom]
            [clojure.string :as string]
            [cljs.pprint]
            [cljsjs.mobiledoc-kit]))

(enable-console-print!)

(def mention-atom
  #js {:name "mention"
       :type "dom"
       :render (fn [d]
                 (let [data (js->clj d)]
                   (gdom/createDom "span" #js {:style "color:blue"} (get data "value"))))})

(defonce db
  (reagent/atom
   {:doc {:version "0.3.0"
          :markups []
          :atoms []
          :cards []
          :sections [[1, "p", [[0, [], 0, "Welcome to Mobiledoc"]]]]}
    :actions []}))

;; export default {
;;   name: 'mention',
;;   type: 'dom',
;;   render({ env, options, value, payload}) {
;;     return document.createTextNode(`@${value}`);
;;   }
;; };
(defn get-selection-rect []
  (let [sel (js/window.getSelection)
        rng (and sel (.-rangeCount sel) (.getRangeAt sel 0))]
    (when rng
      (.getBoundingClientRect rng))))

(defn selection-tether [props & children]
  (let [pos (reagent/atom nil)]
    (reagent/create-class
     {:component-did-mount
      (fn [_]
        (when-let [r (get-selection-rect)]
          (reset! pos {:left (.-left r) :top (.-top r)})))
      :component-did-update
      (fn [comp [_ old-props]]
        (let [new-props (reagent/props comp)]
          (if (not= new-props old-props)
            (if-let [r (get-selection-rect)]
              (reset! pos {:left (.-left r) :top (.-top r)})))))
      :reagent-render
      (fn [props & children]
        (when @pos
          (into [:div {:data-x (pr-str props)
                       :style {:position "fixed"
                               :left (:left @pos)
                               :top (- (:top @pos) 20)}}]
                children)))})))

(defn on-text-input [editor re-or-str cb]
  (cond (string? re-or-str) (.onTextInput editor #js {:text re-or-str :run cb})
        (regexp? re-or-str) (.onTextInput editor #js {:match re-or-str :run cb})))

(defn editor [doc]
  (let [s (reagent/atom nil)]
    (reagent/create-class
     {:component-did-mount
      (fn [this]
        (let [editor (js/Mobiledoc.Editor. (clj->js {:mobiledoc doc
                                                     :atoms [mention-atom]}))]
          (swap! s assoc :editor editor)
          (.render editor (gdom/getElement "editor"))
          (.postDidChange editor (fn [] (swap! db assoc :doc (js->clj (.serialize editor)))))
          (.cursorDidChange editor (fn []
                                     ;; (js/console.log (.-range editor))
                                     (if (-> editor .-range .-direction)
                                       (swap! s assoc :ui? [(-> editor .-range .-head .-offset)
                                                            (-> editor .-range .-tail .-offset)])
                                       (swap! s dissoc :ui?))))
          (on-text-input editor #"@\w*$" (fn [editor matches]
                                           (swap! db update :actions conj [:at/change (js->clj matches)])))
          (on-text-input editor
                         #"@\w+\s$"
                         (fn [editor matches]
                           ;; editor.selectRange(range.extend(-1)); // select the '@' character before the cursor
                           ;; editor.insertAtom('mention-atom', userName); // r
                           (.selectRange editor (-> editor .-range (.extend (- (count (first matches))))))
                           (.insertAtom editor "mention" (string/trim (first matches)))
                           (.insertText editor " ")
                           (swap! db update :actions conj [:at/complete (js->clj matches)])))))
      :reagent-render
      (fn [doc]
        ;; (prn @s)
        [:div
         [:div#editor {:style {:height "4rem" :margin-left "4rem"}}]
         (when (:ui? @s) [selection-tether
                          {:bounds (:ui? @s)}
                          [:button {:on-click (fn [] (.run (:editor @s) (fn [post-editor] (.toggleMarkup post-editor "strong"))))}
                           "bold"]
                          [:button {:on-click (fn [] (.run (:editor @s) (fn [post-editor] (.toggleMarkup post-editor "i"))))} "italic"]])])})))

(defn actions [action-list]
  (into [:ul] (map (fn [a] [:li (pr-str a)]) action-list)))

(defn app [db]
  [:div 
   {:style {:max-width "40rem" :padding-top "4rem" :margin "0 auto"}}
   [editor (:doc @db)]
   [actions (take-last 20 (:actions @db))]
   [:pre (with-out-str (cljs.pprint/pprint (:doc @db)))]])

(defn init []
  (reagent/render-component [app db] (gdom/getElement "container")))
