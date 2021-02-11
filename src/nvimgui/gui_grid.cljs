(ns nvimgui.gui-grid
  (:require [nvimgui.gui-events :as gui]
            [goog.dom :as dom]
            [clojure.string :as str]
            [goog.style :as style]
            ["/js/utils" :as utils])
  (:import [goog.html 
           SafeStyle SafeStyleSheet]))

(defonce root (.getElementById js/document "app"))
(defonce styles (atom nil))
(defonce default-styles (atom nil))

(defn css-from-hl-attr [{:strs [foreground background
                                special reverse
                                italic bold strikethrough
                                undercurl underline
                                blend]}]
  (let [bg (if reverse foreground background)
        fg (if reverse background foreground)]
    (SafeStyle/create
      #js {:background-color 
           (if bg (utils/BinToCSS bg) "inherit")
           :color
           (if fg (utils/BinToCSS fg) "inherit")
           :font-weight (if bold "bold" "normal")
           :text-decoration-color
           (if special (utils/BinToCSS special) "inherit")
           :text-decoration-line (if-not (or underline strikethrough)
                                   "none"
                                   (str (when underline "underline")
                                        " "
                                        (when strikethrough "line-through")))})))

(defn install-sheets [highlights]
  (when @styles
    (style/uninstallStyles @styles))
  (let [rules (mapv (fn [[hl-id hl-value]]
                      (let [selector (str ".hl-" hl-id "")
                            style (css-from-hl-attr hl-value)]
                        (SafeStyleSheet/createRule selector style)))
                    highlights)
        new-styles (apply SafeStyleSheet/concat rules)]
    (reset! styles
            (style/installSafeStyleSheet new-styles))))

(defn install-default-colors [default-colors]
  (when @default-styles
    (style/uninstallStyles @default-styles))
  (let [{:keys [fg bg sp]} default-colors
        rule (SafeStyleSheet/createRule ":root"
                                        (SafeStyle/create
                                          #js {:--default-bg-color (utils/BinToCSS bg)
                                               :--default-fg-color (utils/BinToCSS fg)
                                               :--default-sp-color (utils/BinToCSS sp)}))]
    (reset! default-styles
            (style/installSafeStyleSheet rule))))

(defn draw-row [row-cont row]
  (dom/removeChildren row-cont)
  (let [last-seen-hl-id (atom nil)
        idx (atom 0)]
    (doseq [[c hl-id] row
            :let [hl-id (or hl-id
                            @last-seen-hl-id)
                  _ (reset! last-seen-hl-id hl-id)
                  col-span (dom/createDom "span" 
                                          #js {:class "col-span"
                                               :data-hl-id hl-id
                                               :style (str "left:"
                                                           @idx "ch")}
                                          c)]]
      (swap! idx inc)
      (dom/append row-cont col-span))))

(defn update-row [row-cont row]
  (let [last-seen-hl-id (atom nil)
        cells (dom/getChildren row-cont)
        idx (atom 0)]
    (doseq [col-span cells ;[c hl-id] row
            :let [[c hl-id] (nth row @idx)
                  hl-id (or hl-id
                            @last-seen-hl-id)
                  _ (reset! last-seen-hl-id hl-id)
                  #_#_
                  col-span (nth cells @idx)]]
      (.setAttribute col-span "data-hl-id" hl-id)
      (dom/setTextContent col-span c)
      (swap! idx inc)
      ))
  )

(defn update-grid [root-el rows last-seen]
  (let [idx (atom 0)
        row-els (dom/getChildren root-el)]
    (doseq [row-cont row-els
            :let [cur-idx @idx
                  row (get rows cur-idx)]]
      (.mark js/performance (str "update-row:start:" cur-idx))
      (when-not (identical? (get-in @last-seen [:rows cur-idx])
                            row)
        (update-row row-cont row)
        (swap! last-seen assoc-in [:rows cur-idx] row))
      (.mark js/performance (str "update-row:end" cur-idx))
      (.measure js/performance (str "update-row:" cur-idx)
                (str "update-row:start:" cur-idx)
                (str "update-row:end" cur-idx))
      (swap! idx inc))))

;; clear the root only on grid resize or on grid clear
;; compare last seen row with this one and don'n clobber over unchanged
;; rows
(defn- setup-grid [root-el grid-id {:keys [dimensions rows cursor] :as grid}]
  (let [[w h] dimensions
        pre-container (dom/createDom "pre" #js {:class "grid-container"
                                                :data-grid-id grid-id})
        row-els (mapv (fn [i]
                        (let [row-cont (dom/createDom "div" #js {:class "row-container"
                                                                 :data-row-idx i})]
                          (draw-row row-cont (get rows i))
                          row-cont))
                      (range h))]
    (dom/removeChildren root-el)
    (apply dom/append pre-container row-els)
    (dom/append root-el pre-container)))

(defonce last-seen (atom {}))
(defn draw-grid [state]
  (let [grid-id 1
        grid (get-in state [:grids grid-id])
        highlights (:highlights state)
        dimensions (:dimensions grid)
        rows (:rows grid)
        default-colors (:default-colors state)]
    (when-not (= highlights (:highlights @last-seen))
      (install-sheets highlights)
      (swap! last-seen assoc :highlights highlights))
    (when-not (= default-colors (:default-colors @last-seen))
      (install-default-colors default-colors)
      (swap! last-seen assoc :default-colors default-colors))
    (when-not (= dimensions (:dimensions @last-seen))
      (setup-grid root grid-id grid)
      (swap! last-seen assoc :dimensions dimensions))
    
    (update-grid (.querySelector root (str "[data-grid-id=\"" grid-id "\"]"))
                 rows
                 last-seen)
    #_
    (setup-grid root grid))
  state)

(defmethod gui/ui-event "flush"
  [state _ _]
  (js/requestAnimationFrame (fn []
                              (.mark js/performance "draw-grid:start")
                              (draw-grid state)
                              (.mark js/performance "draw-grid:end")
                              (.measure js/performance "draw-grid" "draw-grid:start" "draw-grid:end")))
  state)



