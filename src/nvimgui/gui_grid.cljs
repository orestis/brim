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
  (SafeStyle/create
           #js {:background-color (if background (utils/BinToCSS background) "inherit")
                :color (if foreground (utils/BinToCSS foreground) "inherit")}))

(defn install-sheets [highlights]
  (when @styles
    (style/uninstallStyles @styles))
  (let [rules (mapv (fn [[hl-id hl-value]]
                      (let [selector (str "[data-hl-id='" hl-id "']")
                            style (css-from-hl-attr hl-value)]
                        (SafeStyleSheet/createRule selector style)))
                    highlights)
        new-styles (apply SafeStyleSheet/concat rules)]
    (tap> new-styles)
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
  (let [last-seen-hl-id (atom nil)]
    (doseq [[col-start cells] row
            :let [col-span (dom/createDom "span" #js {:class "col-span"
                                                      :style (str "left:"
                                                                  col-start "ch")})]]
      (dom/append row-cont col-span)
      (doseq [[c hl-id n] cells
              :let [hl-id (or hl-id
                            @last-seen-hl-id)
                    _ (reset! last-seen-hl-id hl-id)
                    n (or n 1)
                    txt  (str/join "" (repeat n c))
                    dom (dom/createDom "span" #js {:class "hl-run"
                                                   :data-hl-id hl-id }
                                       txt)]]
        (dom/append col-span dom)))))

;; clear the root only on grid resize or on grid clear
;; compare last seen row with this one and don'n clobber over unchanged
;; rows
(defn- setup-grid [root-el {:keys [dimensions rows cursor] :as grid}]
  (let [[w h] dimensions
        pre-container (dom/createDom "pre" #js {:class "grid-container"})
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
  (tap> state)
  (let [grid (get-in state [:grids 1])
        highlights (:highlights state)
        default-colors (:default-colors state)]
    (when-not (= highlights (:highlights @last-seen))
      (install-sheets highlights)
      (swap! last-seen assoc :highlights highlights))
    (when-not (= default-colors (:default-colors @last-seen))
      (install-default-colors default-colors)
      (swap! last-seen assoc :default-colors default-colors))
    
    (setup-grid root grid))
  state)

(defmethod gui/ui-event "flush"
  [state _ _]
  (draw-grid state))



