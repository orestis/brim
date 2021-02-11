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
  (js/console.log "defayt colors" default-colors)
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


(defonce grids (atom {}))

(defn resize-grid [arg]
  (let [[grid-id [w h]] arg
        grid-id (js/parseInt grid-id)]
    (when-let [previous (get-in @grids [grid-id :container])]
      (dom/removeNode previous))
    (let [pre-container (dom/createDom "pre" #js {:class "grid-container"
                                                  :data-grid-id grid-id})
          rows (atom {})
          row-els (mapv (fn [i]
                          (let [row (dom/createDom "div" 
                                                   #js {:class "row-container"
                                                        :data-row-idx i})]
                            (swap! rows assoc i row)
                            row))

                        (range h))]
      (swap! grids update grid-id assoc 
             :container pre-container
             :dimensions [w h]
             :rows @rows)
      (apply dom/append pre-container row-els)
      (dom/append root pre-container))))

(defn grid-clear [grid-id]
  (js/console.log "CLEAR" grid-id)
  (let [{:keys [dimensions]} (get @grids grid-id)]
    (resize-grid [grid-id dimensions])))


(defn draw-lines [[grid-id lines]]
  (let [grid-id (js/parseInt grid-id)
        grid (get @grids grid-id)
        rows (:rows grid)]
    (doseq [[row html] lines
            :let [row (js/parseInt row)
                  node (get rows row)]]
      (set! (.-innerHTML node) html))))






