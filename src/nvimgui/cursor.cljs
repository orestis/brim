(ns nvimgui.cursor
  (:require
    [goog.style :as style]))

(defonce modes (atom {}))
(defonce curr-mode-idx (atom nil))


(defn cursor-shape-class [shape percent]
  (case shape
    "block" nil
    "horizontal" nil
    "vertical" nil))

(defn set-mode-info [{:strs [cursor-style-enabled
                             modes-list]}]
  (when-not cursor-style-enabled
    (js/alert "cursor-style-enabled is false, can't handle this"))
  (js/console.log "MODES LIST" modes-list)
  (reset! modes modes-list)
  ;; TODO generate classes styles for each mode based on the shortname
  ;; (or perhaps the mode-idx?)
  ;; and set them to the body so that 
  ;; [data-mode='insert'] .cursor {border-right: 1-percentage ch;}
  ;; and so on
  )

(defn mode-change [{:strs [mode mode-idx]}]
  (js/console.log "MODE CHANGE" mode mode-idx)
  #_
  (let [mode (get @modes mode-idx)]
    (js/console.log)))

(defn set-cursor [[grid-id row col] grids]
  (let [grid (get grids grid-id)
        cursor (:cursor grid)]
    (set! (.-style cursor)
          (str "left: " col "ch; top: " row "rem;"))))
