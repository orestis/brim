(ns nvimgui.gui-events)

(defonce gui-state (atom {}))

(defmulti ui-event (fn [state event-name args]
                     event-name))


(defn process-redraw-events [events]
  (doseq [[event-name & invocations] events]
    (doseq [args invocations]
      (swap! gui-state
             ui-event 
             event-name
             args))))

(defmethod ui-event :default
  [state event-name args]
  (println "UNHANDLED event" event-name args)
  state)

(defmethod ui-event "mode_info_set"
  [state _ [cursor-style-enabled mode-info]]
  (assoc state
         :cursor-style-enabled cursor-style-enabled
         :modes-list mode-info))

(defmethod ui-event "option_set"
  [state _ [k v]]
  (assoc-in state [:options k] v))

(defmethod ui-event "default_colors_set"
  [state _ [fg bg sp term-fg term-bg]]
  (assoc-in state [:default-colors]
            {:fg fg
             :bg bg
             :sp sp
             :term-fg term-fg
             :term-bg term-bg}))

(defmethod ui-event "hl_attr_define"
  [state _ [id rgb cterm info]]
  ;; cterm we don't care
  ;; info is about ext_hlstate TODO
  (assoc-in state [:highlights id] rgb))

(defmethod ui-event "hl_group_set"
  [state _ [name id]]
  (assoc-in state [:higlight-groups name] id))


(defn update-row [row col-start cells]
  ;; the row is a vector of cells
  ;; each cell is a vector of a char and a (posisble) hl-id
  ;; we can always assume that the row is initialized with 
  ;; the current number of cells?
  (let [r (transient row)
        idx (atom col-start)]
    (->
      (reduce (fn [acc [c hl-id n]]
                (let [n (or n 1)]
                  (loop [acc acc
                         n n]
                    (let [cur-idx @idx]
                      (if (zero? n)
                        acc
                        (do
                          (swap! idx inc)
                          (recur (assoc! acc cur-idx [c hl-id])
                                 (dec n))))))))
              r
              cells)
      persistent!)))


(defn init-row [w]
  (apply vector (repeat w [""])))

#_
(update-row (init-row 10)
            0 [["h" 5 5]
               ["n"]
               ["1" 1]])


(defn init-grid-rows [w h]
  (let [empty-row (init-row w)]
    (persistent!
      (reduce (fn [rows idx]
                (assoc! rows idx empty-row))
              (transient {})
              (range h)))))

(defmethod ui-event "grid_resize"
  [state _ [grid-id w h]]
  (-> state
      (assoc-in [:grids grid-id :dimensions] [w h])
      (assoc-in [:grids grid-id :rows]
                (init-grid-rows w h))))


(defmethod ui-event "grid_clear"
  [state _ [grid-id]]
  (let [[w h] (get-in state [:grids grid-id :dimensions])]
    (assoc-in state [:grids grid-id :rows] 
              (init-grid-rows w h))))


(defmethod ui-event "grid_line"
  [state _ [grid-id row col-start cells]]
  (let [[w _] (get-in state [:grids grid-id :dimensions])
        empty-row (init-row w)]
    (update-in state [:grids grid-id :rows row]
               (fnil update-row empty-row) 
               col-start cells)))

(defmethod ui-event "grid_cursor_goto"
  [state _ [grid-id row col]]
  (assoc-in state [:grids grid-id :cursor] [row col]))

(defmethod ui-event "mode_change"
  [state _ [mode mode-idx]]
  (assoc state 
         :mode mode
         :mode-idx mode-idx))

#_
(defmethod ui-event "flush"
  [state _ _]
  (println "FLUSH")
  state)
