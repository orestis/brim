(ns nvimgui.core)

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

(defmethod ui-event "grid_resize"
  [state _ [grid-id w h]]
  (assoc-in state [:grids grid-id :dimensions] [w h]))


(defmethod ui-event "grid_clear"
  [state _ [grid-id]]
  (assoc-in state [:grids grid-id :rows] {}))

(defmethod ui-event "grid_line"
  [state _ [grid-id row col-start cells]]
  (update-in state [:grids grid-id :rows row]
             (fnil conj [])
             [col-start cells]))

(defmethod ui-event "grid_cursor_goto"
  [state _ [grid-id row col]]
  (assoc-in state [:grids grid-id :cursor] [row col]))

(defmethod ui-event "mode_change"
  [state _ [mode mode-idx]]
  (assoc state 
         :mode mode
         :mode-idx mode-idx))

(defmethod ui-event "flush"
  [state _ _]
  (println "FLUSH")
  state)
