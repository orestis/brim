(ns nvimgui.server-grid
  "Maintain a server-side grid")

(defn create-editor-ui []
  {:grids {}
   :modes-list []
   :cursor-style-enabled true
   :options {}
   :default-colors {}
   :highlight-ids {}
   :highlight-groups {}})

(defmulti redraw-event (fn [state event-name args]
                         event-name))

(defmethod redraw-event :default
  [state event-name args]
  (println "UNHANDLED event" event-name args)
  state)

(defmethod redraw-event "mode_info_set"
  [state _ [cursor-style-enabled mode-info]]
  (assoc state
         :cursor-style-enabled cursor-style-enabled
         :modes-list mode-info))

(defmethod redraw-event "option_set"
  [state _ [k v]]
  (assoc-in state [:options k] v))

(defmethod redraw-event "default_colors_set"
  [state _ [fg bg sp term-fg term-bg]]
  (assoc-in state [:default-colors]
            {:fg fg
             :bg bg
             :sp sp
             :term-fg term-fg
             :term-bg term-bg}))

(defmethod redraw-event "hl_attr_define"
  [state _ [id rgb cterm info]]
  ;; cterm we don't care
  ;; info is about ext_hlstate TODO
  (assoc-in state [:highlights id] rgb))

(defmethod redraw-event "grid_cursor_goto"
  [state _ [grid-id row col]]
  (assoc-in state [:grids grid-id :cursor] [row col]))

(defmethod redraw-event "mode_change"
  [state _ [mode mode-idx]]
  (assoc state 
         :mode mode
         :mode-idx mode-idx))

(defmethod redraw-event "hl_group_set"
  [state _ [name id]]
  (assoc-in state [:higlight-groups name] id))


(defn create-grid [w h]
  {:text (make-array Character/TYPE h w)
   :dimensions [w h]
   :hl-ids (make-array java.lang.Integer h w)})


(defmethod redraw-event "grid_resize"
  [state _ [grid-id w h]]
  (-> state
      (assoc-in [:grids grid-id] (create-grid w h))))


(defmethod redraw-event "grid_clear"
  [state _ [grid-id]]
  (let [[w h] (get-in state [:grids grid-id :dimensions])]
    (assoc-in state [:grids grid-id] (create-grid w h))))

(defmethod redraw-event "grid_destroy"
  [state _ [grid-id]]
  (update state :grids dissoc grid-id))



(defmethod redraw-event "grid_line"
  [state _ [grid-id row col-start cells]]
  (let [{:keys [text hl-ids]} (get-in state [:grids grid-id])
        idx (atom col-start)]
    ;; text and hl-ids are mutable! bang away
    (doseq [[s hl-id n] cells
            :let [n (or n 1)
                  hl-id (when hl-id (int hl-id))
                  c (first s)]]
      (loop [n n]
        (let [cur-idx @idx]
          (when-not (zero? n)
            (aset text row cur-idx c)
            (aset hl-ids row cur-idx hl-id)
            (swap! idx inc)
            (recur (dec n))))))
    state))


(defn debug-grid [{:keys [text dimensions]}]
  (let [[w h] dimensions
        sb (StringBuilder.)]
    (doseq [row (range h)]
      (.append sb (String. (aget text row)))
      (.append sb \newline))
    (str sb)))


(defn segment-row [hl]
  (loop [seg-start 0
         seg-length 1
         i 1
         last-hl (aget hl seg-start)
         ranges []]
    (if (= (alength hl) i)
      (conj ranges [last-hl seg-start seg-length])
      (let [cur-hl (aget hl i)]
        (if (or (nil? cur-hl) (= cur-hl last-hl))
          (recur seg-start (inc seg-length) (inc i) last-hl ranges)
          (recur i  1 (inc i) cur-hl (conj ranges
                                          [last-hl seg-start seg-length])))))))

(defn spans-from-ranges [text ranges]
  (let [sb (StringBuilder.)]
    (doseq [[hl-id start l] ranges]
      (.append sb "<span class='hl-")
      (.append sb hl-id)
      (.append sb "'>")
      (.append sb text start l)
      (.append sb "</span>"))
    (str sb)))


(defn html-grid [{:keys [text dimensions hl-ids]}]
  (let [[w h] dimensions]
    (into {}
          (for [row-idx (range h)
                :let [row-text (aget text row-idx) 
                      row-hl (aget hl-ids row-idx)
                      ranges (segment-row row-hl)]]
            [row-idx (spans-from-ranges row-text ranges)]))))

(defmethod redraw-event "flush"
  [state _ _]
  (println "FLUSH")
  (println (debug-grid (get-in state [:grids 1])))
  ;; put a channel in the editor ui so that on every flush
  ;; we can send in the grid?
  state)

(defn- nested-event-reducer [editor-ui [event-name & invocations]]
  (reduce #(redraw-event %1 event-name %2)
          editor-ui
          invocations))

(defn process-redraw [editor-ui events]
  (reduce nested-event-reducer
          editor-ui
          events))

