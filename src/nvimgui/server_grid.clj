(ns nvimgui.server-grid
  "Maintain a server-side grid"
  (:require [clojure.core.async :as a]))

(set! *warn-on-reflection* true)

(defn create-editor-ui [ops-chan]
  {:grids {}
   :ops-chan ops-chan})

(defmulti redraw-event (fn [state event-name args]
                         event-name))

(defmethod redraw-event :default
  [state event-name args]
  (println "UNHANDLED event" event-name args)
  state)

(defmethod redraw-event "mode_info_set"
  [state _ [cursor-style-enabled mode-info]]
  (update state :current-ops
          conj {:cursor-style-enabled cursor-style-enabled
                :modes-list mode-info}))

(defmethod redraw-event "option_set"
  [state _ [k v]]
  (update-in state [:current-ops 0 :options]
          assoc k v))

(defmethod redraw-event "default_colors_set"
  [state _ [fg bg sp term-fg term-bg]]
  (update state :current-ops 
          conj {:default-colors
                {:fg fg :bg bg :sp sp}}))

(defmethod redraw-event "hl_attr_define"
  [state _ [id rgb cterm info]]
  ;; cterm we don't care
  ;; info is about ext_hlstate TODO
  (update-in state [:current-ops 0 :highlights] assoc id rgb))

(defmethod redraw-event "grid_cursor_goto"
  [state _ [grid-id row col]]
  (update state :current-ops conj
          {:cursor [grid-id row col]}))

(defmethod redraw-event "mode_change"
  [state _ [mode mode-idx]]
  (update state :current-ops 
          conj {:mode mode
                :mode-idx mode-idx}))

(defmethod redraw-event "hl_group_set"
  [state _ [name id]]
  (update-in state [:current-ops 0 :highlight-groups] assoc name id))


(defn make-char-array [w h]
  (make-array java.lang.String h w))

(defn make-hl-array [w h]
  (make-array java.lang.Integer h w))

(defn create-grid [w h]
  {:text (make-char-array w h)
   :dimensions [w h]
   :hl-ids (make-hl-array w h)})


(defmethod redraw-event "grid_resize"
  [state _ [grid-id w h]]
  (-> state
      (assoc-in [:grids grid-id] (create-grid w h))
      (update-in [:current-ops 0 :grid-resize] assoc grid-id [w h])))

(defmethod redraw-event "grid_clear"
  [state _ [grid-id]]
  (let [[w h] (get-in state [:grids grid-id :dimensions])]
    (-> state
        (assoc-in [:grids grid-id] (create-grid w h))
        (update-in [:current-ops 0 :grid-clear] conj grid-id))))

(defmethod redraw-event "grid_destroy"
  [state _ [grid-id]]
  (-> state
      (update :grids dissoc grid-id)
      (update :current-ops conj {:grid-destroy grid-id})))




(comment
(defn debug-grid [{:keys [text dimensions]}]
  (let [[w h] dimensions
        sb (StringBuilder.)]
    (doseq [row (range h)]
      (.append sb (String. (aget text row)))
      (.append sb \newline))
    (str sb))))


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


(defn escape-span [text start l]
  (let [sb (StringBuilder.)]
    (doseq [i (range start (+ start l))
            :let [c (aget text i)]]
      (case c
        nil (.append sb " ")
        "<" (.append sb "&lt;")
        ">" (.append sb "&gt;")
        "&" (.append sb "&amp;")
        "'" (.append sb "&#39;")
        "\"" (.append sb "&quot;")
        (.append sb c)))
    (str sb)))

(defn spans-from-ranges [text ranges]
  (let [sb (StringBuilder.)]
    (doseq [[hl-id start l] ranges]
      (.append sb "<span class='hl-")
      (.append sb hl-id)
      (.append sb "'>")
      (.append sb (escape-span  text start l))
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

(defn html-line [state grid-id row-idx]
  (let [{:keys [text hl-ids]} (get-in state [:grids grid-id])
        row-text (aget text row-idx)
        row-hl (aget hl-ids row-idx)
        ranges (segment-row row-hl)]
    (spans-from-ranges row-text ranges)))

(defmethod redraw-event "grid_line"
  [state _ [grid-id row col-start cells]]
  (let [{:keys [text hl-ids]} (get-in state [:grids grid-id])
        idx (atom col-start)
        last-seen-hl-id (atom nil)]
    ;; text and hl-ids are mutable! bang away
    #_
    (when (= row 0)
      (println "ROW 0" col-start (pr-str  cells)))

    (doseq [[s hl-id n] cells
            :let [n (or n 1)
                  hl-id (if hl-id
                          (reset! last-seen-hl-id (int hl-id))
                          @last-seen-hl-id)
                  c (or s " ")]]
      (loop [n n]
        (let [cur-idx @idx]
          (when-not (zero? n)
            (try
              #_
              (when (= row 0)
                (println "setting " cur-idx (pr-str c)))
              (aset text row cur-idx c)
              (aset hl-ids row cur-idx hl-id)
              (catch Exception e

                (println "EXCEPITION" e (.getMessage e) "setting idx " cur-idx "char" (pr-str c) "hl-id" hl-id )
                (println "CELLS WAS " (pr-str cells))))

              (swap! idx inc)
            (recur (dec n))))))
    (update-in state [:current-ops 0 :grid-lines grid-id]
               assoc row (html-line state grid-id row))))

(defn copy-array [cls arr from to]
  (assert (>= from 0) "from should be non-negative")
  (assert (< from (alength arr)) "from should be less than length")

  (condp = cls
    java.lang.String
    (java.util.Arrays/copyOfRange ^"[Ljava.lang.String;" arr (int from) (int to))
    java.lang.Integer
    (java.util.Arrays/copyOfRange ^"[Ljava.lang.Integer;" arr (int from) (int to))))

(defn create-array [cls w h]
  (condp = cls
    java.lang.String
    (make-char-array w h)
    java.lang.Integer
    (make-hl-array w h)))

;; all wrong! top bot / left right are the scrolling region
;; so all changes should be confined within that area
;; and rows is the amount of copying to do within the area

(defn get-scroll-region-bounds [top bot rows]
  (let [h (- bot top)
        src-top (if (pos? rows) rows 0)
        src-bot (if (pos? rows) h (+ h rows))
        dst-top (if (pos? rows) 0 (- rows))
        dst-bot (if (pos? rows) (- h rows) h) ]
    [src-top src-bot dst-top dst-bot]))

(defn scroll-array [arr top bot left right rows]
  ;; array is a 2d array
  ;; we need to make a copy of the region defined by top/bot/left/right
  (let [w (- right left)
        h (- bot top)
        cls (class (aget arr 0 0))
        region (create-array cls w h)
        [src-top src-bot dst-top dst-bot]
        (get-scroll-region-bounds top bot rows)]
    (assert (= (type arr) (type region)))
    (doseq [i (range h)
            :let [row (aget arr (+ i top))
                  row-copy (copy-array cls row left right)]]
      (aset region i row-copy))
    ;; region now contains a snapshot of the SR
    ;; if the rows is positive, we are scrolling down
    ;; which means that we throw away the top part of the region
    ;; and vice-versa for negative rows, throw away the bottom part
    (doseq [[src-i dst-i] (map vector 
                               (range src-top src-bot) 
                               (range dst-top dst-bot))
            :let [src-line (aget region src-i)
                  abs-dst-idx (+ top dst-i)]]
      (System/arraycopy src-line 0
                        (aget arr abs-dst-idx)
                        left w))
    [(+ top dst-top)
     (+ top dst-bot)]))


(defmethod redraw-event "grid_scroll"
  [state _ [grid-id top bot left right rows _cols]]
  (let [{:keys [text hl-ids]} (get-in state [:grids grid-id])
        [changed-row-start changed-row-end] (scroll-array text top bot left right rows)
        _(scroll-array hl-ids top bot left right rows) ]
    (reduce (fn [state row]
              (update-in state [:current-ops 0 :grid-lines grid-id]
                         assoc row (html-line state grid-id row)))
           state (range changed-row-start changed-row-end))))

(defn compress-ops [ops]
  (->> ops
       (partition-by keys)
       (map #(apply merge-with merge %))))


(defmethod redraw-event "flush"
  [state _ _]
  (let [c (:ops-chan state)
        ops (:pending-ops state)]
    (def OPS ops)
    (a/>!! c ops))
  (assoc state :pending-ops []))

(defn handle-redraw-event [editor-ui event-name args]
  (let [current-event (:current-event editor-ui)
        current-op-col (:current-ops editor-ui)
        new-event (not= current-event event-name)]

    (redraw-event (cond-> editor-ui 
                    new-event
                    (-> 
                      (assoc :current-ops [])
                      (update :pending-ops conj [current-event current-op-col]))
                    :always
                    (assoc 
                      :current-event event-name))
                  event-name args)))

(defn- nested-event-reducer [editor-ui [event-name & invocations]]
  (reduce #(handle-redraw-event %1 event-name %2)
          editor-ui
          invocations))

(defn process-redraw [editor-ui events]
  (def EVENTS events)
  (let [pending-ops []]
    (reduce nested-event-reducer
            (assoc editor-ui 
                   :pending-ops pending-ops
                   :current-ops [])
            events)))

