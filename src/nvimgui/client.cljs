(ns nvimgui.client
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer (go go-loop)])
  (:require
   [cljs.core.async :as async :refer (<! >! put! chan)]
   [clojure.edn :as edn]
   [goog.dom :as dom]
   [nvimgui.gui-events :as gui]
   [nvimgui.gui-grid :as grid]
   [nvimgui.cursor :as cursor]
   [nvimgui.keyboard :as kbd]
  ))

(def ?csrf-token
  (when-let [el (.getElementById js/document "sente-csrf-token")]
    (.getAttribute el "data-csrf-token")))



(defonce conn 
  (js/WebSocket. "ws://localhost:7778/ws"))

(defn send-msg [msg]
  (.send conn (js/JSON.stringify msg)))


(set! (.-onopen conn)
      (fn [e]
        (js/console.log "OPEN!!!" conn)
        (let [[w h] (grid/measure-grid)]
          (send-msg #js ["init" #js {:w w :h h}] ))))

(set! (.-onerror conn) 
  (fn []
    (js/alert "error")
    (.log js/console js/arguments)))

(defn debug-grid [rows]
  (let [root-el (.getElementById js/document "app")
        grid (dom/createDom "pre" #js {:class "grid-container"})
        ks (sort (keys rows))]

    (dom/removeChildren root-el)
    (doseq [i ks
            :let [row (get rows i)
                  row-cont (dom/createDom "div" #js {:class "row-container"
                                                     :data-row-idx i})]]
      (set! (.-innerHTML row-cont) row)
      (dom/append grid row-cont))
    (dom/append root-el grid)))

(defn- extract-default-colors [payload]
  (-> payload
      first
      (js->clj :keywordize-keys true)
      :default-colors))

(defn- extract-highlights [payload]
  (-> payload
      first
      (js->clj)
      (get "highlights")))

(defn- extract-resize [payload]
  (-> payload
      first
      (js->clj)
      (get "grid-resize")
      seq))

(defn- extract-clear [payload]
  (-> payload
      first
      (js->clj)
      (get "grid-clear")))

(defn- extract-grid-lines [payload]
  (-> payload
      first
      (js->clj)
      (get "grid-lines")))

(defn- extract-grid-cursor [payload]
  (-> payload
      first
      (js->clj)
      (get "cursor")))

(defn- extract-mode-info [payload]
  (-> payload
      first
      (js->clj)))

(defn- extract-mode-change [payload]
  (-> payload
      first
      (js->clj)))

(defn raf [f]
  (js/requestAnimationFrame f))

(defn receive [type payload]
  ;(js/console.log "TYPE" type "PAULOAD" payload)
  (case type
    nil nil
    "option_set" nil
    "busy_start" (grid/set-busy true)
    "busy_stop" (grid/set-busy false)
    "default_colors_set" (grid/install-default-colors 
                           (extract-default-colors payload))
    "hl_attr_define" (grid/install-sheets
                       (extract-highlights payload))
    "hl_group_set" nil
    "grid_resize" (doseq [op (extract-resize payload)] 
                    (grid/resize-grid op))
    "grid_clear" (doseq [op (extract-clear payload)]
                   (grid/grid-clear op))
    "grid_line" (raf #(doseq [op (extract-grid-lines payload)]
                        (grid/draw-lines op)))
    ;; eqiuvalent to grid lines for now
    "grid_scroll" (raf #(doseq [op (extract-grid-lines payload)]
                          (grid/draw-lines op)))
    "flush" nil
    "grid_cursor_goto" (cursor/set-cursor (extract-grid-cursor payload) @grid/grids)
    "mode_info_set" (cursor/set-mode-info (extract-mode-info payload))
    "mode_change" (cursor/mode-change (extract-mode-change payload))
    (js/console.log "unknown type" type payload)
    ))

(set! (.-onmessage conn)
  (fn [e]
    (let [ops (js/JSON.parse (.-data e))]
      (doseq [[type payload] ops]
        (receive type payload))
      )))

(defn send-keys [k]
  (send-msg #js ["key" k]))

;; start is called after code reloading finishes
(defn ^:dev/after-load start []
  (js/console.log "start")
  (kbd/attach-handler send-keys))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (js/console.log "init")
  (kbd/attach-handler send-keys)
  )

;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (js/console.log "stop")
  (kbd/remove-handler))
