(ns nvimgui.client
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer (go go-loop)])
  (:require
   [cljs.core.async :as async :refer (<! >! put! chan)]
   [clojure.edn :as edn]
   [goog.dom :as dom]
   [nvimgui.gui-events :as gui]
   [nvimgui.gui-grid :as grid]
   [nvimgui.keyboard :as kbd]
  ))

(def ?csrf-token
  (when-let [el (.getElementById js/document "sente-csrf-token")]
    (.getAttribute el "data-csrf-token")))



(defonce conn 
  (js/WebSocket. "ws://localhost:7778/ws"))

(defn send-msg [msg]
  (.send conn
         (pr-str msg)))

(set! (.-onopen conn)
  (fn [e]
    (js/console.log "OPEN!!!" conn)
    (send-msg [:init])))

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

(defn receive [[type payload]]
  (case type
    :nvim/default-colors (grid/install-default-colors payload)
    :nvim/highlights (grid/install-sheets payload)
    :nvim/debug (js/console.log "DEBUG" payload)
    :nvim/debug-grid (debug-grid payload)))

(set! (.-onmessage conn)
  (fn [e]
    (let [msgs (edn/read-string (.-data e))]
      (js/console.log "received" msgs)
      (receive msgs))))

(defn send-keys [k]
  (js/console.log "sending keycode" k)
  (send-msg [:nvim/key k]))

;; start is called after code reloading finishes
(defn ^:dev/after-load start []
  (js/console.log "start")
  (kbd/attach-handler send-keys)
  (grid/draw-grid @gui/gui-state))

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
