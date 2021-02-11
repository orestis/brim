(ns nvimgui.server
  (:require 
            [org.httpkit.server :refer [run-server] :as http-kit]
            [ring.middleware.params]
            [ring.middleware.keyword-params]
            [ring.middleware.session]
            [ring.middleware.anti-forgery]
            [ring.middleware.resource]
            [clojure.core.async :as a]
            [nvimgui.server-grid :as sg]
            [nvimgui.nvim :as nvim]))


(defn index [req]
  (let [csrf-token (force ring.middleware.anti-forgery/*anti-forgery-token*)
        body 
        (str "
             <!DOCTYPE html>
             <html>
             <head>
             <title>!!ααψNeovim</title>
             <link rel='stylesheet' href='/css/main.css'>
             <link rel=\"icon\" href=\"data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>🎯</text></svg>\">
             "
             "</head> <body>"
             "<div id='sente-csrf-token' data-csrf-token='" csrf-token "'></div> "

             "
             <div id='app'>app here</div>
             <script src='/js/main.js'></script>
             <script>nvimgui.client.init()</script>
             </body>
             </html>

             ")]
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body body}))

(defonce editors (atom {}))
#_
(reset! editors {})

(defn connect-nvim-to-server-grid [ch ic]
  (a/thread
    (loop []
      (when-let [msg (a/<!! ic)]
        ;(println "received from nvim " msg)
        (when-let [{:keys [ui]} (get @editors ch)]
          (println "found editor for channel")
          (let [{type ::nvim/type
                 method :method
                 params :params
                 :as decoded} (nvim/decode-event msg)]
            (println decoded "event type" type)
            (cond
              (= type ::nvim/response)
              (println "response>>> " decoded)
              (and (= type ::nvim/notification)
                   (= method "redraw"))
              (let [new-ui (sg/process-redraw ui (first params))
                    grid (sg/html-grid (get-in new-ui [:grids 1]))]
                (http-kit/send! ch (pr-str [:nvim/highlights
                                            (get-in new-ui [:highlights])]))
                (http-kit/send! ch (pr-str [:nvim/default-colors
                                           (get-in new-ui [:default-colors])]))
                (http-kit/send! ch (pr-str [:nvim/debug-grid grid]))
                (swap! editors assoc-in [ch :ui] new-ui)))))
        (recur)))))

(defn attach-ui [conn {:keys [w h]
                       :or {w 120
                            h 50}}]
  (nvim/send-off-command (:output-chan conn) "nvim_ui_detach")
  (nvim/send-off-command (:output-chan conn)
                         "nvim_ui_attach" w h
                         {"ext_linegrid" true
                          "rgb" true}) )

(defn init-editor [ch]
  (let [conn (nvim/nvim-conn 
                          (nvim/connect-to-nvim "127.0.0.1" 7777)
                          (a/chan 5) (a/chan 5))
        ic (:input-chan conn)
        ui (sg/create-editor-ui)]
    (nvim/start! conn)
    (connect-nvim-to-server-grid ch ic)
    (attach-ui conn {})
  {:conn conn
   :ui ui}))


(defn destroy-editor [{:keys [conn ui]}]
  (nvim/stop! conn))

(defn on-receive [ch msg]
  (println "RECeIVED" ch msg)
  #_
  (when-let [{:keys [input-chan output-chan]} (get @connected ch)]
                   ))


(defn send-keys-to-nvim [websocket oc]
  (a/thread
    (loop []
      (tap> "reading from websocket")
      (when-let [msg (a/<!! websocket)]
        (tap> "got msg from websocket")
        (let [{:keys [id ?data]} msg]
          (case id
            :nvim/key (nvim/send-off-command oc
                        "nvim_input" ?data)
            (tap> (str "unknown id" id))))
        (recur)))))

(defn ws-handler [request]
  (http-kit/as-channel 
    request
    {:init (fn [ch]
             (println "init channel!")
             (swap! editors assoc ch (init-editor ch)))
     :on-open (fn [ch]
                (println "channel open")
                (http-kit/send! ch (pr-str [:nvim/debug "open for business"])))
     :on-close (fn [ch status]
                 (println "channel closed" status)
                 (when-let [editor (get @editors ch)]
                   (destroy-editor editor)
                   (swap! editors dissoc ch)))
     :on-ping (fn [ch msg]
                (println "PING" msg))
     :on-receive #'on-receive}))

(defn routes [{:keys [uri request-method] :as req}]
  (case [uri request-method]
    ["/" :get] (index req)
    ["/ws" :get] (ws-handler req)
    {:status 404
     :headers {}
     :body (str "not found" uri " - " request-method)}))

(def my-app
  (-> routes
      (ring.middleware.resource/wrap-resource "public")
      ring.middleware.keyword-params/wrap-keyword-params
      ring.middleware.params/wrap-params
      ring.middleware.anti-forgery/wrap-anti-forgery
      ring.middleware.session/wrap-session))

;(defn pipe-events-to-ws [ic]
;  (a/thread
;    (loop []
;      (when-let [msg (a/<!! ic)]
;        ;(tap> ["sendinng msg to ws" msg])
;        #_
;        (chsk-send! :sente/all-users-without-uid 
;                    [:nvim/raw msg])
;        (tap> "sent!")
;        (recur)))))


(defn start-server []
  (let []
    (run-server my-app {:port 7778})))


(comment
  (def CONN (nvim/nvim-conn (nvim/connect-to-nvim "127.0.0.1" 7777)
                            (a/chan 5) (a/chan 5)))
  (def SERVER (start-server))
  (SERVER)
  (add-tap println)
  (tap> "Test")
  ;; should send to all clients the HI
  (a/put! (:input-chan CONN) [2 "my" "days"])
  (a/poll! (:output-chan CONN))
  (nvim/send-off-command (:output-chan CONN)
                         "nvim_input"
                         "<Up>")
  (do
    (nvim/send-off-command (:output-chan CONN)
                           "nvim_ui_detach")
    (nvim/send-off-command (:output-chan CONN)
                           "nvim_ui_attach" 180 49
                           {"ext_linegrid" true
                            "rgb" true}))
  )
