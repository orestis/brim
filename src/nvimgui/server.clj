(ns nvimgui.server
  (:require [taoensso.sente :as sente]
            [org.httpkit.server :refer [run-server]]
            [ring.middleware.params]
            [ring.middleware.keyword-params]
            [ring.middleware.session]
            [ring.middleware.anti-forgery]
            [ring.middleware.resource]
            [clojure.core.async :as a]
            [nvimgui.nvim :as nvim]
            [taoensso.sente.server-adapters.http-kit :refer (get-sch-adapter)]))

(let [{:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
      (sente/make-channel-socket! (get-sch-adapter) {})]

  (def ring-ajax-post                ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk                       ch-recv) ; ChannelSocket's receive channel
  (def chsk-send!                    send-fn) ; ChannelSocket's send API fn
  (def connected-uids                connected-uids) ; Watchable, read-only atom
  )

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
             <script>nvimgui.client.start()</script>
             </body>
             </html>

             ")]
    {:status 200
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body body}))

(defn routes [{:keys [uri request-method] :as req}]
  (case [uri request-method]
    ["/" :get] (index req)
    ["/chsk" :get] (ring-ajax-get-or-ws-handshake req)
    ["/chsk" :post] (ring-ajax-post req)
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

(defn pipe-events-to-ws [ic]
  (a/thread
    (loop []
      (tap> "reading from ic")
      (when-let [msg (a/<!! ic)]
        (tap> ["sendinng msg to ws" msg])
        (chsk-send! :sente/all-users-without-uid 
                    [:nvim/raw msg])
        (tap> "sent!")
        (recur)))))

(defn start-server [conn]
  (let [ic (:input-chan conn)]
    (nvim/start! conn)
    (pipe-events-to-ws ic)
    (run-server my-app {:port 7778})))


(comment
  (def CONN (nvim/nvim-conn (nvim/connect-to-nvim "127.0.0.1" 7777)
                            (a/chan 5) (a/chan 5)))
  (def SERVER (start-server CONN))
  (SERVER)
  (add-tap println)
  ;; should send to all clients the HI
  (a/put! (:input-chan CONN) [2 "my" "days"])
  (a/poll! (:output-chan CONN))
  (nvim/send-off-command (:output-chan CONN)
                         "nvim_ui_detach")
  (nvim/send-off-command (:output-chan CONN)
                         "nvim_ui_attach" 40 40
                         {"ext_linegrid" true
                          "rgb" true})
  )
