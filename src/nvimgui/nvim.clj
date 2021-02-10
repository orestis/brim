(ns nvimgui.nvim
  (:require [msgpack.core :as msg]
            [clojure.core.async :as a]
            msgpack.clojure-extensions)
  (:import [java.net Socket]))

;; Read from a socket input stream, write to the channel
;; read from a channel, write the output stream

(defn connect-to-nvim [host port]
  (let [sock (Socket. host port)]
    sock))

(defn nvim-conn [sock input-chan output-chan]
  {:sock sock
   :input-chan input-chan
   :output-chan output-chan})

(defn read-is [is]
  (try 
    (msg/unpack is)
    (catch Exception e
      e)))

(defn write-os [os msg]
  (try
    (msg/pack-stream msg os)
    (catch Exception e
      e)))

(defn start! [{:keys [sock input-chan output-chan]}]
  (let [is (.getInputStream sock)
        os (.getOutputStream sock)]
    ;; spawn off a thread to read the input stream, and
    ;; put the values into the input-chan
    ;; when the input chan is closed, stop
    ;; if an error occurs, put the error on the channel and
    ;; expect reader to handle it and close the input-channel
    (a/thread
      (loop []
        (let [msg (read-is is)]
          (when (a/put! input-chan msg)
            (recur)))))
    ;; 
    (a/thread
      (loop []
        (when-let [msg (a/<!! output-chan)]
          (if-let [error (write-os os msg)]
            (do 
              (a/put! output-chan error)
              (a/close! output-chan))
            (recur)))))))


(defonce ID-SEQ (atom 0))
(defn next-id []
  (swap! ID-SEQ inc))

(defn send-off-command [output-chan cmd & params]
  (a/>!! output-chan [0 (next-id) cmd (or params [])]))


(defn decode-response [id error result]
  {:id id
   ::type ::response
   :error error
   :result result})

(defn decode-notification [method & params]
  {:method method
   ::type ::notification
   :params params})

(defn decode-event [[type & rest]]
  (case type
    1 (apply decode-response rest)
    2 (apply decode-notification rest)))

(comment
  (do 
    (def CONN (nvim-conn (connect-to-nvim "127.0.0.1" 7777)
                         (a/chan 5) (a/chan 5)))

    (start! CONN)
    (def IC (:input-chan CONN))
    (def OC (:output-chan CONN)))
  (do
    (send-off-command (:output-chan CONN)
                           "nvim_ui_detach")
    (send-off-command (:output-chan CONN)
                           "nvim_ui_attach" 30 30
                           {"ext_linegrid" true
                            "rgb" true}))
  (a/<!! IC)
  (def last-even *1)
  (decode-event last-even)
  (require '[nvimgui.server-grid :as sg])
  (do
    (sg/process-redraw (sg/create-editor-ui)
                       (-> (decode-event last-even)
                           :params
                           first))
    nil)
  (a/poll! IC)
  
  (send-off-command OC "nvim_command" "echo \"foo\"")
  (a/poll! OC)
  )
