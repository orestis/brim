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


(comment
  (def IC (a/chan 5))
  (def OC (a/chan 5))
  (def CONN (nvim-conn (connect-to-nvim "127.0.0.1" 7777)
                       IC
                       OC))

  (start! CONN)
  (a/<!! IC)
  (a/poll! IC)
  
  (send-off-command OC "nvim_command" "echo \"foo\"")
  (a/poll! OC)
  )
