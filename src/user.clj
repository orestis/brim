(ns user
  (:require [clojure.java.io :as io]
            [msgpack.core :as msg]
            msgpack.clojure-extensions
            [nvimgui.core :as gui])
  (:import [java.io StringWriter]
           [java.net Socket]))


(defn handle-redraw [events]
  (gui/process-redraw-events events)
  #_
  (doseq [arg params]
    (println "RRR " arg)))

(defn handle-response [id error result]
  (if error
    (print "ERROR> " id error)
    (println "RESP> " id result)))

(defn handle-notification [method & params]
  (case method
    "redraw" (handle-redraw (first params))
    (println "NOTE> " method params)))

(defn start-observing [is]
  (future 
    (println ">>> observing is")
    (while true
      (println ">>> waiting for message...")
      (let [[type & args] (msg/unpack is)]
        (case type
          1 (apply handle-response args)
          2 (apply handle-notification args)
          (println ">>> UNKNOWN MSG" type args))))
    (println "stopped observing is"))
  )


(comment 
(send-off-command OS "nvim_command" "echo \"foo\"")
(send-off-command OS "nvim_buf_attach" 0 true {})
(send-off-command OS "nvim_buf_detach" 0)
(send-off-command OS "nvim_ui_detach")
(send-off-command OS "nvim_ui_attach" 40 40 {"ext_linegrid" true
                                             "rgb" false}))
