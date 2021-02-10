(ns nvimgui.keyboard)


(def ignored-keys
  #{"Alt" "Control" "Shift" "Meta"
    "AltGraph" "CapsLock" "Fn"
    "FnLock" "Hyper" "NumLock" "ScrollLock"
    "Super" "Symbol" "SymbolLock"})

(def special-keys 
  {"Enter" "CR"
   "Tab" "Tab"
   "Escape" "Esc"
   " " "Space"
   "Backspace" "BS"
   "ArrowUp" "Up"
   "ArrowDown" "Down"
   "ArrowLeft" "Left"
   "ArrowRight" "Right"
   "<" "LT"
   })


(defn key->code [k modifiers]
  (let [sp (get special-keys k)]
    (if (or sp (not= "" modifiers))
      (str "<" modifiers (or sp k) ">")
      k)))

(defn key-listener [f e]
  (.preventDefault e)
  (let [k (.-key e)
        shift (.-shiftKey e)
        ctrl (.-ctrlKey e)
        alt (.-altKey e)
        meta (.-metaKey e)]
    (when-not (ignored-keys k)
      (let [modifiers (str (when shift "S-") (when ctrl "C-") (when (or alt meta) "M-"))]
        (f (key->code k modifiers))))))

(defonce listener (atom nil))
(defn attach-handler [f]
  (let [l (partial key-listener f)]
    (reset! listener l)
    (.addEventListener js/document
                       "keydown" l)))

(defn remove-handler []
  (when @listener
    (.removeEventListener js/document
                          "keydown" @listener)))
