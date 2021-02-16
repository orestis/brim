(ns nvimgui.keyboard)


;; The problem:
;; keypress is a nice event in general but modifier keys are an issue
;; because eg you press shift-9 to get (, and the `key` code is correctly
;; ( but shift is also on, so you send vim <S-(> which is wrong.

;; The idea is then to use key-press for some special keys such as:
;; * Tab, Enter, Arrows, F1 etc
;; * everything with Ctrl ?
;; * what else?
;; and then everything else, send to an input field to be interpreted
;; by the OS as needed, then send that single character there

;; ideally we'd let the OS first filter all input stuff,
;; together with composition events and so on, so we didn't
;; need to have custom rules which are bound to be slightly off
;; the problem is that keypress is fired before input
;; so at that moment two things can happen:
;; 1. OS decides this is not an input event, so input doesn't fire
;;    which means we need to dispatch it
;; 2. OS decides this is an input event, so input fires which
;;    means we need to check to see if something meaningful appears
;;    in input, and then either fire that or send off the original event.


(defonce input-kbd (.getElementById js/document "keyboard"))
(defonce debug-kbd (.getElementById js/document "keycode"))


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
   "<" "LT"})

(defn key->code [k modifiers]
  (let [sp (get special-keys k)]
    (if (or sp (not= "" modifiers))
      (str "<" modifiers (or sp k) ">")
      k)))

;; we might be able to use queueMicrotask to create an algorithm like so:
;; 1. onkeypress -> extract all the info needed such as modifiers and key
;; 2. store that info in a global variable "last keypress"
;; 3. Clear the input kbd
;; 3. queue a microtask

(defonce last-key-press (atom nil))
(defonce last-value (atom nil))

(defn dispatch-keycode [f keycode]
  (set! (.-innerText debug-kbd) (pr-str keycode))
  (when (string? keycode)
    (f keycode)))

(defonce NNN (atom 0))

(defn dispatch-proper-key [f]
  (let [iv @last-value
        lkp @last-key-press
        {:keys [shift ctrl alt meta key]} lkp
        modifiers (str (when shift "S-") (when ctrl "C-") (when (or alt meta) "M-"))
        no-input (or (= "" iv)
                     (nil? iv))
        dead-key (= "Dead" (:key lkp))]
    (cond 
      (not no-input)
      (dispatch-keycode f (key->code iv ""))

      (and no-input dead-key)
      (js/console.log "ignoring dead event, waiting for compositionn to finish")

      :else
      (dispatch-keycode f (key->code key modifiers))

      )
    (reset! last-key-press nil)
    (reset! last-value nil)))

(def keys-prevent-default 
  #{"Tab" "Escape" "Enter"})

(defn key-listener2 [f e]
  (let [k (.-key e)
        shift (.-shiftKey e)
        ctrl (.-ctrlKey e)
        alt (.-altKey e)
        meta (.-metaKey e)]
    (set! (.-value input-kbd) "")
    (reset! last-key-press {:key k :shift shift
                            :ctrl ctrl :alt alt
                            :meta meta})
    (js/setTimeout #(dispatch-proper-key f))
    (when (keys-prevent-default k)
      (.preventDefault e))))

(defn key-listener [f e]
  (when (special-keys (.-key e))
    (.preventDefault e))
  (let [k (.-key e)
        shift (.-shiftKey e)
        ctrl (.-ctrlKey e)
        alt (.-altKey e)
        meta (.-metaKey e)]
    (when-not (ignored-keys k)
      (let [modifiers (str (when shift "S-") (when ctrl "C-") (when (or alt meta) "M-"))]
        (f (key->code k modifiers))))))

(defn on-input [e]
  (let [v (-> e .-target .-value)]
    (when-not (.-isComposing e)
      (reset! last-value v))))
(defonce -add-listener (delay (.addEventListener input-kbd "input" on-input true)))
(deref -add-listener)




(defonce listener (atom nil))
(defn attach-handler [f]
  (.focus input-kbd)
  (let [l (partial key-listener2 f)]
    (reset! listener l)
    (.addEventListener input-kbd
                       "keydown" l #js {:capture false})))

(defn remove-handler []
  (when @listener
    (.removeEventListener input-kbd
                          "keydown" @listener #js {:capture false})))
