(ns nes.display
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]))

(def my-canvas (c2d/canvas 600 600))

(def window (c2d/show-window my-canvas "Hello World"))


(defn example-01
  "Create 2 windows and attach event methods"
  []
  (let [name1 "first window"
        name2 "secodn window"
        frame1 (c2d/show-window (c2d/canvas 1 1) name1 400 400 25)
        _ (c2d/show-window (c2d/canvas 100 100) name2 400 200 10)]
    (defmethod c2d/key-pressed [name1 \space] [_ _]
      (println (str "Window: " name1)))
    (defmethod c2d/key-pressed [name2 \space] [_ _]
      (println (str "Window: " name2)))
    (defmethod c2d/mouse-event [name1 :mouse-pressed] [e _]
      (let [c (c2d/canvas 1 1)
            x (c2d/mouse-x e)
            y (c2d/mouse-y e)
            cr (m/cnorm x 0 399 0 255)
            cg (m/cnorm y 0 399 0 255)]
        (c2d/with-canvas-> c
          (c2d/set-background cr cg 128))
        (c2d/replace-canvas frame1 c)))
    nil))

(example-01)



(defn draw-fn
  "Draw events on screen"
  [c w _ _]
  (if (c2d/mouse-pressed? w)
    (c2d/set-color c :white)
    (c2d/set-color c :darkgray))
  (c2d/rect c 100 100 100 100)
  (if (c2d/key-pressed? w)
    (do 
      (c2d/set-color c :white)
      (when (= :space (c2d/key-code w)) (println @(:events w))))
    (c2d/set-color c :darkgray))
  (c2d/rect c 300 100 100 100)
  (c2d/set-color c :black)
  (c2d/text c "Mouse" 110 120)
  (c2d/text c "Key" 310 120))

(def window (c2d/show-window {:canvas (c2d/canvas 500 300)
                              :draw-fn draw-fn
                              :fps 30
                              :window-name "In-loop events"}))
  
  
     
