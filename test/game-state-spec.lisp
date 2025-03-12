(in-package :galaxians-spec)

(def-suite* game-state-spec)

(test simple-test
      (let ((game-state (make-instance 'g::game-state)))
        (is (= 100 (g::x (g::player-state game-state))))))
