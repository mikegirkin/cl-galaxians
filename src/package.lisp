(defpackage :galaxians
  (:use :cl :binding-arrows :let-plus
        :geometry)
  (:import-from :alexandria :clamp :define-constant :format-symbol
                :make-keyword :non-negative-fixnum)
  (:export :main))
