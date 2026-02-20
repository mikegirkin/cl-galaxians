(asdf:defsystem "galaxians"
  :author "Mike V Girkin"
  :license "MIT"
  :serial t
  :components
  ((:module "src"
    :components ((:file "geometry/package")
                 (:file "geometry/geometry")
                 (:file "geometry/trajectory")
                 (:file "package")
                 (:file "seq-utils")
                 (:file "game-config")
                 (:file "game-state")
                 (:file "render")
                 (:file "main")))
   (:module "test"
    :components ((:file "package")
                 (:file "game-state-spec")
                 (:file "geometry/package")
                 (:file "geometry/trajectory-spec"))))

  :depends-on (:alexandria
               :cl-liballegro
               :livesupport
               :fiveam
               :binding-arrows
               :let-plus)

  :entry-point "galaxians:main")
