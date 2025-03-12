(asdf:defsystem "galaxians"
  :author "Mike V Girkin"
  :license "MIT"
  :serial t
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "game-state")
                 (:file "render")
                 (:file "main")))
   (:module "test"
    :components ((:file "package")
                 (:file "game-state-spec"))))

  :depends-on (:alexandria
               :cl-liballegro
               :livesupport
               :fiveam)

  :entry-point "galaxians:main")
