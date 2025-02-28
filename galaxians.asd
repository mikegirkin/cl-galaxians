(asdf:defsystem "galaxians"
    :author "Mike V Girkin"
    :license "MIT"
    :serial t
    :components
    ((:module "src"
              :components
              ((:file "package")
               (:file "game-state")
               (:file "render")
               (:file "main"))))
    :depends-on (#:alexandria
                 #:cl-liballegro
                 #:livesupport)
  :entry-point "galaxians:main")
