(defsystem #:named-read-macros-test
  :author "William Yao <williamyaoh@gmail.com>"
  :maintainer "William Yao <williamyaoh@gmail.com>"
  :depends-on ("fiveam"
               "asdf"
               "uiop"

               "named-read-macros")
  :serial t
  :components ((:file "named-read-macros-test"))
  :perform (test-op (o s)
            (uiop:symbol-call :fiveam '#:run!
               (uiop:find-symbol* '#:named-read-macros-tests
                                  '#:named-read-macros-test))))
