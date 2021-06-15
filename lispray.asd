(defsystem "lispray"
  :version "0.1.0"
  :author "davelpz@gmail.com"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main") (:file "tuples"))))
  :description ""
  :in-order-to ((test-op (test-op "lispray/tests"))))

(defsystem "lispray/tests"
  :author ""
  :license ""
  :depends-on ("lispray"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main") (:file "tuples"))))
  :description "Test system for lispray"
  :perform (test-op (op c) (symbol-call :rove :run c)))
