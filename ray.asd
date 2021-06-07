(defsystem "ray"
  :version "0.1.0"
  :author "davelpz@gmail.com"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "ray/tests"))))

(defsystem "ray/tests"
  :author ""
  :license ""
  :depends-on ("ray"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for ray"
  :perform (test-op (op c) (symbol-call :rove :run c)))
