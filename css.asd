(defsystem "css"
  :description "C-Structure-Ser/des"
  :version "0.0"
  :author "murry2018 <2clean8@naver.com>"
  :licence "MIT"
  :depends-on ("cl-tree-sitter" "uiop" "cffi" "str" "alexandria")
  :in-order-to ((test-op (test-op "css/tests")))
  :components ((:module "lib"
                :components ((:file "object")
                             (:file "string")))
               (:module "types"
                :depends-on ("lib")
                :serial t
                :components ((:file "package")
                             (:file "c-type")
                             (:file "primitive-type")
                             (:file "type-alias")
                             (:file "enum-type")
                             (:file "array-type")
                             (:file "struct-type")))))

(defsystem "css/tests"
  :depends-on ("css" "str" "fiveam" "alexandria" "cl-ppcre" "rutils")
  :components ((:module "tests"
                :serial t
                :components ((:file "test-all")
                             (:file "test-types")
                             (:file "test-lib"))))
  :perform (test-op (o c)
                    (symbol-call 'fiveam 'run!
                                 (find-symbol* 'all-tests 'css.tests))))
