(load "../../../../poor-man-module.scm")
(load-module "../../../../libs/" "r5rs")
(load-module "../../../../libs/" "pmatch")
(load-module "../../../../libs/" "sllgen")
(load-module "../../../../libs/" "define-datatype")
(load-module "../../../../libs/" "test-harness")

(load-module "../../" "letcc-print-set!")

(cps '(lambda (x) x))
