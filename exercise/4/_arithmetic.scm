(load "_o-*.scm")
(load "_o--.scm")
(load "_o-div.scm")
(load "_o-+.scm")

(define install-arithmetic
  (lambda()
    (install-o-*)
    (install-o--)
    (install-o-div)
    (install-o-+)
    ))
