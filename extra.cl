(load "extra.dll")

(ff:def-foreign-call
    (truncate-file "truncate") ((filename (* :char)) (size :int)))
