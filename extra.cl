;; $Id: extra.cl,v 1.2 2001/05/23 15:59:02 layer Exp $

(in-package :user)

(load "extra.dll")

(ff:def-foreign-call
    (truncate-file "truncate") ((filename (* :char)) (size :int)))
