;; $Id: extra.cl,v 1.3 2001/05/23 16:51:40 layer Exp $

(in-package :user)

(load "extra.dll")

(ff:def-foreign-call (truncate-file "truncate")
    ((filename (* :char)) (size :int))
  :strings-convert nil)
