;; $Id: extra.cl,v 1.4 2001/05/24 01:03:34 dancy Exp $

(in-package :user)

(load "extra.dll")

(ff:def-foreign-call (truncate-file "truncate")
    ((filename (* :char)) (size :int))
  :strings-convert t)
