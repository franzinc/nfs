#if defined(__CYGWIN__) || defined(__APPLE__)

int xdr_uint64_t(a, b) {
  xdr_u_int64_t(a, b);
}
#endif
