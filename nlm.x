/* $Id: nlm.x,v 1.1 2006/01/31 15:53:15 dancy Exp $ */

/* Modifications made by Franz Inc. */

/* The maximum length of the string identifying the caller. */
/* const LM_MAXSTRLEN = 1024; */

/* The maximum number of bytes in the nlm_notify name argument. */
/* const LM_MAXNAMELEN = 1+LM_MAXSTRLEN; */

/* const MAXNETOBJ_SZ = 1024; */

typedef opaque netobj<MAXNETOBJ_SZ>;

enum nlm_stats {
    LCK_GRANTED = 0,
    LCK_DENIED = 1,
    LCK_DENIED_NOLOCKS = 2,
    LCK_BLOCKED = 3,
    LCK_DENIED_GRACE_PERIOD = 4
};

struct nlm_stat {
    nlm_stats stat;
};

struct nlm_res {
    netobj cookie;
    nlm_stat stat;
};

struct nlm4_res {
    netobj cookie;
    nlm_stat stat;
};

struct nlm_holder {
    bool exclusive;
    int uppid;
    netobj oh;
    unsigned l_offset;
    unsigned l_len;
};


union nlm_testrply switch (nlm_stats stat) {
    case LCK_DENIED:
        struct nlm_holder holder;    /*  holder of the lock */
    default:
        void;
};

union nlm4_testrply switch (nlm4_stats stat) {
    case LCK_DENIED:
        struct nlm4_holder holder;    /*  holder of the lock */
    default:
        void;
};

struct nlm_testres {
    netobj cookie;
    nlm_testrply test_stat;
};

struct nlm4_testres {
    netobj cookie;
    nlm_testrply test_stat;
};

struct nlm_lock {
    string caller_name<LM_MAXSTRLEN>;
    fhandle fh;         /*  identify a file  */
    netobj oh;         /*  identify owner of a lock  */
    int uppid;         /*  Unique process identifier  */
    unsigned l_offset; /*  File offset (for record locking) */
    unsigned l_len;    /*  Length (size of record)  */
};

struct nlm_lockargs {
    netobj cookie;
    bool block;            /*  Flag to indicate blocking behaviour. */
    bool exclusive;        /*  If exclusive access is desired. */
    struct nlm_lock alock; /*  The actual lock data (see above) */
    bool reclaim;          /*  used for recovering locks  */
    int state;             /*  specify local NSM state  */
};

struct nlm4_lockargs {
    netobj cookie;
    bool block;            /*  Flag to indicate blocking behaviour. */
    bool exclusive;        /*  If exclusive access is desired. */
    struct nlm4_lock alock; /*  The actual lock data (see above) */
    bool reclaim;          /*  used for recovering locks  */
    int state;             /*  specify local NSM state  */
};

struct nlm_cancargs {
    netobj cookie;        
    bool block;
    bool exclusive;
    struct nlm_lock alock;
};

struct nlm4_cancargs {
    netobj cookie;        
    bool block;
    bool exclusive;
    struct nlm4_lock alock;
};


struct nlm_testargs {
    netobj cookie;        
    bool exclusive;
    struct nlm_lock alock;
};

struct nlm4_testargs {
    netobj cookie;        
    bool exclusive;
    struct nlm4_lock alock;
};


struct nlm_unlockargs {
    netobj cookie;        
    struct nlm_lock alock;
};

struct nlm_unlockargs {
    netobj cookie;        
    struct nlm4_lock alock;
};

enum fsh_mode {
    fsm_DN = 0,        /*  deny none  */
    fsm_DR = 1,        /*  deny read  */
    fsm_DW = 2,        /*  deny write  */
    fsm_DRW = 3        /*  deny read/write  */
};

enum fsh_access {
    fsa_NONE = 0,     /*  for completeness  */
    fsa_R = 1,        /*  read-only  */
    fsa_W = 2,        /*  write-only  */
    fsa_RW = 3        /*  read/write  */
};

struct nlm_share {
    string caller_name<LM_MAXSTRLEN>;
    fhandle fh;
    netobj oh;
    fsh_mode mode;
    fsh_access access;
};

struct nlm_shareargs {
    netobj cookie;
    nlm_share share;     /*  actual share data  */
    bool reclaim;        /*  used for recovering shares  */
};

struct nlm_shareres {
    netobj cookie;
    nlm_stats stat;
    int sequence;
};

struct nlm_notify {
    string name<LM_MAXNAMELEN>;
    long state;
};

struct nlm4_notify {
    string name<LM_MAXNAMELEN>;
    long state;
};

enum nlm4_stats {
   NLM4_GRANTED = 0,
   NLM4_DENIED = 1,
   NLM4_DENIED_NOLOCKS = 2,
   NLM4_BLOCKED = 3,
   NLM4_DENIED_GRACE_PERIOD = 4,
   NLM4_DEADLCK = 5,
   NLM4_ROFS = 6,
   NLM4_STALE_FH = 7,
   NLM4_FBIG = 8,
   NLM4_FAILED = 9
};

struct nlm4_holder {
     bool     exclusive;
     int32    svid;
     netobj   oh;
     uint64   l_offset;
     uint64   l_len;
};

struct nlm4_lock {
     string   caller_name<LM_MAXSTRLEN>;
     fhandle   fh;
     netobj   oh;
     int32    svid;
     uint64   l_offset;
     uint64   l_len;
};

struct nlm4_share {
     string      caller_name<LM_MAXSTRLEN>;
     fhandle      fh;
     netobj      oh;
     fsh4_mode   mode;
     fsh4_access access;
};

struct nlm4_lockargs {
    netobj cookie;
    bool block;            /*  Flag to indicate blocking behaviour. */
    bool exclusive;        /*  If exclusive access is desired. */
    struct nlm4_lock alock; /*  The actual lock data (see above) */
    bool reclaim;          /*  used for recovering locks  */
    int state;             /*  specify local NSM state  */
};


program NLM_PROG {
    version NLM_VERSX {
        /*
         *  synchronous procedures
         */
        void         NLM_NULL(void) = 0;
        nlm_testres  NLM_TEST(struct nlm_testargs) = 1;
        nlm_res      NLM_LOCK(struct nlm_lockargs) = 2;
        nlm_res      NLM_CANCEL(struct nlm_cancargs) = 3;
        nlm_res      NLM_UNLOCK(struct nlm_unlockargs) = 4;

        /*
         *  server   NLM call-back procedure to grant lock
         */
        nlm_res      NLM_GRANTED(struct nlm_testargs) = 5;

        /*
         *  asynchronous requests and responses
         */
        void         NLM_TEST_MSG(struct nlm_testargs) = 6;
        void         NLM_LOCK_MSG(struct nlm_lockargs) = 7;
        void         NLM_CANCEL_MSG(struct nlm_cancargs) =8;
        void         NLM_UNLOCK_MSG(struct nlm_unlockargs) = 9;
        void         NLM_GRANTED_MSG(struct nlm_testargs) = 10;
        void         NLM_TEST_RES(nlm_testres) = 11;
        void         NLM_LOCK_RES(nlm_res) = 12;
        void         NLM_CANCEL_RES(nlm_res) = 13;
        void         NLM_UNLOCK_RES(nlm_res) = 14;
        void         NLM_GRANTED_RES(nlm_res) = 15;

        /*
         *  synchronous non-monitored lock and DOS file-sharing
         *  procedures (not defined for version 1 and 2)
         */
        nlm_shareres NLM_SHARE(nlm_shareargs) = 20;
        nlm_shareres NLM_UNSHARE(nlm_shareargs) = 21;
        nlm_res      NLM_NM_LOCK(nlm_lockargs) = 22;
        void         NLM_FREE_ALL(nlm_notify) = 23;
    } = 3;
       version NLM4_VERS {
         void
            NLMPROC4_NULL(void)                  = 0;

         nlm4_testres
            NLMPROC4_TEST(nlm4_testargs)         = 1;

         nlm4_res
            NLMPROC4_LOCK(nlm4_lockargs)         = 2;

         nlm4_res
            NLMPROC4_CANCEL(nlm4_cancargs)       = 3;

         nlm4_res
            NLMPROC4_UNLOCK(nlm4_unlockargs)     = 4;

         nlm4_res
            NLMPROC4_GRANTED(nlm4_testargs)      = 5;

         void
            NLMPROC4_TEST_MSG(nlm4_testargs)     = 6;

         void
            NLMPROC4_LOCK_MSG(nlm4_lockargs)     = 7;

         void
            NLMPROC4_CANCEL_MSG(nlm4_cancargs)   = 8;

         void
            NLMPROC4_UNLOCK_MSG(nlm4_unlockargs) = 9;

         void
            NLMPROC4_GRANTED_MSG(nlm4_testargs) = 10;

         void
            NLMPROC4_TEST_RES(nlm4_testres)     = 11;

         void
            NLMPROC4_LOCK_RES(nlm4_res)         = 12;

         void
            NLMPROC4_CANCEL_RES(nlm4_res)       = 13;

         void
            NLMPROC4_UNLOCK_RES(nlm4_res)       = 14;

         void
            NLMPROC4_GRANTED_RES(nlm4_res)      = 15;

         nlm4_shareres
            NLMPROC4_SHARE(nlm4_shareargs)      = 20;

         nlm4_shareres
            NLMPROC4_UNSHARE(nlm4_shareargs)    = 21;

         nlm4_res
            NLMPROC4_NM_LOCK(nlm4_lockargs)     = 22;

         void
            NLMPROC4_FREE_ALL(nlm4_notify)      = 23;

      } = 4;
} = 100021;


