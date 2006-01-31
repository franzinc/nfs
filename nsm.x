/* $Id: nsm.x,v 1.1 2006/01/31 15:53:15 dancy Exp $ */

/*
 * This defines the maximum length of the string
 * identifying the caller.
 */
const SM_MAXSTRLEN = 1024;

struct sm_name {
    string mon_name<SM_MAXSTRLEN>;
};

enum res {
    STAT_SUCC = 0,   /*  NSM agrees to monitor.  */
    STAT_FAIL = 1    /*  NSM cannot monitor.  */
};

struct sm_stat_res {
    res    res_stat;
    int    state;
};

struct sm_stat {
    int    state;    /*  state number of NSM  */
};

struct my_id {
    string my_name<SM_MAXSTRLEN>;  /*  hostname  */
    int    my_prog;                /*  RPC program number  */
    int    my_vers;                /*  program version number  */
    int    my_proc;                /*  procedure number  */
};

struct mon_id {
    string mon_name<SM_MAXSTRLEN>; /* name of the host to be monitored */
    struct my_id my_id;
};

struct mon {
    struct mon_id mon_id;
    opaque    priv[16];        /*  private information  */
};

struct stat_chge {
    string    mon_name;
    int    state;
};

struct nsm_callback_status {
    string mon_name<SM_MAXSTRLEN>;
    int    state;
    opaque priv[16];        /*  for private information  */
};

/*
 *  Protocol description for the NSM program.
 */

program SM_PROG {
    version SM_VERS {
        void SM_NULL(void) = 0;
        struct sm_stat_res SM_STAT(struct sm_name) = 1;
        struct sm_stat_res SM_MON(struct mon) = 2;
        struct sm_stat SM_UNMON(struct mon_id) = 3;
        struct sm_stat SM_UNMON_ALL(struct my_id) = 4;    
        void SM_SIMU_CRASH(void) = 5;
        void SM_NOTIFY(struct stat_chge) = 6;
    } = 1;
} = 100024;

