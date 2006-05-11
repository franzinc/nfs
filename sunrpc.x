/* $Id: sunrpc.x,v 1.1 2006/05/11 21:58:59 dancy Exp $ */

/* Extracted from rfc1057 (plus some tweaks) */

	const TRUE = 1;
	const FALSE = 0;

         enum auth_flavor {
            AUTH_NULL       = 0,
            AUTH_UNIX       = 1,
            AUTH_SHORT      = 2,
            AUTH_DES        = 3
            /* and more to be defined */
         };

         struct opaque_auth {
            auth_flavor flavor;
            opaque body<400>;
         };

         enum msg_type {
            CALL  = 0,
            REPLY = 1
         };

         enum reply_stat {
            MSG_ACCEPTED = 0,
            MSG_DENIED   = 1
         };

         enum accept_stat {
            SUCCESS       = 0, /* RPC executed successfully       */
            PROG_UNAVAIL  = 1, /* remote hasn't exported program  */
            PROG_MISMATCH = 2, /* remote can't support version #  */
            PROC_UNAVAIL  = 3, /* program can't support procedure */
            GARBAGE_ARGS  = 4  /* procedure can't decode params   */
         };

         enum reject_stat {
            RPC_MISMATCH = 0, /* RPC version number != 2          */
            AUTH_ERROR = 1    /* remote can't authenticate caller */
         };

         enum auth_stat {
            AUTH_BADCRED      = 1,  /* bad credentials (seal broken) */
            AUTH_REJECTEDCRED = 2,  /* client must begin new session */
            AUTH_BADVERF      = 3,  /* bad verifier (seal broken)    */
            AUTH_REJECTEDVERF = 4,  /* verifier expired or replayed  */
            AUTH_TOOWEAK      = 5   /* rejected for security reasons */
         };

         struct rpc_msg {
            unsigned int xid;
            union switch (msg_type mtype) {
            case CALL:
               call_body cbody;
            case REPLY:
               reply_body rbody;
            } body;
         };

         struct call_body {
            unsigned int rpcvers;       /* must be equal to two (2) */
            unsigned int prog;
            unsigned int vers;
            unsigned int proc;
            opaque_auth cred;
            opaque_auth verf;
            /* procedure specific parameters start here */
         };

         union reply_body switch (reply_stat stat) {
         case MSG_ACCEPTED:
            accepted_reply areply;
         case MSG_DENIED:
            rejected_reply rreply;
         };

         struct accepted_reply {
            opaque_auth verf;
            union switch (accept_stat stat) {
            case SUCCESS:
               /* opaque results[0]; */
               xdr results;
               /*
                * procedure-specific results start here
                */
             case PROG_MISMATCH:
                struct {
                   unsigned int low;
                   unsigned int high;
                } mismatch_info;
             default:
                /*
                 * Void.  Cases include PROG_UNAVAIL, PROC_UNAVAIL,
                 * and GARBAGE_ARGS.
                 */
                void;
             } reply_data;
         };

         union rejected_reply switch (reject_stat status) {
         case RPC_MISMATCH:
            struct {
               unsigned int low;
               unsigned int high;
            } mismatch_info;
         case AUTH_ERROR:
            auth_stat stat;
         };

         struct auth_unix {
            unsigned int stamp;
            string machinename<255>;
            unsigned int uid;
            unsigned int gid;
            unsigned int gids<16>;
         };

