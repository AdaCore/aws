
.. _Status_page:

***********
Status page
***********
.. index:: Status

The status page gives information about the `AWS` internal status. For
example it returns the server socket ID, the number of simultaneous
connection, the number of time a connection has been used...

To display the information `AWS` use a template file. The
template file (default is |STATUS_PAGE|) is an `HTML` file with
some specific tags recognized by the parser. For more information
about how the template parser works, please look for the template
parser documentation distributed with `AWS`.

Here are the tag variables recognized by `AWS` status page:

*ABORTABLE_V (vector tag)*
  .. index:: ABORTABLE_V

  A list of boolean. One for each connection. True means that this
  connection can be aborted if none is available. This is to be inserted
  in a template table.

*ACCEPT_QUEUE_SIZE*
  .. index:: ACCEPT_QUEUE_SIZE

  see :ref:`Configuration_options`.

*ACCEPTOR_LENGTH*
  .. index:: ACCEPTOR_LENGTH

  Number of sockets in the internal socket set.

*ACTIVITY_COUNTER_V (vector tag)*
  .. index:: ACTIVITY_COUNTER_V

  A list of natural. One for each connection. This is the number of
  request the connection has answered. This counter is reset each time the
  connection is closed. In other word this is the number of request a
  keep-alive connection has processed.

*ACTIVITY_TIME_STAMP_V (vector tag)*
  .. index:: ACTIVITY_TIME_STAMP_V

  A list of date. One for each connection. This is the time of the latest
  request answered.

*ADMIN*
  .. index:: ADMIN

  URI to the administrative page.

*CASE_SENSITIVE_PARAMETERS*
  .. index:: CASE_SENSITIVE_PARAMETERS

  see :ref:`Configuration_options`.

*CHECK_URL_VALIDITY*
  .. index:: CHECK_URL_VALIDITY

  see :ref:`Configuration_options`.

*CLEANER_CLIENT_DATA_TIMEOUT*
  .. index:: CLEANER_CLIENT_DATA_TIMEOUT

  see :ref:`Configuration_options`.

*CLEANER_CLIENT_HEADER_TIMEOUT*
  .. index:: CLEANER_CLIENT_HEADER_TIMEOUT

  see :ref:`Configuration_options`.

*CLEANER_SERVER_RESPONSE_TIMEOUT*
  .. index:: CLEANER_SERVER_RESPONSE_TIMEOUT

  see :ref:`Configuration_options`.

*CLEANER_WAIT_FOR_CLIENT_TIMEOUT*
  .. index:: CLEANER_WAIT_FOR_CLIENT_TIMEOUT

  see :ref:`Configuration_options`.

*CURRENT_CONNECTIONS*
  .. index:: CURRENT_CONNECTIONS

  Number of current connections to the server.

*ERROR_LOG (boolean tag)*
  .. index:: ERROR_LOG

  This is set to true if error logging is active.

*ERROR_LOG_FILE*
  .. index:: ERROR_LOG_FILE

  The error log file full pathname.

*ERROR_LOG_FILENAME_PREFIX*
  .. index:: ERROR_LOG_FILENAME_PREFIX

  see :ref:`Configuration_options`.

*ERROR_LOG_SPLIT_MODE*
  .. index:: ERROR_LOG_SPLIT_MODE

  see :ref:`Configuration_options`.

*FORCE_CLIENT_DATA_TIMEOUT*
  .. index:: FORCE_CLIENT_DATA_TIMEOUT

  see :ref:`Configuration_options`.

*FORCE_CLIENT_HEADER_TIMEOUT*
  .. index:: FORCE_CLIENT_HEADER_TIMEOUT

  see :ref:`Configuration_options`.

*FORCE_SERVER_RESPONSE_TIMEOUT*
  .. index:: FORCE_SERVER_RESPONSE_TIMEOUT

  see :ref:`Configuration_options`.

*FORCE_WAIT_FOR_CLIENT_TIMEOUT*
  .. index:: FORCE_WAIT_FOR_CLIENT_TIMEOUT

  see :ref:`Configuration_options`.

*FREE_SLOTS_KEEP_ALIVE_LIMIT*
  .. index:: FREE_SLOTS_KEEP_ALIVE_LIMIT

  see :ref:`Configuration_options`.

*LINE_STACK_SIZE*
  .. index:: LINE_STACK_SIZE

  see :ref:`Configuration_options`.

*KEYS_M (matrix tag)*
  .. index:: KEYS_M

  A list of set of keys (for each key correspond a value in the tag
  VALUES_L, see below). Each key in the vector tag start with an `HTML`
  "<td>" tag. This is to be able to display the key/value in column.

*LOG (boolean tag)*
  .. index:: LOG

  This is set to true if logging is active.

*LOG_FILE*
  .. index:: LOG_FILE

  The log file full pathname.

*LOG_FILENAME_PREFIX*
  .. index:: LOG_FILENAME_PREFIX

  see :ref:`Configuration_options`.

*LOG_FILE_DIRECTORY*
  .. index:: LOG_FILE_DIRECTORY

  see :ref:`Configuration_options`.

*LOG_MODE*
  .. index:: LOG_MODE

  The rotating log mode, this is either `NONE`, `DAILY`,
  `MONTHLY` or `EACH_RUN`.

*LOGO*
  .. index:: LOGO

  A string to be placed in an img `HTML` tag. This is the name of
  the `AWS` logo image.

*MAX_CONCURRENT_DOWNLOAD*
  .. index:: MAX_CONCURRENT_DOWNLOAD

  see :ref:`Configuration_options`.

*MAX_CONNECTION*
  .. index:: MAX_CONNECTION

  see :ref:`Configuration_options`.

*PEER_NAME_V (vector tag)*
  .. index:: PEER_NAME_V

  A list of peer name. One for each connection. This is the name of the last
  peer connected to the slot.

*PHASE_V (vector tag)*
  .. index:: PHASE_V

  What is the slot currently doing, for example Server_Processing or Closed.

*RECEIVE_TIMEOUT*
  .. index:: RECEIVE_TIMEOUT

  see :ref:`Configuration_options`.

*REUSE_ADDRESS*
  .. index:: REUSE_ADDRESS

  see :ref:`Configuration_options`.

*SECURITY*
  .. index:: SECURITY

  A boolean set to True if this is a secure socket (HTTPS/SSL).

*SECURITY_MODE*
  .. index:: SECURITY_MODE

  see :ref:`Configuration_options`.

*CIPHER_PRIORITIES*
  .. index:: CIPHER_PRIORITIES

  see :ref:`Configuration_options`.

*SEND_TIMEOUT*
  .. index:: SEND_TIMEOUT

  see :ref:`Configuration_options`.

*SERVER_HOST*
  .. index:: SERVER_HOST

  see :ref:`Configuration_options`.

*SERVER_NAME*
  .. index:: SERVER_NAME

  see :ref:`Configuration_options`.

*SERVER_PORT*
  .. index:: SERVER_PORT

  see :ref:`Configuration_options`.

*SERVER_SOCK*
  .. index:: SERVER_SOCK

  Server socket ID.

*SESSION*
  .. index:: SESSION

  see :ref:`Configuration_options`.

*SESSION_CLEANUP_INTERVAL*
  .. index:: SESSION_CLEANUP_INTERVAL

  Number of seconds between each run of the session cleanup task. This
  task will remove all session data that have been obsoleted.

*SESSION_LIFETIME*
  .. index:: SESSION_LIFETIME

  Number of seconds to keep session information. After this period a
  session is obsoleted and will be removed at next cleanup.

*SESSION_NAME*
  .. index:: SESSION_NAME

  see :ref:`Configuration_options`.

*SESSIONS_TERMINATE_V (vector tag)*
  .. index:: SESSIONS_TERMINATE_V

  A list of time. Each item correspond to the time when the session will
  be obsoleted.

*SESSIONS_TS_V (vector tag)*
  .. index:: SESSIONS_TS_V

  A list of time stamp. Each item correspond to a session last access time.

*SESSIONS_V (vector tag)*
  .. index:: SESSIONS_V

  A list of session ID.

*SLOT_ACTIVITY_COUNTER_V (vector tag)*
  .. index:: SLOT_ACTIVITY_COUNTER_V

  A list of natural. One for each connection. This is the total number of
  requests the slot has answered. This counter is never reseted.

*SOCK_V (vector tag)*
  .. index:: SOCK_V

  A list of sockets ID. One for each connection.

*STATUS_PAGE*
  .. index:: STATUS_PAGE

  see :ref:`Configuration_options`.

*START_TIME*
  .. index:: START_TIME

  A timestamp in YYYY-MM-DD HH:MM:SS format. When the server was started.

*TRANSIENT_CLEANUP_INTERVAL*
  .. index:: TRANSIENT_CLEANUP_INTERVAL

  see :ref:`Configuration_options`.

*TRANSIENT_LIFETIME*
  .. index:: TRANSIENT_LIFETIME

  see :ref:`Configuration_options`.

*UPLOAD_DIRECTORY*
  .. index:: UPLOAD_DIRECTORY

  see :ref:`Configuration_options`.

*UPLOAD_SIZE_LIMIT*
  .. index:: UPLOAD_SIZE_LIMIT

  see :ref:`Configuration_options`.

*VALUES_M (matrix tag)*
  .. index:: VALUES_M

  A list of set of values (for each value correspond a key in the vector tag
  KEYS_L, see above). Each key in the vector tag start with an `HTML`
  "<td>" tag. This is to be able to display the key/value in column.

*VERSION*
  .. index:: VERSION

  `AWS` version string.

*WWW_ROOT*
  .. index:: WWW_ROOT

  see :ref:`Configuration_options`.

There is also all `Templates_Parser` specific tags. This is not listed
here please have a look at the `Templates_Parser` documentation
distributed with `AWS`.
