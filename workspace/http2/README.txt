# Some notes about usage / implementation

This simple demo is created to quickly build and test the HTTP/2
implementation. It uses the AWS build tree to create the binary. No
need to install.

To build the demo (no secure, upgrade from HTTP/1):

   $ make h2c

or (for secure connection):

   $ make h2


Then send a request to server (non secure):

[1]
  $ curl --http2 http://localhost:1234/toto

  => note that Chrome or Firefox are implementing http2 only
     over secure TLS connection.

  => with curl we can start testing more easily the implementation
     of HTTP/2 with upgrade protocol.

 [2]
   $ curl --verbose --trace - --http2-prior-knowledge \
     -o out http://localhost:1234/toto

Or secure:

[3]
  $ curl --http2 --cacert cert.pem https://localhost:1234/toto

  => will be dealt with later

[4]
  $ curl --trace - --http2-prior-knowledge -o out --data body \
    http://localhost:1234/toto

(a POST request)

-----------------------
[1] non secure requests

In this mode the headers sent are (displayed by srv CB routine):

 1 Host = localhost:1234
 2 User-Agent = curl/7.74.0
 3 Accept = */*
 4 Connection = Upgrade, HTTP2-Settings
 5 Upgrade = h2c
 6 HTTP2-Settings = AAMAAABkAAQCAAAAAAIAAAAA


-----------------------
[2] HTTP/2 - no upgrade as prior knowledge that the server supports it

-----------------------
[3] secure requests

Start srv with port as parameter:

   $ make h2

And then enter into the browser:

   https://localhost:1234/sometext


----------------------------------
Running the testsuite using h2spec

It requires a Go compiler. On GNU/Linux Debian this is provided by the
golang package:

$ sudo apt install golang

$ git clone https://github.com/summerwind/h2spec.git
$ cd h2spec
$ make

A binary named h2spec should be compiled on top directory.

On AWS (branch prj/http2):

$ cd workspace/http2
$ make && ./srv 1234

Then on another console run the testsuite:

$ cd h2spec
$ ./h2spec --host 127.0.0.1 --port 1234 --insecure --tls

------------------------
TO DO:

- Client support
- Stream priority (default priority)
- Send part of data : content ranges support (aws-http2-message.adb)
- Makes all HTTP2 settings standard conf parameters compatible with aws.ini

------------------------
Test h2spec (only support H2 protocol not H2C).

Start aws server with secure mode:

   $ ./srv 1234

   $ ./h2spec --host 127.0.0.1 --port 1234 --insecure --tls

Currently Passing
-----------------

Generic tests for HTTP/2 server
  1. Starting HTTP/2
    ✔ 1: Sends a client connection preface

  2. Streams and Multiplexing
    ✔ 1: Sends a PRIORITY frame on idle stream
    ✔ 2: Sends a WINDOW_UPDATE frame on half-closed (remote) stream
    ✔ 3: Sends a PRIORITY frame on half-closed (remote) stream
    ✔ 4: Sends a RST_STREAM frame on half-closed (remote) stream
    ✔ 5: Sends a PRIORITY frame on closed stream

  3. Frame Definitions
    3.1. DATA
      ✔ 1: Sends a DATA frame
      ✔ 2: Sends multiple DATA frames
      ✔ 3: Sends a DATA frame with padding

    3.2. HEADERS
      ✔ 1: Sends a HEADERS frame
      ✔ 2: Sends a HEADERS frame with padding
      ✔ 3: Sends a HEADERS frame with priority

    3.3. PRIORITY
      ✔ 1: Sends a PRIORITY frame with priority 1
      ✔ 2: Sends a PRIORITY frame with priority 256
      ✔ 3: Sends a PRIORITY frame with stream dependency
      ✔ 4: Sends a PRIORITY frame with exclusive
        5: Sends a PRIORITY frame for an idle stream, then send a HEADER frame f      ✔ 5: Sends a PRIORITY frame for an idle stream, then send a HEADER frame for a lower stream ID

    3.4. RST_STREAM
      ✔ 1: Sends a RST_STREAM frame

    3.5. SETTINGS
      ✔ 1: Sends a SETTINGS frame

    3.7. PING
      ✔ 1: Sends a PING frame

    3.8. GOAWAY
     ✔ 1: Sends a GOAWAY frame

    3.9. WINDOW_UPDATE
      ✔ 1: Sends a WINDOW_UPDATE frame with stream ID 0
      ✔ 2: Sends a WINDOW_UPDATE frame with stream ID 1

    3.10. CONTINUATION
      ✔ 1: Sends a CONTINUATION frame
      ✔ 2: Sends multiple CONTINUATION frames

  4. HTTP Message Exchanges
    ✔ 1: Sends a GET request
    ✔ 2: Sends a HEAD request
    ✔ 3: Sends a POST request
    ✔ 4: Sends a POST request with trailers

  5. HPACK
    ✔ 1: Sends a indexed header field representation
    ✔ 2: Sends a literal header field with incremental indexing - indexed name
      3: Sends a literal header field with incremental indexing - indexed name (    ✔ 3: Sends a literal header field with incremental indexing - indexed name (with Huffman coding)
    ✔ 4: Sends a literal header field with incremental indexing - new name
      5: Sends a literal header field with incremental indexing - new name (with    ✔ 5: Sends a literal header field with incremental indexing - new name (with Huffman coding)
    ✔ 6: Sends a literal header field without indexing - indexed name
      7: Sends a literal header field without indexing - indexed name (with Huff    ✔ 7: Sends a literal header field without indexing - indexed name (with Huffman coding)
    ✔ 8: Sends a literal header field without indexing - new name
      9: Sends a literal header field without indexing - new name (huffman encod    ✔ 9: Sends a literal header field without indexing - new name (huffman encoded)
    ✔ 10: Sends a literal header field never indexed - indexed name
      11: Sends a literal header field never indexed - indexed name (huffman enc    ✔ 11: Sends a literal header field never indexed - indexed name (huffman encoded)
    ✔ 12: Sends a literal header field never indexed - new name
      13: Sends a literal header field never indexed - new name (huffman encoded    ✔ 13: Sends a literal header field never indexed - new name (huffman encoded)
    ✔ 14: Sends a dynamic table size update
    ✔ 15: Sends multiple dynamic table size update

Hypertext Transfer Protocol Version 2 (HTTP/2)
  3. Starting HTTP/2
    3.5. HTTP/2 Connection Preface
      ✔ 1: Sends client connection preface
      ✔ 2: Sends invalid connection preface

  4. HTTP Frames
    4.1. Frame Format
      ✔ 1: Sends a frame with unknown type
      ✔ 2: Sends a frame with undefined flag
      ✔ 3: Sends a frame with reserved field bit

    4.2. Frame Size
      ✔ 1: Sends a DATA frame with 2^14 octets in length
        2: Sends a large size DATA frame that exceeds the SETTINGS_MAX_FRAME_SIZ      ✔ 2: Sends a large size DATA frame that exceeds the SETTINGS_MAX_FRAME_SIZE
        3: Sends a large size HEADERS frame that exceeds the SETTINGS_MAX_FRAME_      ✔ 3: Sends a large size HEADERS frame that exceeds the SETTINGS_MAX_FRAME_SIZE

    4.3. Header Compression and Decompression
      ✔ 1: Sends invalid header block fragment
      ✔ 2: Sends a PRIORITY frame while sending the header blocks
        3: Sends a HEADERS frame to another stream while sending the header bloc      ✔ 3: Sends a HEADERS frame to another stream while sending the header blocks

  5. Streams and Multiplexing
    5.1. Stream States
      ✔ 1: idle: Sends a DATA frame
      ✔ 2: idle: Sends a RST_STREAM frame
      ✔ 3: idle: Sends a WINDOW_UPDATE frame
      ✔ 4: idle: Sends a CONTINUATION frame
      ✔ 5: half closed (remote): Sends a DATA frame
      ✔ 6: half closed (remote): Sends a HEADERS frame
      ✔ 7: half closed (remote): Sends a CONTINUATION frame
      ✔ 8: closed: Sends a DATA frame after sending RST_STREAM frame
      ✔ 9: closed: Sends a HEADERS frame after sending RST_STREAM frame
      ✔ 10: closed: Sends a CONTINUATION frame after sending RST_STREAM frame
      ✔ 11: closed: Sends a DATA frame
      ✔ 12: closed: Sends a HEADERS frame
      ✔ 13: closed: Sends a CONTINUATION frame

      5.1.1. Stream Identifiers
        ✔ 1: Sends even-numbered stream identifier
        ✔ 2: Sends stream identifier that is numerically smaller than previous

      5.1.2. Stream Concurrency
          1: Sends HEADERS frames that causes their advertised concurrent stream        1: Sends HEADERS frames that causes their advertised concurrent stream limit to be exceeded

    5.3. Stream Priority
      5.3.1. Stream Dependencies
        ✔ 1: Sends HEADERS frame that depends on itself
        ✔ 2: Sends PRIORITY frame that depend on itself

    5.4. Error Handling
      5.4.1. Connection Error Handling
        ✔ 1: Sends an invalid PING frame for connection close

    5.5. Extending HTTP/2
      ✔ 1: Sends an unknown extension frame
      ✔ 2: Sends an unknown extension frame in the middle of a header block

  6. Frame Definitions
    6.1. DATA
      ✔ 1: Sends a DATA frame with 0x0 stream identifier
        2: Sends a DATA frame on the stream that is not in "open" or "half-close      ✔ 2: Sends a DATA frame on the stream that is not in "open" or "half-closed (local)" state
      ✔ 3: Sends a DATA frame with invalid pad length

    6.2. HEADERS
        1: Sends a HEADERS frame without the END_HEADERS flag, and a PRIORITY fr      ✔ 1: Sends a HEADERS frame without the END_HEADERS flag, and a PRIORITY frame
      ✔ 2: Sends a HEADERS frame to another stream while sending a HEADERS frame
      ✔ 3: Sends a HEADERS frame with 0x0 stream identifier
      ✔ 4: Sends a HEADERS frame with invalid pad length

    6.3. PRIORITY
      ✔ 1: Sends a PRIORITY frame with 0x0 stream identifier
      ✔ 2: Sends a PRIORITY frame with a length other than 5 octets

    6.4. RST_STREAM
      ✔ 1: Sends a RST_STREAM frame with 0x0 stream identifier
      ✔ 2: Sends a RST_STREAM frame on a idle stream
      ✔ 3: Sends a RST_STREAM frame with a length other than 4 octets

    6.5. SETTINGS
      ✔ 1: Sends a SETTINGS frame with ACK flag and payload
      ✔ 2: Sends a SETTINGS frame with a stream identifier other than 0x0
        3: Sends a SETTINGS frame with a length other than a multiple of 6 octet      ✔ 3: Sends a SETTINGS frame with a length other than a multiple of 6 octets

      6.5.2. Defined SETTINGS Parameters
        ✔ 1: SETTINGS_ENABLE_PUSH (0x2): Sends the value other than 0 or 1
          2: SETTINGS_INITIAL_WINDOW_SIZE (0x4): Sends the value above the maxim        ✔ 2: SETTINGS_INITIAL_WINDOW_SIZE (0x4): Sends the value above the maximum flow control window size
          3: SETTINGS_MAX_FRAME_SIZE (0x5): Sends the value below the initial va        ✔ 3: SETTINGS_MAX_FRAME_SIZE (0x5): Sends the value below the initial value
          4: SETTINGS_MAX_FRAME_SIZE (0x5): Sends the value above the maximum al        ✔ 4: SETTINGS_MAX_FRAME_SIZE (0x5): Sends the value above the maximum allowed frame size
        ✔ 5: Sends a SETTINGS frame with unknown identifier

      6.5.3. Settings Synchronization
        ✔ 1: Sends multiple values of SETTINGS_INITIAL_WINDOW_SIZE
        ✔ 2: Sends a SETTINGS frame without ACK flag

    6.7. PING
      ✔ 1: Sends a PING frame
      ✔ 2: Sends a PING frame with ACK
        3: Sends a PING frame with a stream identifier field value other than 0x      ✔ 3: Sends a PING frame with a stream identifier field value other than 0x0
      ✔ 4: Sends a PING frame with a length field value other than 8

    6.8. GOAWAY
      ✔ 1: Sends a GOAWAY frame with a stream identifier other than 0x0

    6.9. WINDOW_UPDATE
      ✔ 1: Sends a WINDOW_UPDATE frame with a flow control window increment of 0
        2: Sends a WINDOW_UPDATE frame with a flow control window increment of 0      ✔ 2: Sends a WINDOW_UPDATE frame with a flow control window increment of 0 on a stream
      ✔ 3: Sends a WINDOW_UPDATE frame with a length other than 4 octets

      6.9.1. The Flow-Control Window
          1: Sends SETTINGS frame to set the initial window size to 1 and sends         ✔ 1: Sends SETTINGS frame to set the initial window size to 1 and sends HEADERS frame
          2: Sends multiple WINDOW_UPDATE frames increasing the flow control win        ✔ 2: Sends multiple WINDOW_UPDATE frames increasing the flow control window to above 2^31-1
          3: Sends multiple WINDOW_UPDATE frames increasing the flow control win        ✔ 3: Sends multiple WINDOW_UPDATE frames increasing the flow control window to above 2^31-1 on a stream

      6.9.2. Initial Flow-Control Window Size
        ✔ 1: Changes SETTINGS_INITIAL_WINDOW_SIZE after sending HEADERS frame
        ✔ 2: Sends a SETTINGS frame for window size to be negative
          3: Sends a SETTINGS_INITIAL_WINDOW_SIZE settings with an exceeded maxi        ✔ 3: Sends a SETTINGS_INITIAL_WINDOW_SIZE settings with an exceeded maximum window size value

    6.10. CONTINUATION
      ✔ 1: Sends multiple CONTINUATION frames preceded by a HEADERS frame
        2: Sends a CONTINUATION frame followed by any frame other than CONTINUAT      ✔ 2: Sends a CONTINUATION frame followed by any frame other than CONTINUATION
      ✔ 3: Sends a CONTINUATION frame with 0x0 stream identifier
        4: Sends a CONTINUATION frame preceded by a HEADERS frame with END_HEADE      ✔ 4: Sends a CONTINUATION frame preceded by a HEADERS frame with END_HEADERS flag
        5: Sends a CONTINUATION frame preceded by a CONTINUATION frame with END_      ✔ 5: Sends a CONTINUATION frame preceded by a CONTINUATION frame with END_HEADERS flag
      ✔ 6: Sends a CONTINUATION frame preceded by a DATA frame

  7. Error Codes
    ✔ 1: Sends a GOAWAY frame with unknown error code
    ✔ 2: Sends a RST_STREAM frame with unknown error code

  8. HTTP Message Exchanges
    8.1. HTTP Request/Response Exchange
      ✔ 1: Sends a second HEADERS frame without the END_STREAM flag

      8.1.2. HTTP Header Fields
          1: Sends a HEADERS frame that contains the header field name in upperc        ✔ 1: Sends a HEADERS frame that contains the header field name in uppercase letters

        8.1.2.1. Pseudo-Header Fields
          ✔ 1: Sends a HEADERS frame that contains a unknown pseudo-header field
            2: Sends a HEADERS frame that contains the pseudo-header field defin          ✔ 2: Sends a HEADERS frame that contains the pseudo-header field defined for response
            3: Sends a HEADERS frame that contains a pseudo-header field as trai          ✔ 3: Sends a HEADERS frame that contains a pseudo-header field as trailers
            4: Sends a HEADERS frame that contains a pseudo-header field that ap          ✔ 4: Sends a HEADERS frame that contains a pseudo-header field that appears in a header block after a regular header field

        8.1.2.2. Connection-Specific Header Fields
            1: Sends a HEADERS frame that contains the connection-specific heade          ✔ 1: Sends a HEADERS frame that contains the connection-specific header field
            2: Sends a HEADERS frame that contains the TE header field with any           ✔ 2: Sends a HEADERS frame that contains the TE header field with any value other than "trailers"

        8.1.2.3. Request Pseudo-Header Fields
          ✔ 1: Sends a HEADERS frame with empty ":path" pseudo-header field
          ✔ 2: Sends a HEADERS frame that omits ":method" pseudo-header field
          ✔ 3: Sends a HEADERS frame that omits ":scheme" pseudo-header field
          ✔ 4: Sends a HEADERS frame that omits ":path" pseudo-header field
            5: Sends a HEADERS frame with duplicated ":method" pseudo-header fie          ✔ 5: Sends a HEADERS frame with duplicated ":method" pseudo-header field
            6: Sends a HEADERS frame with duplicated ":scheme" pseudo-header fie          ✔ 6: Sends a HEADERS frame with duplicated ":scheme" pseudo-header field
          ✔ 7: Sends a HEADERS frame with duplicated ":path" pseudo-header field

        8.1.2.6. Malformed Requests and Responses
            1: Sends a HEADERS frame with the "content-length" header field whic          ✔ 1: Sends a HEADERS frame with the "content-length" header field which does not equal the DATA frame payload length
            2: Sends a HEADERS frame with the "content-length" header field whic          ✔ 2: Sends a HEADERS frame with the "content-length" header field which does not equal the sum of the multiple DATA frames payload length

    8.2. Server Push
      ✔ 1: Sends a PUSH_PROMISE frame

HPACK: Header Compression for HTTP/2
  2. Compression Process Overview
    2.3. Indexing Tables
      2.3.3. Index Address Space
        ✔ 1: Sends a indexed header field representation with invalid index
        ✔ 2: Sends a literal header field representation with invalid index

  4. Dynamic Table Management
    4.2. Maximum Table Size
      ✔ 1: Sends a dynamic table size update at the end of header block

  5. Primitive Type Representations
    5.2. String Literal Representation
        1: Sends a Huffman-encoded string literal representation with padding lo      ✔ 1: Sends a Huffman-encoded string literal representation with padding longer than 7 bits
      ✔ 2: Sends a Huffman-encoded string literal representation padded by zero
        3: Sends a Huffman-encoded string literal representation containing the       ✔ 3: Sends a Huffman-encoded string literal representation containing the EOS symbol

  6. Binary Format
    6.1. Indexed Header Field Representation
      ✔ 1: Sends a indexed header field representation with index 0

    6.3. Dynamic Table Size Update
        1: Sends a dynamic table size update larger than the value of SETTINGS_H      ✔ 1: Sends a dynamic table size update larger than the value of SETTINGS_HEADER_TABLE_SIZE

Finished in 0.4219 seconds
146 tests, 145 passed, 1 skipped, 0 failed
