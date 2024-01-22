## 5.1.0

* BREAKING CHANGE: Use String for Authority
  [#105](https://github.com/kazu-yamamoto/http2/pull/105)
* Properly close streams
  [#104](https://github.com/kazu-yamamoto/http2/pull/104)

## 5.0.1

* Allowing bytestring 0.12.

## 5.0.0

* Using the network-control package.
* The limits of resources can be specified in ServerConfig and ClientConfig.
* Open streams based on peer's MaxStreams.
* Rejecting Data if it is over the receiving limit.
* Informing MaxStreams properly.
* Informing WindowUpdate properly.
* New API: Server.Internal.runIO and Client.Internal.runIO.

## 4.2.2

* Adding rate limit for RST_STREAM to work around CVE-2023-44487.
  [#94](https://github.com/kazu-yamamoto/http2/pull/94)

## 4.2.1

* This version is identical to v4.2.0 by accident.

## 4.2.0

* Treating HALF_CLOSED_LOCAL correctly.
  [#90](https://github.com/kazu-yamamoto/http2/pull/90)
* Ensuring that GOAWAY is sent after DATA in the client side.
  [#89](https://github.com/kazu-yamamoto/http2/pull/90)
* Test uses a random port instead of 8080.
* Breaking change: adding two optional `SockAddr`s to `Config` to be copied into `Aux`.
* Close all streams on termination.
  [#83](https://github.com/kazu-yamamoto/http2/pull/83)
* Introducing `OutBodyStreamingUnmask`
  [#80](https://github.com/kazu-yamamoto/http2/pull/80)
* Introducing `KilledByHttp2ThreadManager` instead of `ThreadKilled`.
  [#79](https://github.com/kazu-yamamoto/http2/pull/79)
  [#81](https://github.com/kazu-yamamoto/http2/pull/82)
  [#82](https://github.com/kazu-yamamoto/http2/pull/82)
* Handle RST_STREAM with NO_ERROR.
  [#78](https://github.com/kazu-yamamoto/http2/pull/78)
* Internal changes:
  [#74](https://github.com/kazu-yamamoto/http2/pull/74)
* Breaking change: `Client` is generalized into `(forall b. Request -> (Response -> IO b) -> IO b) -> IO a`. The `RankNTypes` language extension is required.
  [#72](https://github.com/kazu-yamamoto/http2/pull/72)

## 4.1.3

* Using crypton instead of cryptonite.

## 4.1.2

* Removing the race of frameSender and frameReceiver in the server side.
  This fixes the loss of RST_Stream and TLS bad MAC error.
  [#67](https://github.com/kazu-yamamoto/http2/pull/67)

## 4.1.1

* Fixing memory-blow-up due to no flow control.
  [#62](https://github.com/kazu-yamamoto/http2/issues/62)
  [#66](https://github.com/kazu-yamamoto/http2/issues/66)

## 4.1.0

* Implementing streaming from the client side.
  [#41](https://github.com/kazu-yamamoto/http2/pull/41)
* Making use of SettingsMaxFrameSize
  [#44](https://github.com/kazu-yamamoto/http2/pull/44)
  [#57](https://github.com/kazu-yamamoto/http2/pull/57)
* Disabling flow control
  [#55](https://github.com/kazu-yamamoto/http2/pull/55)
* Fixing buffer overrun by trailers
  [#52](https://github.com/kazu-yamamoto/http2/pull/52)
* Proper use of settings
* Breaking change: the data structure of `Next` was changed.
  The `http3` package is influenced.

## 4.0.0

* Breaking change: `HTTP2Error` is redefined.
* Breaking change: `FrameTypeId`, `SettingsKeyId` and `ErrorCodeId` are removed.
  Use `FrameType`, `SettingsKey` and `ErrorCode` instead.
* A client can receive a concrete `HTTP2Error`.
* Catching up RFC 9113. Host: and :authority cannot disagree.
* Breaking change: `Network.HTTP2` and `Network.HTTP2.Priority` are removed.
* Breaking change: obsoleted stuff are removed.

## 3.0.3

* Return correct status messages in HTTP2 client
  [#31](https://github.com/kazu-yamamoto/http2/pull/31)
* Follow changes in Aeson 2
  [#32](https://github.com/kazu-yamamoto/http2/pull/32)
* Make sure connection preface is always sent first
  [#33](https://github.com/kazu-yamamoto/http2/pull/33)
* Avoid empty data
  [#34](https://github.com/kazu-yamamoto/http2/pull/34)

## 3.0.2

* Skip inserting entries that do not fit in the encoding table
  [#28](https://github.com/kazu-yamamoto/http2/pull/28)

## 3.0.1

* Including a necessary file for testing.

## 3.0.0

* DOS preventions.
* Providing Network.HTTP.Client.
* `Internal` modules are exported.
* Dropping the priority feature from Network.HTTP.Server.
* `Network.HTTP2.Priority` is deprecated.
* `Network.HTTP2` module is deprecated. Use `Network.HTTP2.Frame` instead.
* Adding some tokens.

## 2.0.6

* Dropping support of GHC 7.x

## 2.0.5

* Passing the correct request

## 2.0.4

* Freeing dynamic tables.

## 2.0.3

* Using shutdown instead of close in the example. This is important to
  send GOAWAY properly.

## 2.0.2

* Bug fix of flush limit.

## 2.0.1

* Bug fix for defaultReadN.
* Providing allocSimpleConfig and freeSimpleConfig.
* Deprecating makeSimpleConfig.

## 2.0.0

* Providing Network.HTTP.Server.

## 1.6.5

* Deny shrink of dynamic table to zero size
  [#17](https://github.com/kazu-yamamoto/http2/pull/17)

## 1.6.4

* checkFrameHeader for FrameHeaders.
  [#15](https://github.com/kazu-yamamoto/http2/pull/15)

## 1.6.3

* Fixing two bugs of HPACK pointed out by h2spec v2.

## 1.6.2

* Improving the performance of HPACK.
* Huffman encoding is now based on H2O's one.

## 1.6.1

* Improving the performance of HPACK.

## 1.6.0

* Reverse indices of HPACK are now based on tokens.
* New APIs: encodeTokenHeader and decodeTokenHeader.
* Deleted API: encodeHeaderBuffer -- use encodeTokenHeader instead.
* New module: Network.HPACK.Token

## 1.5.4

* Fixing a bug due to misuse of memcpy(). (#8)

## 1.5.3

* Adding debug information.

## 1.5.2

* Minor optimization for HPACK.

## 1.5.1

* Adding a missing file for testing.

## 1.5.0

* New API for HPACK. HPACK is much faster than 1.4.x (roughly x3.2).
  The default encoding is now Linear instead of LinearH.

## 1.4.5

* Removing zero reset from priority queues.

## 1.4.4

* Fixing a bug of reverse index.

## 1.4.3

* Priority benchmark is now external information versions.
* Using proper baseDeficit for deletion.

## 1.4.2

* Test files are now self-contained.

## 1.4.1

* The reverse indices for static and dynamic are combined for performance.

## 1.4.0

* Providing dequeueSTM, isEmpty and isEmptySTM. Users can compose
  their own control queue with dequeueSTM and isEmptySTM.

* Removing enqueueControl: it appeared that PriorityTree is not
  suitable for control frames. 　For example, the dependency of all
  control frames is stream 0.  So, PSQ does not contain multiple
  control frames at the same time.  We removed enqueueControl. Users
  should prepare a queue for control frames by themselves.

## 1.3.1

* Defining IllegalTableSizeUpdate.

## 1.3.0

* APIs `Network.HTTP2.Priority` are changed again. `Precedence` is introduced.

## 1.2.0

* APIs of `Network.HTTP2.Priority` are changed. `delete` is provided. Internal data structure is changed from random skew heap to priority search queue.
