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
