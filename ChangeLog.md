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
