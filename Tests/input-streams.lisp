(cl:in-package #:clim-tests)

(def-suite* :mcclim.input-streams
  :in :mcclim)

(test input-streams.smoke-test
  (let ((lame-event (make-instance 'pointer-event :sheet nil))
        (sis (make-instance 'standard-input-stream))
        (seis (make-instance 'standard-extended-input-stream)))
    (is (null (climi::stream-gesture-available-p sis)))
    (is (null (climi::stream-gesture-available-p seis)))
    ;;
    (finishes (climi::stream-append-gesture sis #\d))
    (signals error (climi::stream-append-gesture sis lame-event))
    (finishes (climi::stream-append-gesture seis #\d))
    (finishes (climi::stream-append-gesture seis lame-event))
    ;;
    (is (climi::stream-gesture-available-p sis))
    (is (climi::stream-gesture-available-p seis))
    ;;
    (is (char= #\d (stream-read-char sis)))
    (is (eql #\d (stream-read-gesture seis)))
    (is (eql lame-event (stream-read-gesture seis :peek-p t)))
    (is (eql lame-event (stream-read-gesture seis)))
    ;;
    (is (null (stream-read-gesture sis :timeout 0)))
    (is (null (stream-read-gesture seis :timeout 0)))
    ;;
    (is (null (climi::stream-gesture-available-p sis)))
    (is (null (climi::stream-gesture-available-p seis)))))
