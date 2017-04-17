# Simplified Lisp Interface Manager

This abstraction layer is provided to reduce cognitive and manual
effort of writing graphical user interface in CLIM at expense of using
abbreviations, depending on context and reducing level of control
accessible to the programmer.

## Usage

## API

### Setting context

`with-pane (pane) &body body`

To set context programmer has to point the stream pane which he
operates on. `with-pane` binds `slim:*pane*` and `*standard-output*`
to the supplied `pane` argument.

```common-lisp
(slim:with-pane (pane)
  (format slim:*pane* "hi1")
  (format t "hi2"))
```

### Tables

`with-table (&optional pane &rest options) &body body`

Creates context for printing output in a table. If `pane` is not
supplied, `slim:*pane*` has to be bound to the destination stream
pane. `options` are the same as for `clim:formatting-table`.

Output is organized by rows and columns with the following macros:

- `row &body body`
- `cell &body body`

Example:

```common-lisp
(defun display-pane (frame pane)
  (declare (ignore frame))
  (slim:with-pane (pane)
    (slim:with-table ()
      (slim:row
        (slim:cell (princ "Description:"))
        (slim:cell (princ "foo")))
      (slim:row
        (slim:cell (princ "Information:"))
        (slim:cell (slim:with-opts (clim:+red+ :bold :fixed)
                     (princ "bar"))))
      (when (= 3 3)
        (slim:row
          (slim:cell (princ "Extra:"))
          (slim:cell (format t "~A" 'quux)))))))
```
