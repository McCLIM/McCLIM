(in-package #:slim)

#| Major issue: There is a proposal on the table to unify the sheet and output
record protocols, not by unifying the class structure, but by making them
implement the same generic functions where that makes sense. For instance,
sheets and output records both have regions, transformations (that relate sheets
to their parents), both support a repainting operation, and so forth.

In particular, sheet-parent and output-record-parent are equivalent, as are
sheet-children and output-record-children, sheet-adopt-child and
add-output-record, sheet-disown-child and delete-output-record, and
repaint-sheet and replay-output-record, and the mapping
functions. output-record-position and its setf function have sheet analogs. The
sheet and output record notification functions are also equivalent.

This simplifies the conceptual framework of CLIM, and could eventually simplify
the implementation as well. Doing this work now opens the door for later
unifications, such unifying the pane layout functionality with table
formatting. --- York, SWM |#

(defgeneric parent (object)
  (:method ((sheet clim:sheet))
    (clim:sheet-parent sheet))
  (:method ((output-record clim:output-record))
    (clim:output-record-parent output-record)))

(defgeneric children (object)
  (:method ((sheet clim:sheet))
    (clim:sheet-children sheet))
  (:method ((output-record clim:output-record))
    (clim:output-record-children output-record)))

(defgeneric add-child (object child)
  (:method ((sheet clim:sheet) child)
    (clim:sheet-adopt-child sheet child))
  (:method ((output-record clim:output-record) child)
    (clim:add-output-record child output-record)))

(defgeneric delete-child (object child &optional errorp)
  (:method ((sheet clim:sheet) child &optional errorp)
    (clim:sheet-disown-child sheet child :errorp errorp))
  (:method ((output-record clim:output-record) child  &optional errorp)
    (clim:delete-output-record child output-record errorp)))

(defgeneric repaint (object target region)
  (:method ((sheet clim:sheet) medium region)
    (clim:repaint-sheet sheet region))
  (:method ((output-record clim:output-record) sheet region)
    (clim:replay-output-record output-record sheet region)))
