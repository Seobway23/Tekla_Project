(defun find-duplicates (lines / hash-table duplicates)
  (setq hash-table (vlax-create-object "Scripting.Dictionary"))
  (setq duplicates '())
  (foreach line lines
    (if (vlax-invoke hash-table 'Exists line)
      (progn
        (if (= (vlax-invoke hash-table 'Item line) 1)
          (setq duplicates (cons line duplicates)))
        (vlax-invoke hash-table 'Item line (1+ (vlax-invoke hash-table 'Item line))))
      (vlax-invoke hash-table 'Add line 1)))
  (vlax-release-object hash-table)
  (reverse duplicates))

(defun remove-duplicates-from-list (lines / result hash-table)
  (setq hash-table (vlax-create-object "Scripting.Dictionary"))
  (setq result '())
  (foreach line lines
    (if (not (vlax-invoke hash-table 'Exists line))
      (progn
        (vlax-invoke hash-table 'Add line t)
        (setq result (cons line result)))))
  (vlax-release-object hash-table)
  (reverse result))

(defun process-lines (lines)
  (setq duplicates (find-duplicates lines))
  (if (null duplicates)
    (progn
      (princ "\nNo duplicate lines found.")
      lines)
    (progn
      (princ "\nDuplicate lines found: ")
      (princ duplicates)
      (setq result (remove-duplicates-from-list lines))
      (princ "\nLines after removing duplicates: ")
      (princ result)
      result)))

;; Example usage:
(setq lines '("line1" "line2" "line3" "line2" "line4" "line3" "line5"))
(process-lines lines)
(princ)
